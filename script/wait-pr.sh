#!/usr/bin/env bash
# Wait on a pull request that has auto-merge armed. Usage:
#   script/wait-pr.sh PR [HOURS]
# Polls once a minute and exits when the PR needs its author: 0 once it has
# merged, 1 with the reason on stdout otherwise (closed, a merge conflict,
# failing checks, dropped from the merge queue, auto-merge not armed, a new
# comment or review, or HOURS elapsed, 3 by default). A queued PR shows no
# autoMergeRequest, so the queue is read from isInMergeQueue. Where GraphQL is
# refused (a cloud session's proxy answers 403) the same facts are read over
# REST, the queue from the PR's last merge-queue event. Exits 2 with the error
# after five readings in a row fail, rather than waiting out HOURS blind.
set -euo pipefail
[ "$#" -ge 1 ] || { echo "usage: $0 PR [HOURS]" >&2; exit 2; }
pr=$1
deadline=$(($(date +%s) + ${2:-3} * 3600))
# "OWNER NAME", from the remote when GraphQL (gh repo view) is refused.
repo=$(gh repo view --json owner,name --jq '.owner.login + " " + .name' 2>/dev/null) || {
  url=$(git remote get-url origin)
  url=${url%.git}
  owner=${url%/*}
  repo="${owner##*[/:]} ${url##*/}"
}
query='query($owner: String!, $name: String!, $pr: Int!) {
  repository(owner: $owner, name: $name) { pullRequest(number: $pr) {
    state mergeable isInMergeQueue
    autoMergeRequest { enabledAt }
    comments { totalCount } reviews { totalCount } reviewThreads { totalCount }
    commits(last: 1) { nodes { commit { statusCheckRollup { state } } } }
  } } }'
# The six facts over REST, in the GraphQL reading's order and spelling.
rest() {
  local slug=${repo% *}/${repo#* } pull events sha conclusions
  pull=$(gh api "repos/$slug/pulls/$pr") || return 1
  events=$(gh api "repos/$slug/issues/$pr/events?per_page=100" --jq '
    [.[] | select(.event == "added_to_merge_queue" or .event == "removed_from_merge_queue") | .event] | last // ""') || return 1
  sha=$(jq -r .head.sha <<<"$pull")
  conclusions=$(gh api "repos/$slug/commits/$sha/check-runs?per_page=100" --jq '[.check_runs[].conclusion] | join(" ")') || return 1
  jq -r --arg events "$events" --arg conclusions "$conclusions" '
    [ (if .merged then "MERGED" else (.state | ascii_upcase) end),
      (if .mergeable_state == "dirty" then "CONFLICTING" else "MERGEABLE" end),
      ($events == "added_to_merge_queue" | tostring),
      (.auto_merge != null | tostring),
      (.comments + .review_comments | tostring),
      (if ($conclusions | test("failure|timed_out|startup_failure")) then "FAILURE" else "NONE" end) ]
    | join(" ")' <<<"$pull"
}
graphql() {
  gh api graphql -f query="$query" -F owner="${repo% *}" -F name="${repo#* }" -F pr="$pr" --jq '
      .data.repository.pullRequest
      | [ .state, .mergeable, (.isInMergeQueue | tostring), (.autoMergeRequest != null | tostring),
          (.comments.totalCount + .reviews.totalCount + .reviewThreads.totalCount | tostring),
          (.commits.nodes[0].commit.statusCheckRollup.state // "NONE") ]
      | join(" ")'
}
talk0=''
queued=''
failures=0
read_pr=graphql
while :; do
  if line=$($read_pr 2>/dev/null) || { read_pr=rest; line=$(rest 2>&1); }; then
    failures=0
    read -r state mergeable inQueue armed talk checks <<<"$line"
    [ -n "$talk0" ] || talk0=$talk
    [ "$inQueue" = false ] || queued=1
    if [ "$state" = MERGED ]; then echo "merged"; exit 0; fi
    if [ "$state" = CLOSED ]; then echo "closed without merging"; exit 1; fi
    if [ "$mergeable" = CONFLICTING ]; then echo "merge conflict with main"; exit 1; fi
    if [ "$talk" != "$talk0" ]; then echo "a new comment or review"; exit 1; fi
    if [ "$inQueue" = false ]; then
      case "$checks" in FAILURE | ERROR) echo "failing checks"; exit 1 ;; esac
      if [ -n "$queued" ]; then echo "dropped from the merge queue"; exit 1; fi
      if [ "$armed" = false ]; then echo "auto-merge is not armed"; exit 1; fi
    fi
  else
    failures=$((failures + 1))
    if [ "$failures" -ge 5 ]; then echo "cannot read the pull request: $line"; exit 2; fi
  fi
  if [ "$(date +%s)" -ge "$deadline" ]; then echo "still waiting after ${2:-3} hours"; exit 1; fi
  sleep 60
done
