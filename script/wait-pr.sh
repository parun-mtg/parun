#!/usr/bin/env bash
# Wait on a pull request that has auto-merge armed. Usage:
#   script/wait-pr.sh PR [HOURS]
# Polls once a minute and exits when the PR needs its author: 0 once it has
# merged, 1 with the reason on stdout otherwise (closed, a merge conflict,
# failing checks, dropped from the merge queue, auto-merge not armed, a new
# comment or review, or HOURS elapsed, 3 by default). A queued PR shows no
# autoMergeRequest, so the queue is read from isInMergeQueue.
set -euo pipefail
[ "$#" -ge 1 ] || { echo "usage: $0 PR [HOURS]" >&2; exit 2; }
pr=$1
deadline=$(($(date +%s) + ${2:-3} * 3600))
repo=$(gh repo view --json owner,name --jq '.owner.login + " " + .name')
query='query($owner: String!, $name: String!, $pr: Int!) {
  repository(owner: $owner, name: $name) { pullRequest(number: $pr) {
    state mergeable isInMergeQueue
    autoMergeRequest { enabledAt }
    comments { totalCount } reviews { totalCount } reviewThreads { totalCount }
    commits(last: 1) { nodes { commit { statusCheckRollup { state } } } }
  } } }'
talk0=''
queued=''
while :; do
  if line=$(gh api graphql -f query="$query" -F owner="${repo% *}" -F name="${repo#* }" -F pr="$pr" --jq '
      .data.repository.pullRequest
      | [ .state, .mergeable, (.isInMergeQueue | tostring), (.autoMergeRequest != null | tostring),
          (.comments.totalCount + .reviews.totalCount + .reviewThreads.totalCount | tostring),
          (.commits.nodes[0].commit.statusCheckRollup.state // "NONE") ]
      | join(" ")' 2>/dev/null); then
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
  fi
  if [ "$(date +%s)" -ge "$deadline" ]; then echo "still waiting after ${2:-3} hours"; exit 1; fi
  sleep 60
done
