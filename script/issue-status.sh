#!/usr/bin/env bash
# What the orchestrator reads of a finished unit. Usage:
#   script/issue-status.sh ISSUE...
# Prints each issue's state and labels, the start of its last comment, and for
# the PR that closed it: the merge SHA, the body's `Cost:` and `Deferred`
# lines, and the changed files outside `data/`.
set -euo pipefail
[ "$#" -ge 1 ] || { echo "usage: $0 ISSUE..." >&2; exit 2; }
slug=$(gh repo view --json nameWithOwner --jq .nameWithOwner)
for n in "$@"; do
  gh issue view "$n" --json number,title,state,stateReason,labels \
    --jq '"#\(.number) \(.state) \(.stateReason) [\([.labels[].name] | join(","))] \(.title)"'
  gh api "repos/$slug/issues/$n/comments" --jq '.[-1] | select(.) | "last comment: " + (.body | .[0:600])'
  closers=$(gh api graphql -F owner="${slug%/*}" -F name="${slug#*/}" -F n="$n" -f query='
    query($owner: String!, $name: String!, $n: Int!) { repository(owner: $owner, name: $name) {
      issue(number: $n) { timelineItems(itemTypes: [CLOSED_EVENT], last: 1) {
        nodes { ... on ClosedEvent { closer { ... on PullRequest { number } } } } } } } }' \
    --jq '.data.repository.issue.timelineItems.nodes[].closer.number // empty')
  for p in $closers; do
    gh pr view "$p" --json number,title,mergeCommit,body \
      --jq '"PR #\(.number) \(.title) merge=\(.mergeCommit.oid)", (.body | split("\n")[] | select(test("^\\s*[-*]?\\s*\\**(Cost|Deferred)")))'
    gh pr view "$p" --json files --jq '.files[].path' | grep -v '^data/' | sed 's/^/  /' || true
  done
done
