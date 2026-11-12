# The label routine

The prompt of the claude.ai routine "Parun label"
(`trig_013io5KgRBG8BH5o7CeWWB7g`), which starts a cloud session whenever the
`claude` label is added to an issue. The routine is the source of truth and
this file its history: change both in the same PR, the routine through the
`RemoteTrigger` tool's `update` (or the claude.ai page), and the text below
verbatim.

```text
A GitHub event says the "claude" label was added to an issue in
parun-mtg/parun. Work that one issue, autonomously, to a merged PR. Never ask a
question: nobody is watching this session.

Start:
- Note the time (date -u +%s) before anything else; the Cost line needs it.
- Follow CLAUDE.md and CONTRIBUTING.md as written. The local-machine steps
  don't apply here: no primary checkout and no warm build.
- If the issue names a set tracker or tells you to follow
  docs/agents/set-by-set.md, it is an orchestrated unit: its footer applies,
  and _scratch inputs are fetched per docs/agents/orchestrating.md, "Session
  setup", when a step needs them. Otherwise it is not an orchestrated unit,
  and missing _scratch prior art is a skipped step.
- If an open PR already addresses this issue, or it is closed, comment saying
  so and stop.
- Assign the issue to tfausak.
- Branch off a fresh origin/main, naming the branch claude/<issue-number>-<slug>.
- A slow cold build is expected. Never stop for build time; build, and if you're blocked, say what failed.

Finish:
- Do the self-review and run the tests your change can reach, not the whole
  suite: CI runs that. Read the result before doing anything else; never
  chain a test run with marking ready or enabling auto-merge.
- Once it is green, open the PR. Last thing before marking it ready, add a
  "Cost:" line to its body: the "total" row of script/session-tokens.sh over
  your own transcript (the newest ~/.claude/projects/*/*.jsonl, and its
  subagents/ directory if any), and the wall clock since you started. If no
  transcript is reachable, say that and what you looked for; never estimate.
- Mark the PR ready and enable auto-merge with gh pr merge --auto. The repo
  uses a merge queue. Never run the whole suite: if CI fails, run the cases
  it names.

Wait:
- Wait with script/wait-pr.sh <PR>, run in the foreground with a timeout
  longer than its three hours or in the background; it exits naming why the
  PR needs you (merged, closed, a merge conflict, failing checks, dropped
  from the merge queue, a new comment). Don't write your own loop and don't
  check repeatedly yourself.
- Never update a branch merely because it is behind main. Merge origin/main
  only to resolve a real conflict, then re-run the tests your change can
  reach and read the result.
- Fix failing checks. For a review comment, verify the claim against the code
  and data/comprehensive-rules.txt, then fix it or reply saying why not. After
  a fix, re-arm auto-merge if it was disarmed, then wait again.
- Stop when the PR has merged. Also stop when the script reports three hours
  of waiting, leaving a comment on the PR with its state.

If blocked:
- Comment on the issue with what you tried, what stopped you and what is needed,
  and stop. Push any useful work as a draft PR first, its body carrying the
  Cost line. Remove the "claude" label from the issue and add "needs-planning",
  so its state is visible.
```
