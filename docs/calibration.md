# Calibration

`script/calibrate.sh` checks the bots' win rates against real ones (#9346).
The bot will trail skilled humans; what it checks is whether the ordering of
matchups and the direction of each edge agree, and whether one deck's gap
leans one way across its matchups.

- Matchups: `data/calibration/pauper.tsv`, three Pauper decks
  (`data/decks/pauper/`): two aggro decks and a control deck. Real game win
  rates come from MTGO Challenge rounds since 2025-07-01, as pauperdata
  (pinned in the table's header) records them. `script/calibrate.sh real`
  recounts them.
- Known gaps: real games 2 and 3 are played after sideboarding. `parun match`
  plays main decks only. The lists are from 2026-11, and the real games come
  from the year before.

## Running it

A full run is long (1 to 3 minutes a game with both seats searching, on an
optimized build), so it belongs on a quiet machine:

```
cabal build -O1 --builddir=_scratch/dist-opt exe:parun
PARUN="env parun_datadir=data $(cabal list-bin -O1 --builddir=_scratch/dist-opt exe:parun)" \
  script/calibrate.sh run -j 3 -- --games 200 --search 1 --search 2 \
  --seed 9346000000000000000000000000000000000000000000000000000000000000
```

## Smoke run, 2026-11-22

This run played the first games of the seed above, both seats searching at the
default budget, on a shared host, and was cut short. The search bot is
deterministic per game, so a longer run with that seed replays these games
first.

| Deck | Against | Real | Simulated | Gap | Same side of 50% |
| --- | --- | --- | --- | --- | --- |
| mono-red-madness | rakdos-madness | 0.406 [0.360, 0.454] (421 games) | 0.174 [0.070, 0.371] (23 games) | -0.232 | yes |
| mono-red-madness | jeskai-ephemerate | 0.525 [0.452, 0.596] (181 games) | 0.364 [0.152, 0.646] (11 games) | -0.161 | no |
| rakdos-madness | jeskai-ephemerate | 0.643 [0.492, 0.770] (42 games) | 0.714 [0.359, 0.918] (7 games) | +0.071 | yes |

2 of 3 matchups land on the same side of 50%, and all 3 pairs of matchups are
ordered alike.

| Deck | Mean signed gap | Matchups |
| --- | --- | --- |
| mono-red-madness | -0.197 | 2 |
| rakdos-madness | +0.152 | 2 |
| jeskai-ephemerate | +0.045 | 2 |

Skew: Mono Red Madness is under-rated. Its gap against Rakdos Madness falls
outside both intervals. Games are also far too long for these decks: 21 to
32 turns on average. Both are filed as #9472. Only the first matchup has
enough games to say anything; the rest needs a full run.

## Fitting the evaluation weights

`parun fit` fits the search bot's evaluation weights (`Parun.Types.Weights`)
to how games end (#9724): a logistic regression of each position's winner on
both players' features, the positions `parun match --positions` writes at
every decision at priority. Each game weighs 1 however many positions it
holds. A ridge, chosen by five-fold cross-validation over games, holds the
weights toward `Weights.neutral`'s shape, every feature alike. Each loss it
reports is on games held out of the fit scoring them. A coefficient `b`
becomes the weight `500 b`: a cut-off playout's score stands for its expected
result, 1000 (2p - 1), which rises 500 per unit of the logit where p is a
half. With two seats every position is counted from both sides, so `ours` and
`theirs` come out alike.

A full collection, on a quiet machine:

```
for m in "mono-red-madness rakdos-madness" "mono-red-madness jeskai-ephemerate" "rakdos-madness jeskai-ephemerate"; do
  set -- $m
  $PARUN match --games 200 --search 1 --search 2 --positions _scratch/positions/$1--$2.tsv \
    data/decks/pauper/$1.txt data/decks/pauper/$2.txt &
done; wait
$PARUN fit --out data/calibration/weights.json _scratch/positions/*.tsv
```

Then measure playing strength with `--weights=1=data/calibration/weights.json`.

### Smoke run, 2026-11-26

Eight games of each matchup, both seats searching at the default budget:
11645 positions from 24 games.

| Evaluation | Log loss |
| --- | --- |
| `Weights.neutral` as it stands | 0.6862 |
| `Weights.neutral` at its best scale, held out | 0.5465 |
| Fitted, held out | 0.5894 |
| Fitted with a damage clock, held out | 0.5549 |

Cross-validation chose the largest ridge, so `data/calibration/weights.json`
is `Weights.neutral`'s shape at 68 a unit. All of the gain is scale.
`Weights.neutral`'s 1 a unit scores a cut-off playout near 0 beside a win's
1000. 24 games cannot tell one feature from another, and on them the damage
clock does not beat the six features weighted alike.
