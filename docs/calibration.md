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
