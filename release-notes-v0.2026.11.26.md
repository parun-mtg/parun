Each platform's archive holds the `parun` binary and the `data` directory it reads; set the `parun_datadir` environment variable to that directory's path.

## New cards

### Sets
* **Vanguard Series (PVAN)** is finished (#9855): census (#9825) and batch 1 (#9837).
* **Magic Online Avatars (PMOA)** is finished (#9931): census (#9852) and batches 1–2 (#9860, #9866).
* Hard Avatars added on their own: Stuffy Doll (#9867), Enigma Sphinx (#9871), Sakashima the Impostor (#9872), Arcanis, the Omnipotent, Jaya Ballard and Flametongue Kavu (#9873), Kresh the Bloodbraided (#9874), Figure of Destiny (#9875), Reaper King (#9881), Momir Vig, Sliver Queen and Stonehewer Giant (#9882), Seshiro the Anointed and Ink-Eyes, Servant of Oni (#9879), Two-Headed Giant of Foriys (#9886), Karona, False God (#9887), Murderous Redcap (#9891), Sisters of Stone Death (#9894), Akroma, Angel of Wrath and Master of the Wild Hunt (#9895), Peacekeeper (#9897), Necropotence (#9900), Elvish Champion (#9903), Jhoira of the Ghitu (#9904), Dakkon Blackblade (#9905), Malfegor (#9906), Eight-and-a-Half-Tails (#9908), Maralen of the Mornsong (#9911), Maelstrom Archangel (#9917).

### Cards with new rules support
* Seshiro the Anointed and Ink-Eyes, Servant of Oni Avatars: a vanguard's ability triggers at the beginning of the game (#9879)
* Elvish Champion Avatar: a vanguard's game-start actions are taken from the command zone once the game is set up (#9903), alongside the starting life totals (#9919)
* Maralen of the Mornsong Avatar: pay any amount of life at the beginning of the game (#9911)
* Two-Headed Giant of Foriys and Sisters of Stone Death Avatars: block permissions (#9886) and block requirements (#9894) apply from the command zone; Sisters of Stone Death also filters a creature by what it blocks (#9894)
* Momir Vig, Sliver Queen and Stonehewer Giant Avatars: create a token copy of a random card that is not in the game (#9882)
* Peacekeeper Avatar: an Aura token copy enters attached to a random host (#9897)
* Jhoira of the Ghitu Avatar: copy three random cards and offer a free cast of one (#9904)
* Maelstrom Archangel Avatar: per-creature copies of random cards with the same mana cost, behind a {3} payment, as a token or a cast (#9917)
* Dakkon Blackblade Avatar: play a colored card as a random basic land (#9905), offered only when a copy could be played (#9914)
* Enigma Sphinx Avatar: search for a card chosen at random (#9871), and the searcher may decline to find it (#9893)
* Arcanis, the Omnipotent, Jaya Ballard and Flametongue Kavu Avatars: choose a number at random in a range (#9873)
* Karona, False God Avatar: choose permanents at random (#9887)
* Akroma, Angel of Wrath and Master of the Wild Hunt Avatars: choose among an ability's branches at random (#9895)
* Eight-and-a-Half-Tails Avatar: ask whether a permanent has protection from a color (#9908)
* Sakashima the Impostor Avatar: a copy that keeps the copier's own name (#9872)
* Kresh the Bloodbraided Avatar: trigger on a creature being devoured (#9874), reading its last-known power (#9889)
* Figure of Destiny Avatar: compare a permanent's counters against a number, as in "fewer than X" (#9875)
* Malfegor Avatar: ask whether a permanent was unearthed (#9906)
* Necropotence Avatar: "if it's not the first turn of the game" (#9900)

## Rules
* "Enters with a counter" reads the counters a creature entered with, not ones put on it afterwards (#9891)
* "Same mana cost" matches split cards and {0} correctly, and lands have no mana cost rather than {0} (#9926)
* Under shared team turns, an entering or reselected attacker is offered only its own controller's defenders (#9814)

## Bots
* The search bot's evaluation weights are fitted to how games end: `parun match --positions` records each decision and `parun fit` writes a weights file (#9856)
* The search bot breaks a tie toward passing priority, so it stops activating an ability over and over to no effect (#9836)

## Client improvements

### Web client
* Variants in the lobby: Planechase, dealing planar decks (#9793); Archenemy, dealing a scheme deck (#9796); teams, with Team vs. Team, Two-Headed Giant and Alternating Teams (#9795); up to twelve seats, with Emperor and Grand Melee (#9798)
* Lobby options: deploy creatures (#9803), and shared team turns in Team vs. Team (#9807) and Emperor (#9809)
* The deck builder edits a deck's planar deck (#9797)
* Playing on from a replay: it runs as a new game does, with remote seats, hand-overs and clocks (#9799); its seats are filled in the lobby (#9800), including by invite from another browser (#9805); a paused replay can be played on, and stepping through a long replay is faster (#9861)
* Conceding and timing out: a two-player online game ends the moment a seat concedes (#9792); in a game of three or more, the seat leaves at once (#9802), even while another seat is deciding (#9883), and a subgame is left at once too (#9902)
* The board shows a permanent's chosen values, such as Herald's Horn's creature type (#9833), and each player's ante (#9831)
* A long list of choices is shown as a filterable combo box (#9835), for multi-pick choices such as a library search too (#9853)
* Pay automatically taps the source that gives up least when two tie, so a Forest before Llanowar Elves (#9832)
* Card sizes: Large is gone, and phones start on Compact (#9829); a tapped card's margin fits its turned corner (#9827)

### Command line
* `parun oracle` breaks its coverage down by kind of ability and by the constructor keeping abilities unrendered (#9876)

## Bug fixes
* Reloading the page no longer refunds the time the pending decision's clock had run (#9786)
* A game that ended with a seat leaving early keeps its reason under the headline (#9828)
* A shared pick button can drop back to none, so two identical attackers can both be undeclared (#9830)

## Internal
* `Parun.Oracle.Effect` and `Parun.Oracle.Continuous` are split so parallel rendering work no longer touches the same modules (#9892)
* Lints keep unprinted card shapes out of card data: sacrifice causes (#9889), counters compared against a measure (#9912), and a random permanent pick beside a chooser or an up-to count (#9916)
* Scenarios prove the token-copy Avatars' failure paths (#9899), Dakkon Blackblade Avatar's land-play timing (#9925), and vanguard game-start actions across a restart and a subgame (#9919)
* Decision records: the starting player's turn is cited as the game's first (#9909), Maralen of the Mornsong Avatar's opening-hand reasoning is corrected (#9921), and Dakkon Blackblade Avatar's unobservable entry is dropped (#9925)
* The two-guest whole-game test uses half-size decks, so it no longer times out (#9932)
* Version 0.2026.11.26 (#9804)

**Full Changelog**: https://github.com/parun-mtg/parun/compare/v0.2026.11.25...v0.2026.11.26
