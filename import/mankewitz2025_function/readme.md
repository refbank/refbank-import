# Readme

## Citation

Mankewitz, J., & Hawkins, R. (2025). Function shapes form: Compositionality emerges from communicative needs, not environmental structure alone. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 47).

## Abstract

Human languages are compositional, combining smaller units of meaning to express more complex ideas. To explain the emergence of compositionality, researchers have appealed to functional pressures from communication. However, languages may merely inherit the component structure found in the environment. We designed a reference game to explicitly disentangle these possibilities; pairs of participants (N = 450) communicated about about sets of shapes that were assembled from component parts. Critically, we manipulated whether shapes that shared the same parts were competitors within each trial or were distributed across different trials. We found that participants successfully developed efficient conventions for referring to the shapes. However, participants who needed to distinguish shapes that shared components within the same context were more likely to develop compositional systems. When shared components appeared in separate contexts, participants favored non-compositional conventions. These results suggest compositional language structure most readily emerges from immediate communicative pressures rather than environmental structure alone.

## Study details

Images are coded separately here (per image), but there was structural similarity 
between some images (same top or bottom half sometimes). See original paper for details. 

* recruitment from Prolific
* conditions: non-compositional, within-trial competitor, across-trial competitor
* final sample of 450 dyads after exclusions, across 3 conditions (we have excluded as well- 182 no competitor, 194 within, 176 across)
* roles alternate by trial
* feedback: confirmed with author that full feedback was given (paper says "Both participants received immediate feedback about whether the correct shape was selected.")
* chat box with free typing
* 4 blocks of 16 trials (64 trials total)

## Processing/import

We have images, demographics, text, and selections.

**Player-identity bug (fixed):** the participant-recovery step
(`load_participant_data()`) parsed each player's exit-survey JSON to pull out
demographics; a player who left before completing the exit survey has an empty
`exitSurvey` field, which failed to parse and, via `unnest()`, silently dropped
that player's row entirely. Since this same table is also used to recover the
*other* player's identity when they're the matcher (a director -> partnerID
lookup), losing a row broke that lookup for every round the missing player was
director, producing a phantom shared "unknown player" identity in place of a
real one -- this showed up as ~121 games with 3 distinct player_ids instead of
2. Fixed by keeping a 1-row NA placeholder for unparseable survey data instead
of dropping the row; that player's own demographics come out NA, honestly.

**Exclusions:** previously hardcoded to `exclude = FALSE` for every row (no
real exclusion logic existed). Paper: "participants were excluded from
analysis (but still compensated) if they were missing more than 32 (50%)
trials (n = 52) or had an accuracy rate below 75% (n = 41)." Two further
dyads were excluded for AI-generated-text chat content.

The exact criteria are now taken directly from the authors' own analysis code
(`analysis/02_cogsci2025_analyses.qmd` in
[github.com/JMankewitz/comp-shapes-comm](https://github.com/JMankewitz/comp-shapes-comm)),
not reconstructed by guesswork:
- "missing" = a flat count of trials in the game with no response, across all
  64 trials (not just a trailing run at the end -- there's no special
  handling for scattered vs. trailing timeouts in the source).
- accuracy = fraction correct among trials with an actual response (timed-out
  trials aren't scored either way).
- excluded if missing-trial count > 32 OR accuracy < 0.75.
- two specific games (`game_blacklist` in import.R) are excluded for
  AI-generated text, per the paper authors' own hardcoded game IDs -- not a
  derivable rule.

Applying this exact formula directly to the source repo's own committed
processed data (`data/processed_data/run_v3`) exactly reproduces the paper's
numbers: 41 accuracy-only, 52 missing-only, 2 AI-text blacklist, 450 kept, out
of 617 total games -- confirming the formula itself is correct, not a guess.
That was the missing piece before: our old local `raw_data/` snapshot only
had 552 of those 617 games.

**Live GitHub pull (current):** `import.R` now pulls directly from
[github.com/JMankewitz/comp-shapes-comm](https://github.com/JMankewitz/comp-shapes-comm)
(`data/raw_data/run_v3` and `data/processed_data/run_v3`) instead of a local
`raw_data/` snapshot, the same way other datasets in this repo fetch live from
their source. Getting the full 617-game corpus this way surfaced a handful of
additional raw-data quirks our old (partial, locally-cached) pipeline never
hit:
- `director` is genuinely missing (not just blank) for some rounds -- filled
  in via the modal director among other same-parity rounds in the same game
  (roles alternate by trial, so same-parity rounds normally share a
  director). 10 of 617 games have this unrecoverable for an entire parity
  half or the whole game, and those games are dropped entirely -- validate.R
  requires a real describer per trial as a structural completeness check, not
  a quality-control one, so there's no "exclude" flag for this.
- A handful of rows (4-6 of ~39k) have a *corrupted* `director` field --
  garbled fragments of what looks like tangram-description message text,
  not a player id (one flagged row even has `numTrials = 65` instead of 64,
  confirming upstream CSV row/column misalignment, not a real value). Left
  unguarded, this fabricated a bogus extra "player" (the message-text
  fragment itself, used as a player_id) in 6 games. Fixed by validating
  `director` looks like a real 26-character ULID before trusting it;
  anything else is treated as blank and falls through the same-parity
  fallback above.
- `readr::read_csv()` treats a blank CSV field as `NA` by default, even in
  character columns, unlike the base `read.csv()` the original script used --
  every `response == ""` / `director == ""` check downstream silently
  evaluated to `NA` instead of `TRUE`/`FALSE` until this was pinned down
  (`na = character(0)` on the round.csv read restores the original
  semantics).
- 3 of ~31k trials have a selection that isn't among the images actually
  shown for that trial (upstream glitch) -- these get `selected_image =
  "unk"` rather than passing along a value that can't be a real choice from
  what was on screen.
- For about 1 in 4 games, `player.csv`'s own playerID/partnerID pairing
  doesn't match the player IDs actually used in that game's rounds/chat
  (likely a reconnect assigning a new ID mid-game). Recovering the matcher
  via `player.csv`'s partnerID lookup silently failed for these, leaving
  `player_id = NA` for thousands of selections -- and since player identity
  is deduplicated globally across the whole dataset, those NA rows would
  otherwise collapse into one shared "ghost player" spanning ~150 unrelated
  games. Fixed by deriving matcher identity directly from the player IDs
  actually observed in that game's rounds/chat, ignoring `player.csv`'s
  (unreliable) linking. A residual ~14 games have either only 1 observed
  player id or >2 (genuinely ambiguous) and get a per-game placeholder id
  instead, so they can't collide with each other either.

After all of the above, our own pipeline's output is 449 kept / 41 accuracy /
51 missing (19 missing-only + 32 missing-and-accuracy) / 2 blacklist / 64
"never started" (zero responses recorded) -- within 1-2 games of the
ground-truth numbers above. The residual gap is fully explained by the 10
games dropped for unrecoverable describer identity (several of which the
ground-truth check would also have excluded, just for a different reason).