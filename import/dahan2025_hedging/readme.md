# Readme

## Citation
Dahan, D. (2025). When hedging helps, rather than impedes, communication: collaboration in the referential communication task. Discourse Processes, 62(2), 89-111.

## Abstract

In a referential communication task, where one participant, the director, instructs
their partner, the matcher, to reproduce sequences of cards displaying geometric
configurations (tangrams), directors sometimes include a hedge to their description
when the tangram is hard to describe (e.g. “like an eagle, kinda”). Directors hedge
their descriptions, I argue, to invite matchers to participate in the referring
process by offering a candidate description, thereby mitigating the risk of
misunderstanding. This claim was examined in a corpus with large variability in
matchers’ referential accuracy. Analysis of the matcher’s choices of referent on each
trial revealed that their director’s propensity to include a hedge in their
descriptions over the course of the task was a significant predictor of the matcher’s
success at the task. This finding supports the view that successful communication
benefits from the actions that both conversational partners take to jointly establish
the mutual belief that, for each utterance, the addressee has understood what the
speaker meant.

## Study details

Participants: 40 participants (20 pairs/games), recruited via Craigslist (half paired
with each other, half brought a friend to play with).

Procedure:
* play face to face with visual barrier
* set of 16 cards (either Set 1 or Set 2)
* each rep there is a target sequence of 3: (marked as red / blue / green color boxes)
* switch roles every 4 reps (12 trials)
* 48 total trials (16 targets x 3 each)
* no feedback on errors

Targets: tangrams

## Processing/import

We received raw audio files and transcribed them using whisper (or similar);
a contractor identified which target number each was referring to, tagged (highly)
irrelevant messages, and IDed what messages came from describer or matcher based on
the transcripts.

We have classified this as order -- but it's really a pick 3 in order out of 16, which
is different. Because they could revisit previous choices within the rep, we've called
it order.

We theoretically have timing info because we have the recordings, but we don't have it
per-trial without more work.

For prior_relationship -- there is for sure a prior relationship in half of them
(friends who came together), probably for another quarter, and not for the last
quarter. We've labelled it as yes because we can't tell which are which.

language = English is confirmed (US-based study; audio recordings/transcripts
are all English), even though the paper itself doesn't state this
explicitly. Native language (of individual participants) is presumably
English in most cases; we haven't marked education because it's uncertain
for this sample.

We have target images. I don't think these are in kilogram, but we could check.

## Note on companion dataset

This 2025 study and Dahan (2023) ("Collaboration under uncertainty in unscripted
conversations...", JEP:LMC) share an identical task/procedure and were originally
imported together as two conditions of one combined dataset (`dahan2023_collaboration`,
game_ids 21-40 there corresponding to this sample). They were split (2026-08-27) into
two separate dataset_ids once it was confirmed these are two distinct papers/samples
rather than one dataset with an inferred split -- see
`import/dahan2023_collaboration/readme.md` for the 2023 sample's details and for the
shared whisper-transcription history (`dahan_outputs/`, `dahan_outputs_diarised/`,
`prep_segment.R`), which was not duplicated here since it covers both samples combined
and isn't read by either project's current `import.R`.

The raw source data numbers these dyads 21-40 (matching the "D21SET1".."D40SET2"
dyad codes in `Alignment_Data.txt` and `segmented_transcript.csv`'s `game` column) --
`import.R` reads them as-is under those original numbers. `validate_dataset()`
then renumbers `game_id` to 1-20 for the harmonized output, same as it does for
every dataset; the 21-40 raw numbering is only visible in this project's own
`raw_data/`, not in `harmonized_data/`.
