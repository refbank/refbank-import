# Readme

## Citation
Dahan, D. (2023). Collaboration under uncertainty in unscripted conversations: The role of hedges. Journal of Experimental Psychology: Learning, Memory, and Cognition, 49(2), 320.

## Abstract

The present study examined the role of hedges in a referential communication task.
Pairs of participants received an identical set of cards, each card displaying a
geometric configuration (a “tangram”). One participant, the director, instructed
their partner, the matcher, to reproduce a series of predetermined tangram sequences
using their own cards. Directors sometimes included a hedge in their description
of a tangram (e.g., “looks kinda like an eagle”), and more so the first time than
on subsequent mentions. The present study tested the hypothesis that, by revealing
their uncertainty regarding the adequacy of their description in conveying the
intended meaning, directors signal a possible difficulty in establishing reference.
This in turn prompts their addressee to display, rather than merely assert, their
understanding (by presenting a description for the tangram the matcher believes
the director is referring to, for the director to evaluate). Analyses of matchers’
responses to descriptions that directors had hedged or not confirmed the hypothesis.
This finding supports the view that conversational partners work together to reach
the mutual belief that they have coordinated what the speaker means and what their
addressee takes them to mean. Conversational partners expend more joint effort when
they deem the risk of misunderstanding to be high than when it is perceived to be low.

## Study details

Participants: 40 participants (20 pairs/games), recruited from a university class
(half played with a classmate, half with a friend they brought).

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
(friends), probably for another quarter (classmates), and not for the last quarter.
We've labelled it as yes because we can't tell which are which.

language = English is confirmed (US-based study; audio recordings/transcripts
are all English), even though the paper itself doesn't state this
explicitly. Native language (of individual participants) is presumably
English in most cases; education is presumably mostly "some-college", but we
haven't marked either of those because they're less certain than the
testing/interaction language.

We have target images. I don't think these are in kilogram, but we could check.

## Note on companion dataset

This 2023 study and Dahan (2025) ("When hedging helps, rather than impedes,
communication...", Discourse Processes) share an identical task/procedure and were
originally imported together as two conditions of one combined dataset. They were
split (2026-08-27) into two separate dataset_ids, `dahan2023_collaboration` and
`dahan2025_hedging`, once it was confirmed that they really are two distinct papers/
samples rather than one dataset with an inferred split -- see
`import/dahan2025_hedging/readme.md` for the 2025 sample's details.

The raw whisper-transcription intermediates (`dahan_outputs/`, `dahan_outputs_diarised/`)
and the one-time segmentation-prep script (`prep_segment.R`) that originally produced
`segmented_transcript.csv` were NOT split -- they cover all 40 original dyads (both
samples) combined and are historical/already-consumed artifacts, not read by either
project's current `import.R`. They remain here rather than being duplicated or split.
