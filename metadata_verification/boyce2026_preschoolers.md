# boyce2026_preschoolers

**Source:** `/home/vboyce/Research/kid-tangrams/write-ups/manuscript/preschoolers-can-coordinate.tex` (most current version per instructions; PDF not needed, .tex parsed cleanly)

## Citation check

- **full_cite:** "Boyce, V., Sparks, R. Z., & Frank, M. C. (2026). Preschoolers can coordinate with each other to communicate about novel referents. Preprint"
  - VERIFIED — Title: `\title{Preschoolers can coordinate with each other to communicate about novel referents}`; Authors: `\author{Veronica Boyce\textsuperscript{1,2}, Robert Z. Sparks\textsuperscript{1}, \& Michael C. Frank\textsuperscript{1}}`. Author order, names, and title match exactly.
- **short_cite:** "Boyce et al. (2026)" — VERIFIED, consistent with full citation (3 authors, first author Boyce).

## Condition-level fields

Fields are identical across Experiments 1–3 per manuscript (Exp2: "As Experiment 2 was very similar to Experiment 1, we focus on the differences..."; Exp3: "The procedure for Experiment 3 matched Experiment 2, and the only change was in the stimuli used."). No differences were found in any of the fields below across the three experiments, so results apply to expt1/expt2/expt3 uniformly except where noted.

### group_size = 2
VERIFIED — "Children played with another child from the same class." (Exp1, Participants) and consistently "pairs of children" / "games" (a game = one dyad) throughout all three experiments' Participants sections (e.g., "19 games that completed 12 critical trials", "30 pairs of children completed all 16 critical trials").

### language = English
UNKNOWN from the manuscript text — it never explicitly states the language of testing/data collection. All example child utterances quoted (e.g., "a person flying," "a walrus," "The walking person or the standing person that's holding something?") are in English, and the sample was recruited from a "university preschool laboratory preschool located in the Bay Area, California," which is strong circumstantial evidence for English. **Confirmed (2026-08-27) directly by the dataset owner** — testing language is English — and now documented in the dataset's readme.md.

### prior_relationship = yes
VERIFIED — "Children played with another child from the same class." (Exp1, Participants). Also, in the pooled Qualitative observations section (covering all three experiments): "all children were paired with another child from the same classroom who they were willing to play with, but friendship levels varied." Both quotes confirm pre-existing peer relationships (classmates) for all three experiments (same preschool, same recruitment method throughout).

### partner_constancy = yes
VERIFIED (inferred from procedure description, no single explicit "partners stayed constant" sentence) — Each "game" is one pair of children completing a full session of trials together: "Once a pair of children agreed to play the game, a research assistant took them to a quiet testing room... Children sat across a table from each other, each with a tablet in front of them." Analysis units are per-pair "games" (e.g., "19 games that completed 12 critical trials"), and the only alternation described within a session is role switching, not partner switching: "After each trial, children switched roles." No mention anywhere of partners changing mid-session.

### role_constancy = no
VERIFIED (re-checked against corrected definition: "no" = at least one participant was both describer and matcher at different points) — "After each trial, children switched roles." (Exp1, Procedure) directly shows each child in a pair alternated between "teller" (describer) and "guesser" (matcher) roles across trials within the same session — not a fixed single role for the whole session. This procedure is unchanged in Exp2 ("The procedure was much the same as Experiment 1") and Exp3 ("The procedure was the same as for Experiment 2"). Note this is about the individual's own role over time, independent of partner_constancy (which addresses whether the partner/audience stayed the same — see above, yes).

### population = child
VERIFIED — "4 and 5-year-old children were recruited from a university preschool laboratory preschool..." (Exp1, Participants); consistent age ranges reported for Exp2 (median 4;8, range 3;9–5;9) and Exp3 (median 4;9, range 4;0–5;10).

### confederates = no
VERIFIED — Both members of each dyad are child participants; the research assistant present is explicitly described as not a game participant: "Rather than have a parent who played the game and also kept the child on task, we had an RA who was not participating in the game but did prompt children as needed to keep them on task." (Comparison to methods in prior work, Exp1).

### modality = oral-in-person
VERIFIED — "Children sat across a table from each other, each with a tablet in front of them" and were "asked to 'tell Smurfy what they see' in the black box" (Exp1, Procedure) — face-to-face, spoken descriptions (not typed/written), confirmed by transcription of spoken utterances via Whisper ("Children's descriptions were automatically transcribed from the video using Whisper").

### feedback = full
VERIFIED (re-checked against corrected definition: feedback = correctness signal only, unrelated to backchannel/talking) — "When the guesser selected an image, both children received feedback in the form of a smiley or frowny face and an excited or disappointed sound." (Exp1, Procedure) — this smiley/frowny signal is explicitly a correctness indicator, not a description of verbal exchange. Confirmed explicitly correctness-based by contrast with Leung et al. (2025): "Leung et al. (2025) did not have feedback on the correctness of a match, and we did." Feedback was given after every trial in all three experiments (procedure unchanged in Exp2/Exp3). (No quotes about the matcher talking back were used to support this value.)

### backchannel = full
VERIFIED — The design permits live, spoken back-and-forth between describer and matcher during a trial (not restricted to one-way description). Qualitative observations describe listeners freely asking clarifying questions and describers responding within a trial: "Some children asked clarifying questions of their partner..." and the worked example: "child A described the figure as 'A human', prompting child B to note 'There's two humans.' Later in the game, child B used the description 'A human', leading child A to ask 'Which one?', which child B clarified with 'The one that is walking'." Also: "Occasionally, a guesser would provide descriptions of both images... so the teller just had to indicate which one." These exchanges indicate unrestricted two-way verbal communication during trials, consistent with backchannel = full.

## Summary of flags

- VERIFIED from manuscript text: 9 fields (group_size, prior_relationship, partner_constancy, role_constancy, population, confederates, modality, feedback, backchannel)
- Confirmed by dataset owner (not stated in manuscript text): 1 field (language = English)
- FLAG: 0

**Count: 10/10 fields confirmed** (9 from the manuscript text, 1 from the dataset owner directly) — per condition-field; identical across expt1/expt2/expt3, no cross-experiment discrepancies found.

**Re-check (corrected definitions for role_constancy and feedback):** Both fields re-verified against the corrected definitions — role_constancy="no" (individual children alternate describer/matcher roles across trials, per "After each trial, children switched roles") and feedback="full" (correctness signaled via smiley/frowny face + sound each trial, independent of any backchannel/talking). No value changes; counts above remain the same.
