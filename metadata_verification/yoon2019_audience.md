# yoon2019_audience

**Source:** Yoon, S. O., & Brown-Schmidt, S. (2019). Audience design in multiparty conversation. *Cognitive Science*, 43(8), e12774. PDF read in full (28 pages) from `/home/vboyce/Zotero/storage/CH9FGBEY/Yoon and Brown‐Schmidt - 2019 - Audience Design in Multiparty Conversation.pdf`.

## Citation check

- **Title/authors/journal/DOI**: VERIFIED — title page reads "Audience Design in Multiparty Conversation," Si On Yoon & Sarah Brown-Schmidt, header "Cognitive Science 43 (2019) e12774," "DOI: 10.1111/cogs.12774," received 30 May 2017 / accepted 17 June 2019.
- **Issue number "43(8)"**: UNKNOWN — the PDF itself never states the issue number (only "43 (2019) e12774" in the running header); the article-number format (e12774) is consistent with a Wiley electronic-only Cognitive Science article, but issue 8 could not be independently confirmed from this document.
- **short_cite** "Yoon & Brown-Schmidt (2019)": VERIFIED, matches author names/year.

## Condition-level fields

Notation below: Exp1 = 4 conditions (group_size 4), Exp2 = 5 conditions (group_size 5), Exp3 = 4 conditions (group_size 7).

### group_size

- **Exp1 = 4**: VERIFIED — "One hundred and twelve undergraduates (28 groups of four people)... Four participants took part in the study at the same time and were randomly assigned to the roles of Director, Matcher 1, Matcher 2, and Matcher 3" (p.6, §2.1.1).
- **Exp2 = 5**: VERIFIED — "One hundred and sixty-five undergraduates (35 groups of 5 people)... Five participants took part in the study at the same time and were randomly assigned to the roles of Director, Matcher 1, Matcher 2, Matcher 3, and Matcher 4" (p.13, §3.1.1).
- **Exp3 = 7**: VERIFIED — "Ninety-eight undergraduates... recruited in groups of 7 (14 groups total)... Participants were randomly assigned to the role of Director, or one of six Matcher roles" (p.18, §4.1.1).

### language = English

VERIFIED for all three experiments — "All participants were native speakers of North American English" (stated separately in §2.1.1, §3.1.1, and implied via identical population description in §4.1.1).

### population = adult

VERIFIED for all — participants in all three experiments are University of Illinois at Urbana-Champaign "undergraduates."

### confederates = no

VERIFIED for all 13 conditions — in every experiment, all Director/Matcher roles are filled by randomly assigned real participants recruited as a group ("Four/Five participants took part in the study at the same time and were randomly assigned to the roles of Director, Matcher 1, ..."; Exp3: "Participants were randomly assigned to the role of Director, or one of six Matcher roles"). The "knowledgeable" vs. "naive" status of a Matcher arises entirely from whether that Matcher participated in the sorting phase, not from confederate/experimenter status. The word "confederate" does not appear anywhere in the paper.

### modality = oral-in-person

VERIFIED for all — Exp1: "The Director and Matcher 1 could see each other... During the first phase of the experiment, the Director and Matcher 1 were in a room together" (p.6-7); at test, "each participant was seated at a separate computer" in the room (p.7). Exp3: "the Director and three of the Matchers were seated together in the testing room" (p.18). All experiments describe live, co-present spoken interaction (with a computer/booklet interface, not remote).

### backchannel = full

VERIFIED for all — Exp1: "While test trials were interactive and Matchers were allowed to give feedback and ask questions as needed, they did so infrequently" (p.8); sorting phase: "There was no restriction on what partners could say" (p.7). Exp2 procedure stated to be "identical to Experiment 1" (p.13). Exp3: "the partners were free to interact and converse in order to complete the task" (p.18), and design is "similar to Experiments 1 and 2" (p.18). No restriction on listener communication is described in any experiment.

### feedback = none

UNKNOWN for all 13 conditions, across all three experiments — the paper never discusses experimenter-provided or system-provided correctness/performance feedback to participants (e.g., accuracy scores, "correct/incorrect" signals shown to Directors or Matchers, bonus tied to performance). Matcher accuracy (Tables 3, 5, 7) is computed post hoc by the researchers for analysis purposes and is never described as being communicated back to participants during the task. Two "feedback" mentions were checked and explicitly excluded from supporting this field per the corrected definition, because they concern the separate `backchannel` construct (listener-to-speaker talk-back during a trial), not correctness signals: (1) "test trials were interactive and Matchers were allowed to give feedback and ask questions as needed" (p.8, Exp1; procedure repeated verbatim for Exp2/Exp3); (2) "in order to minimize any potential effect of Matcher feedback in this measure, we only analyze the Director's 'initiating reference'... prior to any feedback from the Matchers" (p.9). A third mention, in the General Discussion ("constraints on the form of the conversation, such as whether it is appropriate for addressees to provide feedback or not," p.22), is a general remark about multiparty conversation as a topic, not a description of this study's own procedure. No statement anywhere confirms or contradicts `feedback=none`.

### prior_relationship = no

- **Exp1**: VERIFIED — "None of the participants knew each other prior to participating" (p.6, §2.1.1).
- **Exp2**: VERIFIED — "None of them had known each other before participating in the study" (p.13, §3.1.1).
- **Exp3**: **FLAG** — no blanket "none of the participants knew each other" statement is given. Instead: "Two pairs of participants (from different groups) who had known each other prior to the study were assigned to the role of Matcher, and therefore did not talk to one another during the task" (p.18, §4.1.1). This indicates at least 4 of the 98 Exp3 participants *did* have a prior relationship with another study participant; the paper's mitigation (placing them in different groups / non-interacting Matcher roles) may mean no two co-present, interacting group members knew each other, but this is a materially different (and weaker) guarantee than the explicit "none knew each other" statements given for Exp1 and Exp2. Recommend flagging `prior_relationship=no` for all four Exp3 conditions (expt3_3N, expt3_1K2N, expt3_2K1N, expt3_3K) for review — "no" may still be the best available label, but the paper does not support it as cleanly as for Exp1/Exp2.

### partner_constancy = no

VERIFIED, but with an important nuance the recorded value only captures at the whole-session level — worth documenting explicitly:

- All three experiments use a **blocked, within-subjects design**: the same fixed group of individuals rotates through *all* conditions of its experiment in one session (counterbalanced block order), e.g., Exp1: "Four different sorting-test blocks rotated each participant group through the four conditions... the order of blocks was counterbalanced across groups" (p.8). Critically, the specific person(s) addressed by the Director changes from block to block/condition to condition — e.g., in Exp1's 2K1N condition, "Matcher 1 left the room, and Matcher 2 joined the Director" for a second sorting round, and then all three Matchers reconvene for test (p.7); in Exp1's 1N condition, the sorting partner (Matcher 1, the only one ever exposed to labels) is *not* the naive Matcher addressed at test. The Exp3 summary states this explicitly: "the combination of addressees at test was unannounced to the speakers and changed across the four blocks of the task" (p.21).
- This supports `partner_constancy=no` as a description of the whole session/task (the audience composition a Director must track is not fixed across the session).
- However, **within any single condition's test phase**, the addressee set is stable across all test trials of that block (e.g., 1K's 16 test trials are all directed at the same Matcher 1; 2K1N's test trials are all directed at the same three-Matcher group). If `partner_constancy` were instead intended to describe within-condition trial-to-trial stability, the answer would be "yes" for every condition. Given the field is recorded once per condition row but the rotation phenomenon is a property of the *session* (spanning all conditions), we consider `no` defensible as currently coded, but flag this as a field-definition ambiguity worth resolving/documenting rather than a factual error.
- Additionally, the **1K conditions** (expt1_1K, expt2_1K) are a partial exception even within a single block: the sorting-phase partner (Matcher 1) and the test-phase partner are the *same* individual throughout, unlike every other condition in Exp1/Exp2 where sorting partners and test addressee(s) diverge. This doesn't change the recommended "no" label but is worth noting as the closest case to genuine partner constancy in the dataset.

### role_constancy = yes

- **Exp1**: VERIFIED — "Participants maintained their role (Director, Matcher 1, Matcher 2, Matcher 3) across all four blocks" (p.8).
- **Exp2**: VERIFIED — "The participants maintained their role (as Director or Matcher) across the five blocks of trials" (p.14).
- **Exp3**: **FLAG** — confirmed, precisely, as a genuine `role_constancy=no` case, not merely a partner/audience-composition change. Each group of 7 completes 8 blocks total: the first four blocks (conditions 3N, 2K1N, 1K2N, 3K) with one Director and six Matchers, then a second set of four blocks (same four conditions, new stimuli) with a **second, different Director**: "a different participant playing the role of Director... was randomly chosen from the three Matchers during the previous four blocks in order to collect data from two Directors per group. The remaining participants were assigned to the role of Matcher" (p.19, §4.1.2). This is confirmed elsewhere: "In Experiment 3, two participants in each group played the role of Director" (p.19, §4.3), and the paper explicitly analyzes "Director order (first vs. second Director)" as a variable (p.19). This is a literal individual switching from Matcher (listener) to Director (describer) within the same session — not a change in who the Director is addressing (that is the separate `partner_constancy` phenomenon, already coded `no`). Because each of the four named Exp3 conditions (expt3_3N, expt3_1K2N, expt3_2K1N, expt3_3K) is instantiated once in each four-block half — i.e., once with the original, never-switching Director and once with the second Director who was a Matcher for the first four blocks — every one of the 4 Exp3 conditions' data pools includes trials produced by a Director who was a Matcher earlier in the same session. The recorded `role_constancy=yes` is therefore incorrect for all four Exp3 conditions; the correct value is `no`.

## Summary of flags

- **VERIFIED**: group_size (×3), language, population, confederates, modality, backchannel — all fully confirmed; prior_relationship for Exp1/Exp2 (8 conditions); role_constancy for Exp1/Exp2 (9 conditions, expt1_* and expt2_*); partner_constancy for all 13 conditions (with a session-vs-condition-level nuance documented above).
- **UNKNOWN**: feedback (all 13 conditions) — paper never describes correctness/performance feedback being given to participants; Matcher accuracy is a researcher-computed analysis metric, not a signal shown to participants. The only "feedback" language in the paper concerns listener-to-speaker backchannel during trials (already captured by the separate `backchannel` field) or is a general-discussion remark not describing this study's own procedure. **Dataset owner confirms (2026-08-27)** "none" as the default assumption given no mention, already documented in readme.md.
- **FLAG**: prior_relationship for the 4 Exp3 conditions (two pre-acquainted pairs existed among participants, mitigated by group placement rather than eliminated).
- **role_constancy for the 4 Exp3 conditions — checked, NOT an error, recorded "yes" stands.** Each group of 7 ran a second Director, drawn from the six Matchers of the first four blocks, for the second four blocks ("a different participant playing the role of Director... was randomly chosen from the three Matchers during the previous four blocks," p.19) — a real individual role-switch *across the two four-block halves*. But refbank codes each four-block half as its own separate `game_id`, with role_constancy evaluated per-game rather than across the whole multi-block session. Within any single `game_id`, no one is both describer and matcher, so "yes" is correct as recorded. (Confirmed directly by the dataset owner, 2026-08-27 — this reverses the FLAG originally raised here.)

Rough count: **~16 VERIFIED**, **1 UNKNOWN field** (applies to all 13 rows), **1 FLAG field** (prior_relationship, applies to the 4 Exp3 conditions).
