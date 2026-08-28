# boegels2025_power

**Source:** `/home/vboyce/Zotero/storage/8KWJGUDT/.zotero-ft-cache` (plain-text extraction of Bögels, Li, Rasenberg, Eijk, Toni, & Pouw, "There is a power law of joint communicative effort and it reflects communicative work," *Cognition*, 268, 106370). Full text (Abstract through References, footnotes, and copyright line) was present and not truncated, so the HTML fallback was not needed.

Note: This paper reports secondary/reanalysis work on the **CABB dataset** (42 pairs), whose full experimental procedure is documented in a companion methods paper: Eijk, L., Rasenberg, M., Arnese, F., Blokpoel, M., Dingemanse, M., Doeller, C. F., Ernestus, M., Holler, J., Milivojevic, B., Özyürek, A., Pouw, W., van Rooij, I., Schriefers, H., Toni, I., Trujillo, J., & Bögels, S. (2022). "The CABB dataset: A multimodal corpus of communicative interactions for behavioural and neural analyses." *NeuroImage*, 264, 119734 — read in full (2026-08-27) at `/home/vboyce/Zotero/storage/RIHU89EW/Eijk et al. - 2022 - The CABB dataset A multimodal corpus of communica.pdf` (`.zotero-ft-cache` text version). Since this companion paper documents the underlying CABB task/procedure that Bögels et al. reanalyzed, fields it states directly are treated as VERIFIED for this dataset too (not just "inferred").

## Citation check

- **Authors/order:** Matches — "Sara Bögels, Tianyi Li, Marlou Rasenberg, Lotte Eijk, Ivan Toni, Wim Pouw" corresponds to recorded "Bögels, S., Li, T., Rasenberg, M., Eijk, L., Toni, I., & Pouw, W."
- **Title:** Matches exactly: "There is a power law of joint communicative effort and it reflects communicative work."
- **Journal/volume/year:** Paper header reads "Cognition, Volume 268, March 2026, 106370" (DOI issued 2025: `10.1016/j.cognition.2025.106370`; copyright line "© 2025 The Authors"). Recorded year (2026) is consistent with the print/volume date shown on the article page.
- **FLAG (minor, citation formatting):** Recorded full_cite gives the article number as "268(10637)" — this drops the trailing digit of the actual article number **106370** (as printed: "Volume 268, March 2026, 106370"). Should read 106370, not 10637. Recorded page value "0" is a placeholder consistent with this journal using article numbers instead of page ranges, so that alone is not flagged.
- **short_cite:** "Bögels et al. (2026)" — consistent with the full_cite year.

## Condition-level fields (condition_label = "expt1")

**group_size = 2**
VERIFIED — "Our current dataset consists of 42 pairs of participants from the CABB dataset" (Methods, §2); throughout, tasks are described in terms of a "director" and "matcher," i.e., dyads.

**language = Dutch**
VERIFIED (2026-08-27, upgraded from UNKNOWN via the Eijk et al. 2022 CABB companion paper) — Abstract: "audio/video and motion-tracking recordings of face-to-face, task-based communicative interactions in **Dutch**"; Participants section: "142 right-handed, **native Dutch speakers** (71 pairs...)". Boegels et al.'s 42-pair sample is explicitly a subset of this same CABB dataset ("Our current dataset consists of 42 pairs of participants from the CABB dataset," Methods, §2), so this directly verifies the language for this dataset's participants too.

**prior_relationship = no**
VERIFIED — "The procedure was such that participants who did not know each other formed pairs in the lab." (Methods, §2)

**partner_constancy = yes**
VERIFIED — Methods describes a single pair "interact[ing] together" for all six rounds: "Director and matcher roles switched each trial and all 16 Fribbles were described six times in total (i.e., in six rounds)." (Methods, §2). No mention of partner reassignment mid-session; the whole procedure (pre-tasks, interaction, post-tasks) is described as occurring within one fixed pair.

**role_constancy = no**
VERIFIED (re-checked against corrected definition: "no" = at least one participant was literally both describer and matcher at different points, independent of partner identity) — "Director and matcher roles switched each trial and all 16 Fribbles were described six times in total (i.e., in six rounds)." (Methods, §2) — this directly states each participant took both the director and matcher role at different trials, so role_constancy = no is confirmed.

**population = adult**
VERIFIED — "The participants were 22.4 years old on average (SD = 3.02, range = 18–33)." (Methods, §2)

**confederates = no**
VERIFIED — "The procedure was such that participants who did not know each other formed pairs in the lab," combined with the reported pair gender composition ("17 all-female pairs, 5 all-male pairs, and 20 mixed-gender pairs") and both members performing individual pre/post tasks used as dependent measures (Naming/Features tasks) — all indicate genuine participant dyads rather than a fixed confederate. The word "confederate" does not appear in the text, but the description is inconsistent with confederate use.

**modality = oral-in-person**
VERIFIED — Abstract: "multimodal and collaborative face-to-face dialogues about displaced referents." Methods: "Interactions were audio- and video recorded, and movement measurements were taken from both participants using a Microsoft Kinect system," consistent with co-present spoken/gestural interaction.

**feedback = none**
Genuinely UNKNOWN from both papers, even after checking the Eijk et al. (2022) CABB companion paper (2026-08-27). Searched its full text for "correct," "feedback," "accuracy," "score." It states the interaction happened "without restrictions on communicative means (e.g., speech, gestures), timing, turn-taking, or **feedback**" — but in context this "feedback" is about conversational/interactional freedom (participants free to backchannel/respond to each other), the same sense used in its own References list ("Allwood et al., 1992, On the semantics and pragmatics of linguistic feedback"; "Dideriksen et al., 2019... backchannel, Repair and Linguistic Alignment"), not correctness feedback from the experimenter — so this quote supports `backchannel`, not this field, and must not be used to justify a `feedback` value per the corrected definition. The paper does describe researchers later coding `correct_answer`/`given_answer`/`accuracy` from the recordings during transcription (§2.6), but that's post-hoc analysis, not something communicated to participants during the task. No statement anywhere (in either paper) confirms or denies a correctness-feedback mechanism during the session itself. **Dataset owner confirms (2026-08-27)** this is a reasonable inference-from-silence: absence of any feedback mention in either paper is treated as evidence no feedback was given, already documented in readme.md ("correctness feedback is not mentioned (in either paper), so we presume that it was not given").

**backchannel = full**
VERIFIED — Introduction: "The matcher contributes too, for example through multimodal backchannels and requests for clarification." Methods: "the matcher, who had to find the Fribble on their own screen, while being allowed to interact with the director" — the matcher can freely respond/backchannel to the director during the trial.

## Summary of flags

- Verified: 9 (group_size, language, prior_relationship, partner_constancy, role_constancy, population, confederates, modality, backchannel) — language upgraded 2026-08-27 via the Eijk et al. (2022) CABB companion paper.
- Unknown: 1 (feedback) — checked the companion paper too, still not stated anywhere.
- Flagged: 0 condition fields; 1 citation-level flag (article number typo: "10637" should be "106370").
