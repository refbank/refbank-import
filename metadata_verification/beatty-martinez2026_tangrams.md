# beatty-martinez2026_tangrams

**Source:** Beatty-Martínez, A. L., Jiaze, L., Shen, Y., Mulík, S., Hawkins, R. D., Guzzardo Tamargo, R. E., & Dussias, P. E. (2026). The Tangrams Codeswitching Corpus. PDF read in full (25 pages) at `/home/vboyce/Zotero/storage/K7KIM5JV/Beatty-Martínez et al. - The Tangrams Codeswitching Corpus.pdf`.

## Citation check

- **full_cite**: Author list in our record has "Tamargo, R. E. G." — the paper byline is "Rosa E. Guzzardo Tamargo" (Guzzardo Tamargo is the surname, "R.E." are initials), so the correctly ordered surname-first form should be "Guzzardo Tamargo, R. E." rather than "Tamargo, R. E. G." **FLAG** (name-ordering error, not a content error — surname component "Guzzardo" is being treated as a middle name). All other authors and the year/title match the PDF title page exactly.
- **short_cite**: "Beatty-Martinez et al. (2026)" — matches first author surname (accented in paper as "Beatty-Martínez") and year. VERIFIED (minor accent stripping is standard/acceptable).

## Condition-level fields

- **group_size = 2**
  VERIFIED — "We implemented a dyadic cued repeated reference game... pairs of participants are assigned the role of either 'director' or 'matcher'" (Method, Tangrams task, p. 6).

- **language = "Spanish/English"**
  VERIFIED — "This Data Descriptor introduces the Tangrams Codeswitching Corpus, a Spanish-English, text-based dataset (30,858 words)" (Background & Summary, p. 3); "use of the chat box was unrestricted: participants were free to use any language(s) of their choice" (Tangrams task, p. 6). The dyads were habitual Spanish-English codeswitchers free to mix both languages, so "Spanish/English" accurately captures the available/used language set (the paper frames it as bilingual codeswitching rather than a single fixed language, but the pair of languages recorded is correct).

- **prior_relationship = yes**
  VERIFIED — "recruitment targeted pairs of bilingual individuals who knew each other well and had an established relationship (e.g., close friends or family members)" (Recruitment and eligibility criteria, p. 6).

- **partner_constancy = yes**
  VERIFIED — Dyads were recruited and completed the single continuous Tangrams task together ("The session began with the dyadic Tangrams task, which lasted around 40 minutes... participants sat at two computers positioned back-to-back," Procedure, p. 12). The task ran as one uninterrupted 60-trial session (5 blocks × 12 trials) for the recruited pair; no mention of reassigning partners mid-task.

- **role_constancy = no**
  VERIFIED (2026-08-27, upgraded from UNKNOWN) — the paper's text doesn't
  state this, but the harmonized `trials.csv` does: checked
  `describer`/`matchers` per `game_id` directly. In every one of the 45
  games, the two players swap roles partway through — e.g. game 1: player 2
  describes trials 1–12, then player 1 describes trials 13+ (matchers
  correspondingly swap the other way). This is a literal per-player
  describer/matcher role swap within the session, confirmed in all 45
  games, not just an artifact of set-overlap: the trial-level sequence
  shows an actual switch partway through, not random alternation.

- **feedback = full**
  VERIFIED — Re-checked against the corrected definition (correctness/accuracy feedback only, independent of backchannel/talk-back). "Participants received immediate accuracy feedback following each trial: the director saw which tangram the matcher selected, and the matcher saw the intended target tangram. Correct target selection resulted in the addition of one point to a shared scoreboard" (Tangrams task, p. 6). This quote concerns correctness signaling only (selection shown + score increment), not the chat/talk-back channel, so it independently supports "full" correctness feedback.

- **population = adult**
  VERIFIED — "Both members of each dyad were required to be at least 18 years of age" (Recruitment and eligibility criteria, p. 6); mean age 21.28 years, SD = 3.81 (Method, p. 5).

- **confederates = no**
  VERIFIED (by strong implication) — Both members of each dyad were independently recruited, consented, and compensated participants ("90 highly proficient Spanish-English bilinguals... All participants gave informed consent... compensated at a rate of $15 per hour," Method, p. 5; dyads recruited jointly via "campus flyers, social media, and word-of-mouth," p. 6). Nothing in the paper indicates either member of a dyad was a lab confederate; both are described identically as recruited participants.

- **modality = written**
  VERIFIED — "a text-based dataset"; participants interacted via "a virtual room containing a chat box"; "We collected the raw text of every chat message sent" (Tangrams task, p. 6). Reinforced by "[participants] wore noise-cancelling headphones playing white noise to ensure all communication occurred strictly through the digital interface" (Procedure, p. 12) — no oral channel existed.

- **feedback = full**
  VERIFIED — "Participants received immediate accuracy feedback following each trial: the director saw which tangram the matcher selected, and the matcher saw the intended target tangram. Correct target selection resulted in the addition of one point to a shared scoreboard" (Tangrams task, p. 6).

- **backchannel = full**
  VERIFIED — The chat interface was shared and bidirectional with no restriction on either party: "use of the chat box was unrestricted: participants were free to use any language(s) of their choice, and there were no constraints on the number or length of messages" (Tangrams task, p. 6). Fig. 1's example chat log shows both roles contributing turns (e.g., matcher responses "I think I know," "mhm"), confirming the listener could freely respond during a trial.

## Summary of flags

- 9 fields checked: **8 VERIFIED** (role_constancy confirmed directly from the raw trial data, not just the paper text), 0 UNKNOWN, 0 FLAG on condition fields.
- Citation check: 1 FLAG (full_cite mis-orders "Guzzardo Tamargo" as "Tamargo, R. E. G."), short_cite VERIFIED.
