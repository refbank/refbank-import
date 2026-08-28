# boyce2024_interaction

**Source:** Boyce, V., Hawkins, R. D., Goodman, N. D., & Frank, M. C. (2024). Interaction structure constrains the emergence of conventions in group communication. *Proceedings of the National Academy of Sciences*, 121(28), e2403888121. Full text fetched from https://pmc.ncbi.nlm.nih.gov/articles/PMC11252989/ (via Europe PMC full-text XML mirror, since the PMC page itself returned a reCAPTCHA gate).

## Citation check

- **full_cite**: matches the paper exactly (authors, year, title, journal, volume/issue/article number). VERIFIED.
- **short_cite**: "Boyce et al. (2024)" — matches first-author surname and year. VERIFIED.

## Condition-level fields

### Fields constant across all 12 conditions

- **language = English**
  VERIFIED — "All participants self-reported as fluent native English speakers on Prolific's demographic prescreen."

- **modality = written**
  VERIFIED — Communication throughout is via "chat box interface" / "chatbox"; explicitly framed as text: "The text-based chat modalities arguably more closely resemble the interfaces used by modern teams who increasingly communicate through group text threads or popular platforms like Slack or Discord." No oral/voice channel is described anywhere.

- **partner_constancy = yes** (all 12 conditions)
  VERIFIED — "Participants were organized into 313 groups of size two to six for a communication game," and within a game, "The same person was the describer for an entire block, but participants rotated roles between blocks." This is the key disambiguation the task asked to check carefully: the group of people (partners) a participant played with was fixed for the whole game/session in every experiment and every condition. What varies across conditions labeled "rotate" vs. "no_rotate" (and "thin"/"thick" role behavior) is **which role** (describer vs. matcher) a given group member holds over time — i.e., **role_constancy**, not partner_constancy. No condition in any of the three experiments reassigns participants to a different set of partners mid-session. So "partner_constancy=yes" is correct for all 12 conditions, and the codebase's "rotate"/"no_rotate" naming maps onto `role_constancy`, exactly as the task description flagged as a common confusion point — verified here that our mapping is the correct one.

- **population = adult**
  UNKNOWN from the paper text — Participants were recruited via Prolific, and the paper describes recruitment/prescreening ("fluent native English speakers on Prolific's demographic prescreen") but never states an explicit age criterion (e.g., "18 or older") or reports participant ages. Prolific's platform policy restricts to adults, but this is not asserted in the paper text itself. **Confirmed (2026-08-27) directly by the dataset owner** — all participants were adults — and now documented in the dataset's readme.md.

- **confederates = no**
  UNKNOWN from the paper text (strong indirect support) — The word "confederate"/"confederates" does not appear anywhere in the full text. Every description of role assignment treats all group members as genuine participants: "One of the participants was randomly selected to begin in the role of describer, and the other participants were assigned to the role of matchers." This strongly implies no confederates were used, but the paper never explicitly rules them out. **Confirmed (2026-08-27) directly by the dataset owner** — no confederates were used — and now documented in the dataset's readme.md.

- **prior_relationship = no**
  UNKNOWN from the paper text — The paper does not state whether participants knew each other beforehand. Participants were recruited independently through Prolific and organized into ad hoc groups, which is suggestive of strangers, but there is no explicit statement to VERIFY against. **Confirmed (2026-08-27) directly by the dataset owner** — participants had no prior relationship to each other — and now documented in the dataset's readme.md.

### Experiment 1 (5 conditions: expt1_2_rotate, expt1_3_rotate, expt1_4_rotate, expt1_5_rotate, expt1_6_rotate)

- **group_size = 2, 3, 4, 5, 6** (respectively)
  VERIFIED — "Participants were organized into 313 groups of size two to six for a communication game." This range covers all five recorded group sizes.

- **role_constancy = no** (re-verified against strict individual-role-switching definition)
  VERIFIED — "One of the participants was randomly selected to begin in the role of describer, and the other participants were assigned to the role of matchers... The same person was the describer for an entire block, but participants rotated roles between blocks." This is an explicit statement that describer/matcher assignment rotates among the group's members across the six repetition blocks — i.e., a participant who is a matcher in one block becomes the describer in another block (and vice versa). That satisfies the corrected definition directly: at least one (in practice, most/all) participants occupy both roles over the session. Independently confirmed as distinct from partner_constancy: the rotation described here is of individual role assignment within a fixed group, not of group membership.

- **feedback = limited** (re-verified using only correctness-specific language, independent of backchannel/chat quotes)
  VERIFIED — "matchers only received binary feedback about whether they had chosen correctly or not; that is, matchers who made an incorrect choice were not shown the correct answer," and matchers "did not see one another's selections." This is a statement strictly about correctness information (right/wrong signal, without the correct answer) and does not depend on or reuse the "free to use the chat box" backchannel quote below. No group-size dependence is mentioned — this baseline feedback level applies uniformly to all five Experiment 1 group sizes (2–6).

- **backchannel = full**
  VERIFIED — "All participants were free to use the chat box to communicate at any time, but matchers could only make a selection after the describer had sent a message." Matchers (listeners) could freely type and send chat messages during a trial, not just make a final selection — matching "full" backchannel.

### Experiment 2 (3 conditions, all group_size = 6: expt2_no_rotate, expt2_emoji, expt2_full_feedback)

- **group_size = 6** (all three conditions)
  VERIFIED — "Experiment 2 consisted of three different variations on Experiment 1, all conducted in 6-player games."

- **expt2_no_rotate — role_constancy = yes** (re-verified: strict individual-role-switching definition)
  VERIFIED — "In the same describer condition, one person was designated the describer for the entire game, rather than having the describer role rotate." Under the corrected definition, this directly states the describer never changes across the whole game, which (combined with the Experiment 1 baseline description that only one person is describer at a time, "the other participants were assigned to the role of matchers") means no participant occupies both roles — the one designated describer never matches, and the remaining five never describe. role_constancy = yes holds.
  - **feedback = limited**: VERIFIED by exclusion (independently re-checked) — the paper describes this condition purely as a change to *who* holds the describer role for the whole game; no correctness/accuracy-related language appears in its description, so the Experiment 1 baseline correctness feedback (binary right/wrong, no correct answer shown) carries over unchanged. Not to be confused with backchannel — this condition's chat interface (and hence backchannel = full) is likewise unmodified, but that is a separate field from feedback.

- **expt2_full_feedback — feedback = full** (re-verified: correctness-only quote)
  VERIFIED — "In the full feedback condition, all participants were shown what all others had selected as well as the identity of the correct target." This is exclusively about correctness/accuracy disclosure (revealing the correct target's identity to everyone), not about backchannel/chat capability, satisfying the corrected feedback definition directly.
  - **role_constancy = no**: VERIFIED by exclusion (re-verified against strict definition) — only feedback content is described as changing in this condition; the paper gives no indication the describer-role-rotation behavior differs from the Experiment 1 baseline, where "participants rotated roles between blocks" (i.e., individuals do occupy both describer and matcher roles across the session).
  - **backchannel = full**: VERIFIED by exclusion — no chat-interface change described; Experiment 1 baseline carries over (unaffected by this re-check, included here only for completeness).

- **expt2_emoji ("thin" condition) — backchannel = limited** (unaffected by this re-check; included for completeness)
  VERIFIED — "In the thin condition, we altered the chatbox interface for matchers. Instead of a textbox, matchers had four buttons, each of which sent a different emoji to the chat... In addition, for the thin condition, we added notifications that appeared in the chat box marking the time when each player had made a selection."
  - **feedback = limited**: VERIFIED by exclusion (re-verified using correctness-only standard) — the thin-condition description covers only the matcher chat interface (emoji buttons) and selection-time notifications — none of this is correctness/accuracy information about the matcher's choice, so it does not constitute a feedback change. The Experiment 1 baseline (limited, binary correctness feedback) applies.
  - **role_constancy = no**: VERIFIED by exclusion (re-verified against strict definition) — no describer-rotation change is mentioned for this condition, so the Experiment 1 baseline applies, where individuals cycle through both describer and matcher roles across blocks.

### Experiment 3 (4 conditions, factorial group_size {2,6} × interaction {thin,thick}: expt3_2_thin, expt3_2_thick, expt3_6_thin, expt3_6_thick)

- **group_size = 2 and 6**
  VERIFIED — "Experiment 3 crossed the extremes of group size from experiment 1 (two vs. six people) with the extremes of group interactions from Experiment 2 (thick vs. thin interaction structure)."

- **expt3_2_thick / expt3_6_thick — role_constancy = yes, feedback = full** (re-verified: both clauses of this quote map cleanly onto the two corrected definitions, one clause each)
  VERIFIED — "The thick condition combined the two coherency-enhancing variations from Experiment 2: the same participant remained in the describer role throughout, and full feedback was given about the correct answer and what all other players had selected." The first clause ("the same participant remained in the describer role throughout") is a strict individual-role-constancy statement — no participant switches roles — satisfying role_constancy = yes independent of group size (2 or 6). The second clause ("full feedback... about the correct answer") is a strict correctness-disclosure statement, satisfying feedback = full independent of, and not conflated with, backchannel. Both hold for expt3_2_thick and expt3_6_thick alike.
  - **backchannel = full**: VERIFIED by exclusion (unaffected by this re-check) — no chat-interface (emoji) restriction is mentioned, so the full free-text chat from the Experiment 1/2 baseline applies.

- **expt3_2_thin / expt3_6_thin — role_constancy = no, feedback = limited** (re-verified)
  VERIFIED — "The thin channel condition in Experiment 3 was the same as the thin condition in Experiment 2." Tracing back to the Experiment 2 thin-condition description (which only modifies the matcher's chat interface to emoji buttons, with no mention of describer rotation or correctness disclosure), both role_constancy and feedback inherit the Experiment 1 baseline values: role_constancy = no (individuals still rotate between describer/matcher across blocks) and feedback = limited (binary correctness signal only). This holds for both group sizes (2 and 6).
  - **backchannel = limited**: unaffected by this re-check (emoji-only matcher interface, as quoted under expt2_emoji above).

## Summary of flags

- 12 conditions × 10 fields = 120 field-checks total.
- Constant fields (language, modality, partner_constancy, population, confederates, prior_relationship) verified/checked once per experiment-group and applied across all 12 conditions.
- **0 FLAG** — no recorded value conflicts with the paper. In particular, the partner_constancy-vs-role_constancy distinction was checked explicitly and confirmed: "rotate"/"no_rotate" in the condition labels refers to describer-role rotation (role_constancy), while partner/group composition is fixed for the whole session in every condition (partner_constancy = yes throughout), matching our records exactly.
- **role_constancy and feedback re-verified (2026-08-27) against corrected definitions** — role_constancy strictly means whether any individual literally switched between describer and matcher (not partner/audience change); feedback strictly means correctness/accuracy information (not backchannel/talk-back capability). Re-reading the source quotes under these stricter criteria produced **no changes**: all 9 recorded role_constancy = no conditions (expt1_2/3/4/5/6_rotate, expt2_emoji, expt2_full_feedback, expt3_2/6_thin) rest on explicit role-switching language ("participants rotated roles between blocks"), and all 3 role_constancy = yes conditions (expt2_no_rotate, expt3_2/6_thick) rest on explicit no-switching language ("the same participant remained in the describer role throughout" / "the describer role rotate[d]" being denied). All 9 recorded feedback = limited conditions and 3 feedback = full conditions rest on correctness-specific quotes (binary right/wrong vs. "identity of the correct target"), never on the "free to use the chat box" backchannel quote. Also confirmed feedback does not vary by group_size within Experiment 1 (the limited-feedback quote is stated as a general Experiment 1 procedure, not tied to a specific group size). All 12 role_constancy values and all 12 feedback values remain **VERIFIED**, 0 FLAG.
- **population, confederates, prior_relationship**: unstated in the paper text, but all three **confirmed directly by the dataset owner (2026-08-27)** and documented in readme.md — apply uniformly across all 12 conditions.
- All other fields (group_size, role_constancy, feedback, backchannel, language, modality, partner_constancy) are **VERIFIED** for all 12 conditions, several by direct quote and several "by exclusion" (i.e., the paper explicitly frames Experiments 2 and 3 as single- or dual-dimension variations on the Experiment 1 baseline, and only the stated dimensions change).
