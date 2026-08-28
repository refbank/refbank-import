# mankewitz2025_function

**Source:** https://escholarship.org/content/qt4tw1c3gn/qt4tw1c3gn.pdf — Mankewitz, J., & Hawkins, R. (2025). "Function shapes form: Compositionality emerges from communicative needs, not environmental structure alone." *Proceedings of the Annual Meeting of the Cognitive Science Society, 47*(0). Full PDF text extracted successfully (8 pages, pp. 5380–5386).

## Citation check

- **Authors:** Matches — "Jess Mankewitz (mankewitz@wisc.edu)... Robert D. Hawkins (rxdh@stanford.edu)" corresponds to recorded "Mankewitz, J., & Hawkins, R."
- **Title:** Matches exactly: "Function shapes form: Compositionality emerges from communicative needs, not environmental structure alone."
- **Journal/volume/year:** Header reads "Proceedings of the Annual Meeting of the Cognitive Science Society, 47(0)," 2025; footer confirms "In D. Barner, N.R. Bramley, A. Ruggeri and C.M. Walker (Eds.), Proceedings of the 47th Annual Conference of the Cognitive Science Society ©2025." Matches recorded full_cite ("Vol. 47").
- **short_cite:** "Mankewitz & Hawkins (2025)" — matches.

**FLAG (participant count, not a schema field but requested cross-check):** The note accompanying this task states "the paper reports 46 native English speakers recruited per an earlier check." This does not match the paper. The Abstract states "pairs of participants (N = 450) communicated..." and the Participants section confirms: "The final sample includes N = 450 dyads distributed evenly across three experimental conditions" (p. 5381, Participants). That is 450 dyads = 900 individual participants (150 dyads / 300 participants per condition), not 46. Whatever prior check produced "46" appears to be incorrect for this paper (possibly confused with an exclusion count, e.g. "n = 52" trial-missing exclusions or "n = 41" low-accuracy exclusions, neither of which is 46 either). This is flagged for correction but does not map onto any of the 10 tracked condition-level fields (group_size = dyad size = 2, which is separate from total N).

## Condition-level fields

All three condition rows ("Across Trial Competitor," "No Competitor," "Within Trial Competitor") are recorded identically across all 10 tracked fields. The paper's Methods and Procedure sections describe the overall task setup (pairing, chat modality, feedback, roles, recruitment) once, before introducing the three conditions, which differ only in how shape components were distributed within vs. across trials — a manipulation of referential competitor structure, not of any of our 10 tracked fields. This is consistent with our schema: the conditions are structurally about stimulus/competitor design, not procedural/demographic setup.

**condition_label**
VERIFIED — Paper's three condition names, as introduced in Procedure (p. 5381–5382) and Figure 3 legend, map directly onto our three rows: "Non-Compositional Baseline" (labeled "No Competitor" in Fig. 3's legend key "Subshape Competitor: No Competitor, Within Trial, Across Trial"), "Within-trial Competitor," and "Across-trial Competitor."

**group_size = 2**
VERIFIED — "Participants were then paired into dyads to play an iterated reference game developed in the experiment developer platform Empirica" (Procedure, p. 5381).

**prior_relationship = no**
UNKNOWN — The paper states participants were "recruited from the online platform Prolific" (Participants, p. 5381) and paired into dyads for the game, which is standard for anonymous online pairing, but no sentence explicitly states participants did not know each other beforehand.

**partner_constancy = yes**
VERIFIED — "Each dyad completed 4 blocks of 16 trials (64 trials total), with director/matcher roles alternating between trials" (Procedure, p. 5381) describes one fixed dyad playing all 64 trials together; there is no mention of re-pairing partners mid-session anywhere in Methods/Procedure.

**role_constancy = no**
VERIFIED (re-checked against corrected definition: "no" = at least one participant was literally both describer and matcher at different points, independent of partner changes) — "Each dyad completed 4 blocks of 16 trials (64 trials total), with director/matcher roles alternating between trials" (Procedure, p. 5381). "Alternating" means each individual participant served as director on some trials and matcher on others within the same fixed dyad, so every participant literally held both roles — matching recorded value "no."

**population = adult**
UNKNOWN — The paper does not state participant ages. Recruitment was via Prolific from the US/UK/Canada (Participants, p. 5381), which conventionally requires adult (18+) registrants, but the paper itself never states this explicitly.

**confederates = no**
VERIFIED — "One participant (the director) saw one shape marked as the target with a black box and was instructed to describe the shape to their partner; their partner (the matcher) selected one of the four shapes" (Procedure, p. 5381) — both dyad members are explicitly described as "participant[s]," not confederates, and the design (450 independently-formed dyads, roles alternating) is inconsistent with confederate use.

**modality = written**
VERIFIED — "both participants viewed an array of 4 abstract shapes on their screen and could freely type to one another in a chat box" (Procedure, p. 5381).

**feedback = full**
VERIFIED (re-checked against corrected definition: correctness feedback only, independent of backchannel) — "Both participants received immediate feedback about whether the correct shape was selected" (Procedure, p. 5381). This quote is strictly about a correctness signal (right/wrong on the matcher's selection), not about the free-form chat channel (which supports `backchannel`, verified separately below), so it stands on its own as correctness-feedback evidence for "full."

**backchannel = full**
VERIFIED — "could freely type to one another in a chat box" (Procedure, p. 5381) indicates unrestricted two-way typed communication between director and matcher during a trial, not a one-directional description-only channel.

**language = English**
VERIFIED — "Native English speakers from the US, UK or Canada were recruited from the online platform Prolific" (Participants, p. 5381).

## Summary of flags

- Verified: 9 of 10 tracked fields (condition_label, group_size, partner_constancy, role_constancy, confederates, modality, feedback, backchannel, language) — identical and confirmed across all three conditions.
- role_constancy and feedback were re-checked against corrected field definitions (role_constancy = individual role-switching only, independent of partner changes; feedback = correctness signal only, independent of backchannel). Both conclusions are unchanged: role_constancy="no" (roles alternate per participant) and feedback="full" (immediate correctness feedback to both participants) remain VERIFIED.
- Unknown: 2 (prior_relationship, population) — not stated in the paper. **Dataset owner confirms (2026-08-27)**: both are reasonable given Prolific recruitment, now documented in readme.md.
- Flagged: 0 of the 10 condition-level fields conflict with the paper. 1 non-schema flag: the "46 native English speakers" figure from an earlier check does not match this paper, which reports N = 450 dyads (900 participants) recruited via Prolific.
- Confirmed as expected: the three competitor conditions differ only in the (non-tracked) within-trial/across-trial component-sharing manipulation; all 10 tracked fields are constant across conditions.
