# hawkins2023_frompartners

**Source:** Hawkins, R. D., Franke, M., Frank, M. C., Goldberg, A. E., Smith, K., Griffiths, T. L., & Goodman, N. D. "From partners to populations: A hierarchical Bayesian account of coordination and convention." Fetched as arXiv preprint PDF (arXiv:2104.05857v3, 2 Dec 2021), corresponding to the eventual *Psychological Review* (2023) publication. Relevant material is the "Behavioral experiment" reported under "Phenomenon #2: Conventions gradually generalize to new partners in community" (pp. 17-19), which is a distinct empirical study from the network paradigm used in hawkins2021_respect.

## Citation check

- **Authors/title** — VERIFIED. PDF title page (p. 1) reads: "From partners to populations: A hierarchical Bayesian account of coordination and convention" by "Robert D. Hawkins, Michael Franke, Michael C. Frank, Adele E. Goldberg, Kenny Smith, Thomas L. Griffiths, and Noah D. Goodman" — matches `full_cite` and `short_cite` author list and order exactly.
- **Journal/volume/issue/page (Psychological Review, 130(4), 977)** — UNKNOWN. The fetched arXiv preprint has no journal pagination information; this could not be checked against the source used here.

## Condition-level fields

- **group_size = 4** — VERIFIED. "Each participant was randomly assigned to one of 23 fully-connected networks with three other participants as their neighbors" (p. 17-18, Behavioral experiment/Participants). Figure 6 caption (p. 16): "participants were placed in fully-connected networks of 4."

- **prior_relationship = no** — UNKNOWN. The paper states participants were recruited from Amazon Mechanical Turk and "randomly assigned to one of 23 fully-connected networks" (p. 17-18), which strongly suggests strangers, but the paper never explicitly states whether participants knew each other beforehand.

- **partner_constancy = no** — VERIFIED. "Partner pairings were determined by a round-robin schedule ... After completing sixteen trials with one partner, participants were introduced to their next partner and asked to play the game again. This process repeated until each participant had partnered with all three neighbors." (p. 18, Stimuli and procedure).

- **role_constancy = no** — VERIFIED. "Participants were randomly assigned to speaker and listener roles and swapped roles on each block." (p. 18, Stimuli and procedure). This is a direct statement that individual participants were literally both speaker (describer) and listener (matcher) at different points in the session, satisfying the strict individual-role-switching definition (distinct from partner_constancy, which concerns partner identity, not role).

- **population = adult** — UNKNOWN. Not explicitly stated for this experiment's participant pool. The paper recruited "92 participants from Amazon Mechanical Turk" (p. 17-18) with no age/population statement given; other parts of the paper discuss "adult communication" only as general theoretical framing (Discussion, p. 19-20), not as a description of this sample.

- **confederates = no** — UNKNOWN. No mention of confederates anywhere in the paper; the Participants description implies all 92 participants were genuine dyad members drawn from MTurk, but there is no explicit statement ruling out confederates.

- **modality = written** — VERIFIED. "They were instructed to use a chatbox to communicate the identity of this object to their partner, the listener. The two participants could engage freely in dialogue through the chatbox" (p. 18, Stimuli and procedure).

- **feedback = full** — VERIFIED. "Finally, both participants in a pair were given full feedback on each trial about their partner's choice and received bonus payment for each correct response." (p. 18, Stimuli and procedure). This quote is specifically about correctness feedback (choice outcome + bonus tied to correct responses), not about backchannel ability to talk during the trial (which is covered separately by the "could engage freely in dialogue" quote under `backchannel`).

- **backchannel = full** — VERIFIED. "The two participants could engage freely in dialogue through the chatbox but the listener must ultimately make a selection from the array." (p. 18, Stimuli and procedure) — indicates the listener could freely type back to the speaker during a trial, not just make a forced-choice response.

- **language = English** — UNKNOWN. Not explicitly stated for this experiment. The paper is written in English and stimuli/example utterances throughout are English, but there is no explicit statement of participants' language for this study.

## Summary of flags

No FLAGs. 6 fields VERIFIED (group_size, partner_constancy, role_constancy, modality, feedback, backchannel); 4 fields UNKNOWN (prior_relationship, population, confederates, language) — none contradicted by the source, but the paper does not state them explicitly for this experiment. **Dataset owner confirms (2026-08-27)**: adult, no prior relationship, no confederates (given AMT recruitment), and English (given the transcripts) are all reasonable, now documented in readme.md. `role_constancy=no` and `feedback=full` were re-checked against the strict, non-conflated definitions (individual describer/matcher role-switching only, and correctness feedback only) and both remain VERIFIED with quotes distinct from the partner_constancy and backchannel fields. This confirms the study is a distinct, separate network experiment (92 participants, 23 networks of 4, tangram stimuli from Clark & Wilkes-Gibbs 1986) rather than a reuse of the hawkins2021_respect dataset, though it uses a similar network design (group_size=4, rotating partners).
