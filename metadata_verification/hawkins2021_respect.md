# hawkins2021_respect

**Source:** Hawkins, R. D., Liu, I., Goldberg, A. E., & Griffiths, T. L. (2021). Respect the code: Speakers expect novel conventions to generalize within but not across social group boundaries. *Proceedings of the Annual Meeting of the Cognitive Science Society, 43*(43). Fetched via https://par.nsf.gov/servlets/purl/10285683 (full text extracted from PDF).

Note: Only **Experiment 1** (the networked communication task) is relevant to the condition-level metadata being checked; Experiment 2 is a separate downstream comprehension survey with naive raters, not a reference-communication-game condition.

## Citation check

- **full_cite** — VERIFIED. Matches paper: authors "Hawkins, R. D., Liu, I., Goldberg, A. E., Griffiths, T. L."; title "Respect the code: Speakers expect novel conventions to generalize within but not across social group boundaries"; venue "Proceedings of the Annual Meeting of the Cognitive Science Society," Vol. 43, No. 43, 2021 (title page / p. 2232).
- **short_cite** — VERIFIED. "Hawkins et al. (2021)" is the correct APA short form for a 4-author paper.

## Condition-level fields

Condition: `pairs-network` (group_size=4, prior_relationship=no, partner_constancy=no, role_constancy=no, population=adult, confederates=no, modality=written, feedback=full, backchannel=full, language=English)

- **group_size = 4** — VERIFIED. "We recruited 272 participants from Prolific and connected them in groups of four using a reactive web app built with Empirica" and "we were left with complete data from 33 groups, consisting of 132 unique participants" (33×4=132) (p. 2234, Methods/Participants). The "4" is the number of people in each interacting network/community, not a dyad count: "Partner pairings were determined by a round-robin schedule, such that every participant had an extended interaction with each of their neighbors in a private room" (p. 2234) — a network of 4 gives each participant exactly 3 neighbors/partners, matching "x3 partners" in Fig. 2B. So group_size=4 correctly refers to the network/community size, confirmed.

- **language = English** — VERIFIED. "All participants were pre-screened as fluent (but not necessarily 'native') English speakers" (p. 2234, Methods/Participants).

- **prior_relationship = no** — UNKNOWN. The paper never states whether network-mates knew each other beforehand. Participants were recruited from Prolific and algorithmically "connected... in groups of four," which makes prior acquaintance unlikely, but this is not asserted in the text.

- **partner_constancy = no** — VERIFIED. "Partner pairings were determined by a round-robin schedule, such that every participant had an extended interaction with each of their neighbors in a private room" ... "After completing sixteen trials with one partner, participants were introduced to their next partner" (p. 2234). Each participant interacted with 3 different partners in sequence within the network, so partners did not stay constant.

- **role_constancy = no** — VERIFIED (under corrected definition: individuals literally switched between describer and matcher roles). "Participants swapped speaker and listener roles at the beginning of each block" (p. 2234), where each partner-pairing comprised 4 blocks. This is a direct statement that the same participant served as both speaker (describer) and listener (matcher) at different points, not merely that partners changed (partner_constancy, separately recorded as "no," covers that).

- **population = adult** — UNKNOWN. The paper states participants were "recruited... from Prolific" (p. 2234) but never explicitly characterizes them as adults. (Prolific's platform policy requires participants to be 18+, but this is external knowledge, not a statement in the paper.)

- **confederates = no** — UNKNOWN. The paper describes only real, mutually-paired Prolific participants (e.g., both members of a dyad "were given full feedback and received bonus payment," and roles/partners rotated symmetrically among the 4 real network members), which is consistent with no confederates, but the paper never uses the word "confederate" or otherwise explicitly rules them out.

- **modality = written** — VERIFIED. "They were instructed to use a chatbox to communicate the identity of this object to their partner, the listener. The two participants were able to communicate freely through the chatbox..." (p. 2234).

- **feedback = full** — VERIFIED (under corrected definition: correctness feedback specifically, distinct from backchannel/chat access). "After a selection was made, both participants in a dyad were given full feedback and received bonus payment for correct responses" (p. 2234). The phrase "full feedback" is paired directly with "bonus payment for correct responses," confirming it refers to being told whether the matcher's selection was correct, not to the free-form chatbox communication (which is separately recorded as backchannel=full, supported by a different quote about communicating "freely through the chatbox").

- **backchannel = full** — VERIFIED. "The two participants were able to communicate freely through the chatbox until the listener decided to select one of the objects" (p. 2234), indicating unrestricted two-way chat during a trial (not a one-way, turn-limited channel).

## Summary of flags

**Dataset owner confirms (2026-08-27)**: prior_relationship=no, population=adult,
confederates=no are all reasonable given Prolific recruitment, now documented in
readme.md. Still not independently stated in the paper text itself.

Condition-level fields (10 total): **7 VERIFIED** (group_size, language, partner_constancy, role_constancy, modality, feedback, backchannel), **3 UNKNOWN** (prior_relationship, population, confederates), **0 FLAG**. Citation fields (full_cite, short_cite): both VERIFIED. No conflicts found; the "network of 4, rotating partners" interpretation of group_size is confirmed correct by the paper. **role_constancy** and **feedback** were re-checked against corrected, stricter definitions (role_constancy = literal describer/matcher role-switching by individuals, not partner turnover; feedback = correctness feedback only, not backchannel/chat access) and both remain **VERIFIED**: "Participants swapped speaker and listener roles at the beginning of each block" directly shows individual role-switching, and "full feedback ... for correct responses" is explicitly tied to correctness, separate from the free chatbox communication recorded under backchannel.
