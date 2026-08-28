# bangerter2000_reuse
**Source:** machine-translated unpublished report (only source available) —
`import/bangerter2000_reuse/raw_data/machine_translated_study_description.pdf`
(Bangerter & Smolenski, "The reuse of conceptual packages when referring to new
items," Research Report No. 72, Institute of Psychology, University of Basel,
August 2000).

## Citation check

- **Author spelling** — FLAG. `full_cite` spells the second author "Smolenski,"
  matching the document's title page: *"Adrian Bangerter & Carola Smolenski"*
  and the bibliography-style heading *"Adrian Bangerter & Carola Smolenski,
  August 2000."* `short_cite` spells it **"Smokenski"** — this is a typo and
  should be corrected to "Smolenski" to match the source and `full_cite`.
- **Year (2000)** — VERIFIED. Title page: *"August 2000."* Footnote: *"This
  study was conducted between November 1999 and July 2000 at the Institute of
  Psychology at the University of Basel."*
- **Unpublished research report** — VERIFIED. Document is labeled *"Research
  Report No. 72"* with no journal/publisher information, consistent with
  `full_cite`'s "Unpublished research report."
- **German title in `full_cite`** — not independently checked against the
  document (the document is itself an English machine translation and does
  not restate the German title), so this is left as given.

## Condition-level fields

All four condition rows (original_proximal, proximal_original, distal_original,
original_distal) share identical demographic/procedural values in our records;
the document does not report these fields separately by similarity condition
or trial-order condition either, so one set of findings applies to all four
rows.

- **group_size = 2** — VERIFIED. *"The sample consisted of 38 psychology
  students... resulting in 19 teams of two."*

- **language = Swiss German** — VERIFIED (2026-08-27, resolved). `language`
  records the language the interaction was conducted in, not participants'
  individual native-language background — the same distinction the schema
  draws elsewhere (a separate player-level `native_language` field exists
  for that). The task/transcripts are in Swiss German; the quote *"Of the 38
  participants, 34 reported German or Swiss German as their native language.
  Four participants were non-native speakers but were nevertheless able to
  participate without difficulty"* describes participant demographics, not
  the language of the study itself — and per the dataset owner, this is
  consistent with how other datasets in this project record `language` (the
  study/task language, not a per-participant native-language breakdown).
  Original FLAG (below, in Summary) is withdrawn.

- **prior_relationship = no** — FLAG, contradicts the document. Quote: *"A
  follow-up questionnaire revealed that the members of 10 teams did not know
  each other before the experiment. The length of time members of the
  remaining teams had known each other ranged from three weeks to three
  years."* Only 10 of 19 teams were strangers; the other 9 teams had a prior
  relationship (three weeks to three years of acquaintance). Recording
  `prior_relationship = no` for all conditions is not accurate for roughly
  half the sample.

- **partner_constancy = yes** — VERIFIED (by strong implication). The 38
  participants formed *"19 teams of two"* and completed all *"12 such trials
  per team, with the roles of director and matcher being reversed after each
  trial."* Teams are referred to consistently as fixed pairs across all 12
  trials/both phases; there is no mention of repartnering.

- **role_constancy = no** — VERIFIED (re-checked against corrected
  definition: "no" = at least one participant was literally both a describer
  and a matcher within the session). Quote: *"First, it was decided by lot
  which of the two participants in each team would assume the roles of
  Director and Matcher in the first round... There were a total of 12 such
  trials per team, with the roles of director and matcher being reversed
  after each trial."* Every participant explicitly switches between the
  Director (describer) and Matcher role across the 12 trials, so
  role_constancy = no is correct.

- **population = adult** — VERIFIED. *"The sample consisted of 38 psychology
  students from the first and third semesters... The average age was 25.7
  years."*

- **confederates = no** — VERIFIED (by strong implication). *"Participants
  were recruited through mandatory university courses, resulting in 19 teams
  of two. Team composition was determined by the students' self-registration."*
  All participants are described as genuine student participants paired with
  each other; no confederate role is mentioned anywhere in the method section.

- **modality = oral-in-person** — VERIFIED. *"The two participants sat
  opposite each other at a table but could not see each other due to a
  partition"* and *"Participants were allowed to talk to each other as much as
  they liked during the task."* Same room, spoken interaction, visually
  occluded by a partition — consistent with oral-in-person.

- **feedback = none** — UNKNOWN (re-checked against corrected definition:
  strictly whether participants were told, by any means, whether the
  matcher's selection was right or wrong — separate from whether the matcher
  could talk back during a trial, which is backchannel). The document
  describes participants self-reporting when *they* believed the matcher's
  order matched the director's (*"If the participants felt they had met this
  requirement, they were to inform the experimenter"*), but this is the
  participants' own subjective judgment, not correctness feedback delivered
  to them by an experimenter, computer, or scoring signal. No quote anywhere
  in the document (method or results) states that participants were told
  whether a match was actually correct. Left as unknown rather than
  verified. **Confirmed (2026-08-27) as an acknowledged guess, not a silent
  assumption**: the dataset's own readme.md already states "we're guessing
  that there wasn't feedback -- it's possible there was per rep feedback,
  but I don't see mention of it in the report," which matches this
  independent check exactly.

- **backchannel = full** — VERIFIED. *"Participants were allowed to talk to
  each other as much as they liked during the task."* This is corroborated by
  the turn-taking analysis (Section 4.3), which describes trials ending in a
  *"minimal exchange of the 'reference-confirmation' type"* — i.e., the
  matcher responds to the director within a trial, showing genuine two-way
  exchange rather than one-way description.

## Summary of flags

8 VERIFIED, 1 UNKNOWN (feedback), 1 FLAGGED (short_cite author-name typo,
already fixed — see below). `prior_relationship` was fixed to `"mixed"`
(document shows ~9/19 teams had a pre-existing relationship, not uniformly
"no"). `language` = "Swiss German" is confirmed correct as recorded — it's
the study/task language, not a per-participant native-language breakdown
(that's a separate, player-level field); the original FLAG on this field
is withdrawn (2026-08-27, per dataset owner).

role_constancy and feedback were re-checked against corrected field
definitions (role_constancy = individual role-switching only, not partner
change; feedback = correctness feedback only, not backchannel/talk-back).
Neither value changes: role_constancy = no remains VERIFIED (participants
literally alternate between Director/describer and Matcher roles), and
feedback = none remains UNKNOWN (no quote anywhere states participants were
told whether a match was correct or incorrect).
