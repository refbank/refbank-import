# Conditions-table metadata verification — summary

One file per dataset in this folder gives the field-by-field check (VERIFIED /
UNKNOWN / FLAG) with quotes from the source. This file is just the index of
what needs your attention — see the per-dataset file for full quotes/context.

**Update:** `role_constancy` and `feedback` were re-checked across every
dataset with corrected definitions — `role_constancy` is strictly about
whether an individual ever switched between describer and matcher (not about
partner/audience turnover), and `feedback` is strictly about correctness
feedback from the experimenter/computer (not about backchannel/talk-back
ability). This changed the branigan2016 findings back to "no flag."

**Update 2:** `dahan2023_collaboration` was split (2026-08-27) into two
separate datasets — `dahan2023_collaboration` (2023 college-student sample)
and `dahan2025_hedging` (2025 Philadelphia community sample) — each with its
own `import.R`, `raw_data`, and `readme.md`. This also resolved the citation
duplication flag automatically, since each dataset now has its own correct
`full_cite`/`short_cite`.

**Update 3 (fixes applied):**
- Added `"mixed"` as a valid `prior_relationship` value (schema.txt,
  validate.R, merge_and_export.Rmd) for conditions whose participants are a
  known blend of acquainted/unacquainted pairs that can't be split further.
  Applied to `bangerter2000_reuse` (~9/19 teams had a prior relationship,
  ~10/19 didn't) and `hawkins2026_fmri` (per dataset owner).
- `hawkins2020_characterizing`'s "unconstrained" condition: `feedback`
  changed "full" → "limited" (confirmed).
- `ji2025_adhoc`'s "expt3" condition: `feedback` changed "limited" → "full"
  (confirmed).
- `leung2024_scaffolding`: `short_cite` changed to "Leung et al. (2025)" to
  match `full_cite`'s already-final citation info (2025, *Child Development*
  96(2):546–561).
- `wang2025_lvlms`: citation stays as Wang et al. (2025) — that's the
  published, open-data source, confirmed by the dataset owner; not changing
  despite the human data's 2001 Lockridge & Brennan origin.
- `hawkins2026_fmri`: `prior_relationship` changed "yes" → "mixed" (same
  rationale as bangerter2000_reuse); `modality` changed "oral-in-person" →
  "oral-remote". `github.com/hawkrobe/tangrams-fmri` (the `task` repo)
  turned out to exist but be private — the earlier 404 wasn't a rename, it
  was a permissions error. The dataset owner authenticated `gh` with their
  own GitHub account, which gave direct access to the actual task
  application source code (Meteor/Empirica app: `client/`, `server/`,
  `config.yaml`). This let two more fields move from UNKNOWN to VERIFIED:
  - `modality=oral-remote`: confirmed 4 ways — separate-scanner run labels
    in the public `tangrams-fmri-data` repo's `CHANGES.md` ("prisma-side"
    vs. "skyra-side"); no chat/messaging component anywhere in the client
    code (communication must be spoken, not written); each player has a
    distinct `roomId`/`scannerName` in the code; the listener responds via
    scanner-safe button-box keypresses, not a shared input device.
  - `feedback=full`: confirmed directly — the task UI has a dedicated
    "feedback" stage that shows "Correct!" or "Ooops, this was the real
    target!" after every round.
  - `role_constancy=no`: also newly confirmed — each player's role is
    looked up fresh per round from a list indexed by round number
    (`player.get('roleList')[round.index]`), i.e. describer/listener can
    and does change across rounds within a session.
  See `hawkins2026_fmri.md` for full detail. `backchannel` remains UNKNOWN
  — the physical intercom's two-way-ness isn't something the web app code
  controls or logs.

**Update 4 (more resolutions, 2026-08-27):**
- `beatty-martinez2026_tangrams`: `role_constancy` moved from UNKNOWN to
  VERIFIED by checking the harmonized `trials.csv` directly (the paper
  itself never states this) — every one of the 45 games shows a literal
  per-player describer/matcher swap partway through (e.g. game 1: player 2
  describes trials 1–12, then player 1 describes 13+). No data change
  needed, the recorded "no" was already correct.
- `boegels2025_power`: `language` moved from UNKNOWN to VERIFIED by reading
  the companion CABB methods paper (Eijk et al., 2022, *NeuroImage*) that
  Bögels et al.'s 42-pair sample is drawn from — it states directly
  "communicative interactions in **Dutch**" and "142 right-handed, native
  **Dutch** speakers." `feedback` was also checked against this companion
  paper and remains genuinely UNKNOWN — its only "feedback" mention is
  about conversational/backchannel freedom, not correctness feedback, and
  neither paper describes a correctness-feedback mechanism during the task.
- `bangerter2000_reuse`: `feedback=none` confirmed to already be documented
  in the dataset's own readme.md as an acknowledged guess ("we're guessing
  there wasn't feedback... I don't see mention of it in the report") — no
  change needed, just confirmed the readme already says this.
- `bangerter2020_lexical`: expt3's `prior_relationship=no` and
  `feedback=none` (previously UNKNOWN/inferred-by-continuity) confirmed by
  the dataset owner as intentional assumptions, now documented explicitly
  in readme.md.
- `boyce2024_interaction`: `population=adult`, `confederates=no`,
  `prior_relationship=no` (previously UNKNOWN for all 12 conditions, not
  stated in the Prolific-recruitment methods text) confirmed directly by
  the dataset owner, now documented in readme.md.
- `boyce2026_preschoolers`: `language=English` (previously UNKNOWN, never
  stated in the manuscript) confirmed directly by the dataset owner, now
  documented in readme.md.
- `branigan2016_doyouknow`: `prior_relationship=yes` (children recruited
  from the same junior school, assumed to know each other) and
  `language=English` (UK study site, matches transcripts) both confirmed
  directly by the dataset owner, now documented in readme.md.
- `dahan2023_collaboration` and `dahan2025_hedging`: `language=English`
  confirmed directly by the dataset owner (US-based studies, audio/
  transcripts are English) — readme.md language in both projects
  strengthened from "presumably" to a confirmed fact.

**Update 5 (final round of owner confirmations, 2026-08-27):** every remaining
field in the "genuinely UNKNOWN" table below has now been confirmed by the
dataset owner as an accepted assumption and documented in the relevant
project's readme.md — `boegels2025_power` (feedback), `dale2011_tangram`
(language, prior_relationship, feedback), `hawkins2019_continual`
(prior_relationship, population, language), `hawkins2021_respect`
(prior_relationship, population, confederates), `hawkins2023_frompartners`
(prior_relationship, population, confederates, language), `hawkins2026_fmri`
(group_size, language, population, partner_constancy, confederates,
backchannel), `ji2025_adhoc` (population), `mankewitz2025_function`
(prior_relationship, population), `wang2025_lvlms` (prior_relationship,
confederates, feedback), `yoon2019_audience` (feedback). Also, on review,
`yoon2019_audience`'s Experiment 3 `role_constancy` FLAG from the first pass
was itself an error, corrected the same way as the Checked/confirmed-NOT-
an-error item below (per-`game_id` role evaluation, not whole-session).
None of these needed a data value change — every recorded value already
matched what the owner confirmed. What changed is that every "we assumed
this because it's typical/standard/not mentioned" judgment call is now
written down in the dataset's own readme.md instead of living only in this
audit, so a future reader hits the same reasoning without re-deriving it.

## Checked and confirmed NOT an error

- **yoon2019_audience** — `role_constancy="yes"` for the 4 Experiment 3
  conditions is correct as recorded. The paper does describe a participant
  being reassigned from Matcher to Director partway through the session, but
  refbank codes each four-block half as its own separate `game_id` with the
  same participants — so within any single `game_id`, no one is both
  describer and matcher. (Confirmed by the dataset owner.)
- **hawkins2019_continual** — `confederates="yes"` for the
  human-speaker/model-listener condition is correct as recorded.
  "Confederate" in this schema just means "one participant is not a real
  independently-recruited human participant," regardless of whether
  participants were told the truth about their partner — so the openly-
  disclosed AI partner still counts as a confederate. (Confirmed by the
  dataset owner; this reverses the FLAG raised in the first pass.)
- **bangerter2000_reuse** — `language="Swiss German"` is correct as
  recorded. This field records the study/task language, not a per-
  participant native-language breakdown (that's the separate, player-level
  `native_language` field) — so the source's mixed native-language data
  (34/38 German or Swiss German, 4 non-native) doesn't conflict with it.
  (Confirmed by the dataset owner; this reverses the FLAG raised in the
  first pass.)
- **hawkins2019_continual** — the "human-human-easy" condition label does
  correspond to a real condition in the paper — Sec. 5.1 describes "an
  additional set of 52 human-human interactions we collected in easier
  contexts (where images belonged to different COCO categories)." My first
  pass missed this because the two-column PDF's text extraction garbled a
  hyphenated line break ("eas-" / "-ier"). The N matches exactly (paper: 52;
  readme.md: "56 games, 52 post exclusions"). Not a refbank-side post-hoc
  split — a real, distinct data-collection condition, just not surfaced in
  the main Methods section. (Reverses the FLAG raised in the first pass.)

## Still open

None — both remaining items from the previous round are resolved above.

## Full list of fields not stated in any paper/source text (owner-confirmed assumptions, all now documented in readme.md)

Every field below is absent from the paper/manuscript/task-code text itself
— none of these are independently citable — but every one has been reviewed
and confirmed by the dataset owner as an accepted assumption, and that
reasoning is now written into each project's own readme.md rather than
living only in this audit.

| Dataset | Fields (assumption) |
|---|---|
| bangerter2000_reuse | feedback — acknowledged guess |
| bangerter2020_lexical | prior_relationship, feedback (expt3 only) — continuity from expt1/2 |
| boegels2025_power | feedback — absence of any mention (in either the paper or its CABB companion) taken as "none" |
| dale2011_tangram | language, prior_relationship, feedback — English-speaking university setting; no mention ⇒ no relationship / no feedback |
| hawkins2019_continual | prior_relationship, population, language (all 3 conditions) — MTurk ⇒ adults, strangers, English |
| hawkins2021_respect | prior_relationship, population, confederates — Prolific ⇒ adults, strangers, no confederates |
| hawkins2023_frompartners | prior_relationship, population, confederates, language — AMT ⇒ adults, strangers, no confederates; English from transcripts |
| hawkins2026_fmri | group_size (counted from task code), language (transcripts), population, partner_constancy, confederates, backchannel — all confirmed directly by the dataset owner |
| ji2025_adhoc | population — Prolific ⇒ adults |
| mankewitz2025_function | prior_relationship, population — Prolific ⇒ adults, strangers |
| wang2025_lvlms | prior_relationship, confederates, feedback — no mention ⇒ strangers, no confederates, no feedback |
| yoon2019_audience | feedback (all 13 conditions) — no mention ⇒ no feedback |

Also resolved earlier via Update 4 (confirmed by raw data, a companion
paper, or the dataset owner, and — where applicable — now documented in
readme.md): `beatty-martinez2026_tangrams` (role_constancy),
`boegels2025_power` (language), `boyce2024_interaction` (population,
confederates, prior_relationship), `boyce2026_preschoolers` (language),
`branigan2016_doyouknow` (prior_relationship, language),
`dahan2023_collaboration` and `dahan2025_hedging` (language).

Datasets not listed at all (dale2011's other fields, etc. aside) had every
field either VERIFIED from source or FLAGged — see each file for exact
per-condition breakdowns where fields apply.

**By field, ranked by how often it needed an owner-confirmed assumption:**
- `prior_relationship` — 8 datasets: bangerter2020_lexical (expt3),
  dale2011_tangram, hawkins2019_continual, hawkins2021_respect,
  hawkins2023_frompartners, mankewitz2025_function, wang2025_lvlms,
  hawkins2026_fmri (already "mixed", not "no")
- `feedback` — 6 datasets: bangerter2000_reuse, bangerter2020_lexical
  (expt3), boegels2025_power, dale2011_tangram, wang2025_lvlms,
  yoon2019_audience
- `population` — 6 datasets: hawkins2019_continual, hawkins2021_respect,
  hawkins2023_frompartners, ji2025_adhoc, mankewitz2025_function,
  hawkins2026_fmri
- `confederates` — 4 datasets: hawkins2021_respect, hawkins2023_frompartners,
  wang2025_lvlms, hawkins2026_fmri
- `language` — 4 datasets: dale2011_tangram, hawkins2019_continual,
  hawkins2023_frompartners, hawkins2026_fmri
- `group_size`, `partner_constancy`, `backchannel` — 1 dataset each
  (hawkins2026_fmri only)
- `role_constancy` — 0 datasets remaining (was beatty-martinez2026_tangrams
  and hawkins2026_fmri; both resolved with direct evidence, not assumption)

None of these needed a data value change — every recorded value already
matched what the owner confirmed. This audit's actual output at this point
is documentation: every non-obvious assumption is now written into the
dataset it belongs to.

## Double-check list: oral-in-person vs. oral-remote

12 datasets use an oral modality value. All were checked against source
quotes describing the physical setup:

| Dataset | Value | Basis |
|---|---|---|
| bangerter2000_reuse | oral-in-person | Same table, partition blocking sight only — VERIFIED, unambiguous |
| bangerter2020_lexical | oral-in-person | Same room, partition, audio-recorded verbatim — VERIFIED; explicitly checked that a visual partition doesn't change the classification |
| boegels2025_power | oral-in-person | Face-to-face, audio+video recorded — VERIFIED |
| boyce2026_preschoolers | oral-in-person | Same room — VERIFIED |
| branigan2016_doyouknow | oral-in-person | Same room — VERIFIED |
| dahan2023_collaboration | oral-in-person | "sat across from each other at a table with an opaque barrier" — VERIFIED |
| dahan2025_hedging | oral-in-person | Same procedure as 2023 — VERIFIED |
| dale2011_tangram | oral-remote | "Two eye tracking labs on different floors... communicated through hands-free headsets" — VERIFIED, genuinely remote |
| hawkins2026_fmri | oral-remote (fixed from oral-in-person) | RESOLVED — confirmed via `tangrams-fmri-data` repo's `CHANGES.md`: "prisma-side" vs. "skyra-side" run labels show dyad members were scanned in two separate MRI scanners simultaneously, not co-present. |
| leung2024_scaffolding | oral-in-person | Same lab, divider, verbal-only instruction — VERIFIED |
| wang2025_lvlms | oral-remote | "Partners sat in separate rooms and communicated via an audio channel" — VERIFIED, genuinely remote |
| yoon2019_audience | oral-in-person | VERIFIED |

**Bottom line:** every oral-modality assignment is now directly supported by
evidence about the physical setup, and the in-person/remote line was drawn
consistently (partition-in-same-room = in-person; different rooms/floors/
scanners + audio-only link = remote). `hawkins2026_fmri` was the one
genuinely questionable case and is now resolved.

## Already fixed (typos/citations, done in the first pass)

- bangerter2000_reuse: `short_cite` "Smokenski" → "Smolenski"
- beatty-martinez2026_tangrams: `full_cite` author name order fixed
- boegels2025_power: `full_cite` article number "10637" → "106370"
- hawkins2019_continual: `full_cite`/`short_cite` year 2019 → 2020

## Clean (no outstanding flags — every field either VERIFIED from source or confirmed by the dataset owner and documented) — all 20 datasets (19 original + dahan2025_hedging split out)

- bangerter2000_reuse (language flag resolved — it's the task language, not native-language demographics)
- bangerter2020_lexical
- beatty-martinez2026_tangrams
- boegels2025_power
- boyce2024_interaction
- boyce2026_preschoolers
- branigan2016_doyouknow (flags from the first pass resolved on recheck)
- dahan2023_collaboration
- dahan2025_hedging
- dale2011_tangram
- hawkins2019_continual (confederates flag and "human-human-easy" condition-identity question both resolved)
- hawkins2020_characterizing (after the feedback fix)
- hawkins2021_respect
- hawkins2023_frompartners
- hawkins2026_fmri
- ji2025_adhoc (after the feedback fix)
- leung2024_scaffolding (after the short_cite fix)
- mankewitz2025_function
- wang2025_lvlms (citation intentionally left as Wang et al. 2025)
- yoon2019_audience (role_constancy pattern confirmed intentional, not an error)
