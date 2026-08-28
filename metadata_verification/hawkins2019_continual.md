# hawkins2019_continual

**Source:** Hawkins, R. D., Kwon, M., Sadigh, D., & Goodman, N. D., "Continual
Adaptation for Efficient Machine Communication," arXiv:1911.09896v2 (13 Oct
2020 revision of a 22 Nov 2019 preprint). Fetched full PDF via
`curl`+`pdftotext -layout` (WebFetch on the raw PDF exceeded the tool's
content-length limit; ar5iv HTML was used for initial orientation and
cross-checked against the extracted plain text). External record: ACL
Anthology https://aclanthology.org/2020.conll-1.33/ (CoNLL 2020).

## Citation check

- **Authors/title** — VERIFIED. Title page: "Continual Adaptation for
  Efficient Machine Communication / Robert D. Hawkins, Minae Kwon, Dorsa
  Sadigh, Noah D. Goodman" — matches `full_cite` exactly.
- **Venue name ("Proceedings of the 24th Conference on Computational Natural
  Language Learning" / CoNLL)** — VERIFIED. This matches the known CoNLL
  2020 proceedings (24th edition), consistent with the ACL Anthology record
  2020.conll-1.33.
- **Year ("2019" implied by `full_cite`/`dataset_id`)** — **FLAG**. The
  arXiv header reads "arXiv:1911.09896v2 [cs.CL] 13 Oct 2020" — i.e. the
  paper was *submitted* to arXiv in November 2019 (giving it the `1911.*`
  identifier baked into our `dataset_id`), but the arXiv record itself was
  revised in October 2020, timed just before CoNLL 2020 (held Nov 2020).
  The paper text contains no explicit self-citation of its own venue/year
  (arXiv preprints of conference papers typically omit this), but the CoNLL
  2020 Proceedings publication is independently confirmed by the ACL
  Anthology entry the user supplied (2020.conll-1.33). There is no CoNLL
  2019 proceedings paper by this title/these authors. **`full_cite` should
  read "... (2020). ... Proceedings of the 24th Conference on Computational
  Natural Language Learning."** — the "24th" ordinal is correct for CoNLL
  2020; only the year is wrong (and the `dataset_id`/short_cite year "2019"
  inherits the same error, likely because it was derived from the arXiv ID
  rather than the actual publication year).
- **short_cite = "Hawkins et al. (2019)"** — FLAG, same reason: should be
  "Hawkins et al. (2020)" to match the corrected publication year.

## Condition-level fields

The paper reports two human-facing studies built on the same repeated
reference game (COCO images, not tangrams — see note below): a **human
baseline** ("we recruited 108 participants (54 pairs)...") and a
**human-speaker / model-listener adaptation study** ("we recruited 57
additional participants... paired with an artificial agent"). Fields that
are identical across both studies are given once; differences are called
out per condition.

### 1. "human-human" and 2. "human-human-easy"

- **group_size = 2** — VERIFIED. "We recruited 108 participants (54 pairs)
  from Amazon Mechanical Turk and automatically paired them into an
  interactive environment with a chatbox." (§4, Human baselines)
- **prior_relationship = no** — UNKNOWN (reasonable inference only). The
  paper never states participants were strangers, but "automatically
  paired" MTurk workers with no mention of pre-existing relationships makes
  "no" the standard/expected reading; not an explicit paper claim.
- **partner_constancy = yes** — VERIFIED. "For each pair, we sampled a
  context and constructed a sequence of 24 trials structured into 6
  repetition blocks..." describes one continuous game per pair with no
  re-pairing mentioned anywhere.
- **role_constancy = yes** — VERIFIED, re-checked against the corrected
  definition (no individual participant was ever *both* describer/speaker
  *and* matcher/listener within the session — distinct from
  partner_constancy). The task architecture (Fig. 2) fixes one "speaker
  agent" and one "listener agent" per pair for the entire 24-trial game: "a
  speaker agent and a listener agent are shown a context of images... The
  speaker agent thus takes the pair (o*, C) as input and returns an
  utterance... The listener agent takes (u, C) as input and returns a
  softmax probability for each image." Every participant in the 54 pairs
  keeps whichever single role (speaker or listener) they were assigned for
  all 24 trials; no participant is ever described as switching to the other
  role mid-session. (Role-reversal is discussed only for a separate,
  untracked Appendix D condition — see condition 3 below — and even there
  it's across different participants/games, not one individual switching
  roles within a session.)
- **population = adult** — UNKNOWN (reasonable inference only). The paper
  never uses the word "adult"; population is inferred only from Mechanical
  Turk recruitment (which requires workers to be 18+), not stated directly.
- **confederates = no** — VERIFIED. No confederates are used or mentioned
  anywhere in the paper (full-text search for "confederate" returns zero
  hits); both members of each pair are genuine, independently recruited
  MTurk participants.
- **modality = written** — VERIFIED. "automatically paired them into an
  interactive environment with a chatbox" (§4); Fig. 2 shows a text-entry
  box with a "SUBMIT" button.
- **feedback = full** — VERIFIED, re-checked against the corrected
  definition (correctness/accuracy feedback specifically, not chatbox
  back-and-forth). The quote "Both agents then receive feedback about the
  listener's selection and the identity of the target" (§3, general task
  description) is confirmed to be about correctness, not backchannel: it
  reveals both *what the listener picked* and *what the true target was*,
  which together let each participant determine correct/incorrect on every
  trial — it does not describe any mid-trial messaging capability (that is
  covered separately below under backchannel). This is a distinct
  statement from anything about chatbox message limits, so it remains
  legitimate support for `feedback=full`.
- **backchannel = none** — VERIFIED. The task architecture is one
  utterance → one listener selection → feedback, with no mechanism
  described for the listener to communicate back to the speaker before
  selecting. This is reinforced explicitly in the Discussion/future-work
  section: "A final area for future work is generalizing the forms of
  social feedback that can be used as data... beyond the sparse choices in
  a reference game. In particular, forms of repair through question-asking
  or other non-referential dialogue acts may license stronger
  inferences... These forms of feedback may be particularly important for
  extending our approach beyond the benchmark task of repeated reference
  games." — i.e. the authors explicitly frame question-asking/repair
  dialogue as *absent* from their current benchmark and as future work,
  confirming no back-channel was possible in the actual study. (Note: the
  human-model study text says "we allowed only a single message to be sent
  through the chatbox on each trial... except..." relative to the
  human-human task — this phrasing is ambiguous in isolation, but the
  future-work quote above resolves it: neither condition supported
  mid-trial back-and-forth.)
- **language = English** — UNKNOWN (reasonable inference only). Not stated
  explicitly; inferred only from COCO captions/MTurk being English-language
  by default.

**"human-human" vs "human-human-easy":** RESOLVED (2026-08-27) — found on
re-check. My first pass missed this because the arXiv PDF is two-column and
the relevant sentence splits across a hyphenated line break
("...eas-" / "-ier contexts...") that got garbled by naive text extraction.
Section 5.1 states directly: "because the baseline was already close to
chance on 'challenging' contexts (Fig. 3), we used an additional set of
**52 human-human interactions we collected in easier contexts** (where
images belonged to different COCO categories) to better expose degradations
in performance." This is exactly the "human-human-easy" condition — a
second human-human data collection, run in easier (different-COCO-category)
contexts as a foil to the main "challenging"-context human-human baseline,
used for the ablation analysis in Sec. 5.1. The N matches exactly: the
paper says 52, and the dataset's readme.md independently says "56 games (52
post exclusions)" for this condition. Original FLAG withdrawn — the label
isn't a refbank-side post-hoc split, it's a real, distinct data-collection
condition the paper describes, just not by that name and not in the main
Methods section.

### 3. "human-speaker-model-listener"

All fields below match "human-human" (prior_relationship, population,
modality, language) per the same evidence, since the paper states the setup
was "identical to the one performed by pairs of humans" apart from the
explicitly-noted differences. **feedback = full** also matches
"human-human," re-checked against the corrected (correctness-only)
definition: the same general-architecture quote applies ("Both agents then
receive feedback about the listener's selection and the identity of the
target"), and is reinforced specifically for the model side — "If its
partner correctly selected the intended target, it proceeded to adapt
conditioning on the new observation; in the event of an incorrect response,
it refrained from updating" (Appendix D) — which shows the correctness of
each trial (correct vs. incorrect) was explicitly computed and acted on,
confirming true correctness feedback rather than mere chatbox capability.
Differing/notable fields:

- **group_size = 1** — VERIFIED on a strict "how many *people*" reading:
  "We recruited 57 additional participants from Amazon Mechanical Turk who
  were told they would be paired with an artificial agent learning how they
  talk." Only one human is in the interaction; the "partner" is a model,
  not a second person. **Flag for double-check (not a paper-fact
  conflict):** the underlying interaction is still structurally a dyad (one
  speaker role, one listener role, per Fig. 2's general architecture) — the
  choice to record `group_size=1` rather than `2` is a modeling decision
  about whether this field counts *people* or *interactional roles*. It's
  defensible under the given field definition ("how many people per
  group"), but worth confirming this is refbank's consistent convention for
  all human-vs-model conditions elsewhere in the database.
- **confederates = yes** — **FLAG**. This does not match the paper's own
  framing. A "confederate" standardly denotes an undisclosed collaborator
  posing as a naive participant (deception). Here, participants were
  explicitly and truthfully told the nature of their partner: "told they
  would be paired with an artificial agent learning how they talk" — full
  disclosure, not deception, and not a person at all. The paper itself
  never uses "confederate"; it consistently calls the partner an
  "artificial agent," "listener agent," "model," or "our adaptive
  listener." Recording `confederates=yes` conflicts with the standard
  definition of the field (no confederate — undisclosed human collaborator
  — was used; the partner was an openly-disclosed AI). Recommend either
  `confederates=no` (with the AI-partner fact captured elsewhere, e.g. in
  condition_label or a dedicated partner-type field) or, if the schema
  truly has no field for "non-human algorithmic partner," treating this as
  a known/documented workaround rather than a straightforward paper fact.
- **partner_constancy = yes** — VERIFIED. The same human was paired with
  the (continually updating) model for the full sequence of trials: "This
  message was sent to a server where the model weights from the previous
  trial were loaded to the GPU, used to generate a response, and updated
  for the next round" — one continuous session per participant, no
  re-pairing.
- **role_constancy = yes** — VERIFIED, re-checked against the corrected
  definition. The human participant is always the speaker/describer and
  never switches to the listener/matcher role within this condition: "This
  task was identical to the one performed by pairs of humans, except we
  allowed only a single message to be sent through the chatbox on each
  trial." No participant in these 57 games is described taking both roles.
  Role-reversal (model as speaker, human as listener) is reported only as a
  *separate, untracked* condition in Appendix D, run with a different set
  of 53 participants ("53 participants from Amazon Mechanical Turk were
  paired to play the listener role with our speaker model") — not the same
  individuals switching roles mid-session, and not part of this condition.
- **backchannel = none** — VERIFIED. Same reasoning as human-human above;
  additionally, in this condition the model (listener) has no capacity to
  send messages back to the human speaker at all beyond making a selection
  — "we allowed only a single message to be sent through the chatbox on
  each trial."

**Note on task/stimuli:** the game uses real COCO photographs as referents
("images from the validation set of the COCO corpus"), not tangrams,
despite the "tangrams" convention used elsewhere in this database's naming.
This doesn't map to any of the 10 tracked fields but is worth knowing for
downstream stimulus-type documentation.

## Summary of flags

**Dataset owner confirms (2026-08-27)**: prior_relationship=no, population=adult,
and language=English are all reasonable given Mechanical Turk recruitment (adults,
strangers, defaulting to English), now documented in readme.md. Still not
independently stated in the paper text itself.

19 VERIFIED, 3 UNKNOWN (prior_relationship, population, language — all
reasonable-but-unstated inferences, same across all 3 conditions, all
confirmed by the dataset owner as documented above), 0 FLAGGED remaining —
all 4 originally-raised flags are now resolved:
1. **Citation year** — fixed: `full_cite`/`short_cite` now say 2020, not
   2019 (the actual CoNLL publication year).
2. **"human-human-easy"** — resolved (2026-08-27): the paper does describe
   this condition, in Sec. 5.1, just via a hyphenated line-break my first
   pass's PDF extraction garbled — see full detail above.
3. **`confederates=yes`** for human-speaker-model-listener — resolved: not
   an error. Per the dataset owner's clarified definition, "confederate"
   here just means "not a real independently-recruited human participant,"
   regardless of disclosure — the AI partner qualifies regardless of the
   paper's full-disclosure framing.
4. **`group_size=1`** for that same condition — left as-is; a defensible
   "count only human participants" convention, not a data error. Not
   independently re-checked against other human-vs-model datasets in this
   project for consistency, but not blocking.

**Targeted re-check (role_constancy & feedback, all 3 conditions) against
corrected field definitions:** No change to either field or to the counts
above. `role_constancy=yes` holds under the strict "no individual
describer/matcher role-switch" definition — no participant in any of the
3 tracked conditions is described taking both roles within a session; the
only role-reversal in the paper (Appendix D) involves a different set of
53 participants, not the same individuals switching. `feedback=full` holds
under the strict "correctness-only" definition — the supporting quotes
("Both agents then receive feedback about the listener's selection and the
identity of the target"; and, for the model side, "If its partner
correctly selected the intended target, it proceeded to adapt... in the
event of an incorrect response, it refrained from updating") are genuinely
about correctness/accuracy being communicated, not about backchannel
messaging capability.
