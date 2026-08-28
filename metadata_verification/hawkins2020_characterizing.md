# hawkins2020_characterizing

**Source:** Hawkins, Frank, & Goodman, "Characterizing the dynamics of
learning in repeated reference games," arXiv:1912.07199v2 (14 Apr 2020
preprint of Hawkins, Frank, & Goodman, 2020, *Cognitive Science*, 44(6),
e12845). Fetched via `pdftotext -layout` on the arXiv PDF (WebFetch could
not parse the PDF binary directly).

## Citation check

- **Authors** — VERIFIED. Title page: "Characterizing the dynamics of
  learning in repeated reference games / Robert D. Hawkins, Michael C.
  Frank, Noah D. Goodman" — matches `full_cite` author order and initials.
- **Title** — VERIFIED. Exact match to `full_cite` title.
- **Year / volume / issue / page (44(6), e12845)** — UNKNOWN. The arXiv
  preprint (v2, 14 Apr 2020) carries no journal volume/issue/page metadata;
  this information can't be checked against the fetched source. (The 2020
  *Cognitive Science* publication is well known externally but not
  confirmable from this document.)
- **short_cite** = "Hawkins et al. (2020)" — VERIFIED as consistent with
  the three-author full_cite (3 authors → "et al." is correct APA style)
  and the year, contingent on the unverified-but-plausible 2020 pub date
  above.

## Condition-level fields

The paper describes **two task variants**, both run in the same study:
a "relatively unconstrained free-matching version" and "a more tightly
controlled cued version" (Methods, p.5–6: *"We developed two variants of
the repeated reference task used in classic work by Clark & Wilkes-Gibbs
(1986): a relatively unconstrained free-matching version ... and a more
tightly controlled cued version."*). This maps directly onto our
`condition_label` values "unconstrained" (= free-matching) and "cued."

**Important scope note:** the paper states plainly that "we restrict our
analyses to the cued version throughout the paper as a cleaner
confirmatory sample" (p.5) — the free-matching/unconstrained version was
an unregistered exploratory pilot whose corpus is released but not
analyzed. All quotes below distinguish which version they describe.

### cued

- **group_size = 2** — VERIFIED. "pairs of participants had to coordinate"
  (Abstract); "480 participants ... were recruited ... and paired into
  dyads" (§2.1, p.6).
- **language = English** — VERIFIED (indirect). Exclusion criteria mention
  "games where participants reported a native language different from
  English (2 in free matching and 3 in cued)" were excluded (§2.3, p.7),
  establishing English as the study's working language.
- **prior_relationship = no** — VERIFIED (indirect). Participants were
  MTurk workers "automatically paired into virtual rooms" (§2.2, p.6); the
  Discussion refers to "a relatively large number of arbitrary dyads
  within a convenience population" (p.29-30). No prior acquaintance is
  implied or possible under this recruitment method.
- **partner_constancy = yes** — VERIFIED (indirect). No re-pairing is
  described anywhere in Methods; participants are paired once into a
  dyad and play the full 6-block/72-trial sequence together. Discussion:
  "It will be important to determine how the ad hoc meanings formed in
  one novel context generalize to other contexts with the same partner"
  (p.29) — treating "same partner" as the norm within a single game.
- **population = adult** — VERIFIED (indirect). "480 participants ... were
  recruited from Amazon's Mechanical Turk" (§2.1) — MTurk requires adult
  workers; the paper never mentions children.
- **role_constancy = yes** — VERIFIED (indirect). Re-checked against the
  corrected definition (no individual switches between describer/matcher
  role, independent of whether the partner changes). "participants were
  randomly assigned the role of either 'director' or 'matcher'" once, at
  the start (§2.2, p.6); all subsequent longitudinal analyses track "the
  director" and "the matcher" as fixed roles across all 6 blocks (e.g.
  "the mean number of words used by directors for each tangram decreases"
  over repetitions, §3). No participant is ever described as switching
  from director to matcher or vice versa anywhere in the paper.
- **confederates = no** — VERIFIED (indirect). Both roles are filled by
  recruited MTurk participants paired with each other; no confederate is
  mentioned anywhere in the design.
- **modality = written** — VERIFIED. "virtual rooms containing a chat box
  ... The chat box was a standard modern messaging interface" (§2.2, p.6).
- **feedback = full** — VERIFIED (re-checked against corrected
  definition: correctness signal only, distinct from backchannel).
  "participants were given full, immediate feedback: the director saw
  which tangram their partner clicked, and the matcher saw the intended
  tangram" (§2.2, p.7-8), given after every one of the 72 trials — this
  reveals the correct target and the actual selection each trial, i.e.
  full correctness information, not merely conversational ability.
- **backchannel = full** — VERIFIED. "Use of the chat box was completely
  unrestricted in both versions of the task: both participants could
  freely use the chat box to communicate at any time and there was no
  limit on the number or length of messages" (§2.2, p.6); matchers "were
  not restricted in their use of the chat box" (§2.2, p.7) even though
  they could not click their answer until after a message was sent.

### unconstrained (free-matching)

Same VERIFIED status and quotes as "cued" for group_size, language,
prior_relationship, partner_constancy, population, role_constancy,
confederates, modality, and backchannel — none of those fields differ
between the two versions in the paper's description.

- **feedback = full** — **FLAG** (confirmed under corrected definition —
  this reasoning is about correctness signals, not backchannel/talking
  ability; re-checked and the distinction holds). The paper describes
  free-matching feedback differently from cued: "When the players were
  satisfied that their boards matched, the matcher clicked a 'submit'
  button that gave players **batched feedback on their score (out of
  12)**" (§2.2, p.6-7). This is an aggregate round-level correctness
  score (X/12 correct, revealed only after all 12 tangrams on the board
  are matched), not per-trial correctness feedback — unlike the cued
  version's per-trial "the director saw which tangram their partner
  clicked, and the matcher saw the intended tangram," which tells each
  participant, every trial, whether that specific selection was right or
  wrong. A batched aggregate score without trial-by-trial correctness
  disclosure reads as **limited** rather than **full** feedback under our
  field definition. Recommend changing `feedback` to "limited" for the
  "unconstrained" condition row, or at minimum flagging the
  cued/unconstrained rows as no longer identical on this field.

## Note on "cued" vs "unconstrained"

This is *not* about whether the describer was cued with a previously-used
word/label — that guess was incorrect. The actual manipulation is
structural: in **cued**, one of the twelve tangrams is privately
highlighted as the sequential target each trial (matcher clicks one
shape, full per-trial feedback given, grid numbers removed since order is
enforced). In **unconstrained/free-matching**, the whole 12-tangram board
is described/matched at once in any order the pair chooses (matcher
drags shapes into place, grid cells are numbered to allow spatial
reference, feedback is only a batched end-of-round score). This
manipulation is largely orthogonal to our schema's 10 fields, with one
exception: it does bear directly on `feedback`, as detailed in the FLAG
above.

## Summary of flags

17 VERIFIED, 1 UNKNOWN (citation year/volume/issue/page), 1 FLAGGED
(`feedback` for "unconstrained" condition — paper describes a batched
end-of-round correctness score, not per-trial correctness feedback as
currently recorded). `role_constancy` (both conditions) and `feedback`
(cued condition) were re-checked against corrected field definitions
(role_constancy = strictly about individual describer/matcher
role-switching, not partner change; feedback = correctness signal only,
distinct from backchannel) and remain VERIFIED as originally reported;
the "unconstrained" feedback flag also holds under the corrected
definition, since the original reasoning was already about correctness
granularity, not conversational/backchannel ability.
