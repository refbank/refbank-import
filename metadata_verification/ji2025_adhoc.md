# ji2025_adhoc

**Source:** Ji, A., Bergey, C. A., Eliav, R., Artzi, Y., & Hawkins, R. D. (2025). Ad hoc conventions
generalize to new referents. arXiv:2509.05566. Fetched full text via
`https://arxiv.org/pdf/2509.05566` and converted with `pdftotext -layout` (WebFetch could not
extract readable text directly from the PDF stream).

**Mapping note:** The paper reports three studies: **Pilot 1** (Appendix C, "Manipulating
nameability"), **Pilot 2** (Appendix D, "Generalizing to unseen targets"), and the main study
described in Results (§2) / Methods (§3), whose **interactive phase** is the only phase in which
partners exchange multiple messages. Based on the `_singlemessage` suffix and the recorded
backchannel values, I mapped:
- `expt1_singlemessage` → **Pilot 1** (Appendix C)
- `expt2_singlemessage` → **Pilot 2** (Appendix D)
- `expt3` → the **main study's interactive phase** (§3.3), the only phase with free chat exchange

This mapping is an inference (the paper does not use the labels "Experiment 1/2/3" itself), but it
is strongly supported: Pilots 1 and 2 both use an explicit "send only one message" design, while
the main study's interactive phase explicitly allows free chat — exactly the single-message vs.
backchannel distinction the recorded values encode.

## Citation check

- Authors: "Anya Ji, Claire Augusta Bergey, Ron Eliav, Yoav Artzi, Robert D. Hawkins" (p.1 byline) — VERIFIED, matches `full_cite`.
- Title: "Ad hoc conventions generalize to new referents" (p.1) — VERIFIED.
- arXiv ID: header stamp "arXiv:2509.05566v1 [cs.CL] 6 Sep 2025" (p.1) — VERIFIED.
- `short_cite` "Ji et al. (2025)" — VERIFIED, standard format for 5-author paper matching first author surname and year.

## Condition-level fields

### 1. expt1_singlemessage (Pilot 1, Appendix C)

| Field | Value | Status | Evidence |
|---|---|---|---|
| group_size | 2 | VERIFIED | "We recruited 60 pairs of participants from Prolific" (§C.1, p.25) |
| prior_relationship | no | VERIFIED | "Participants were randomly assigned to pairs after providing consent and passing a tutorial and a quiz" (§C.3, p.27) — randomly matched strangers, not pre-existing pairs |
| partner_constancy | yes | VERIFIED (inferred) | Same passage (§C.3, p.27) describes one continuous 5-block/6-trial game per assigned pair with no re-pairing event mentioned; only roles alternate ("participants alternated roles between blocks") |
| role_constancy | no | VERIFIED | "The participants were randomly assigned to the speaker or the listener roles... participants alternated roles between blocks" (§C.3, p.27) — each participant is literally both a speaker (describer) and a listener (matcher) at different points, since roles alternate every block |
| population | adult | UNKNOWN | Paper gives no explicit age statement; only that participants were recruited "from Prolific" with English/US-UK inclusion criteria (§C.1, p.25). Adult status is a reasonable Prolific-platform inference but not stated in text. |
| confederates | no | VERIFIED (inferred) | Recruited N matches pairs exactly (60 pairs = 120 participants, minus 8 excluded pairs); no mention of confederates or experimenter-players anywhere in the paper |
| modality | written | VERIFIED | "both players saw 10 tangrams and a chat box... the speaker was asked to send only one message" (§C.3, p.27) — text-based |
| feedback | limited | VERIFIED | "Both participants received feedback after each trial indicating if the listener had responded correctly" (§C.3, p.27) — a correctness-only signal (right/wrong), not the full selection/target detail seen in the main study; this quote is strictly about post-trial correctness reporting, separate from the (also "none") backchannel/reply-ability quote below |
| backchannel | none | VERIFIED | "The speaker was asked to send only one message to describe the highlighted target tangram... The listener needed to select a tangram based on the description" (§C.3, p.27) — no reply channel for listener |
| language | English | VERIFIED | "preregistered inclusion criteria (English as first language and location based in US or UK)" (§C.1, p.25) |

### 2. expt2_singlemessage (Pilot 2, Appendix D)

| Field | Value | Status | Evidence |
|---|---|---|---|
| group_size | 2 | VERIFIED | "We recruited 60 pairs of participants, 8 of whom were excluded based on the same criteria used in Pilot 1" (§D.1, p.30) |
| prior_relationship | no | VERIFIED (inferred) | "the procedure was a direct replication and extension" of Pilot 1 (§D.2, p.30), which randomly assigns strangers into pairs |
| partner_constancy | yes | VERIFIED (inferred) | Same replication statement (§D.2, p.30); test-phase addendum: "Speaker and listener roles were swapped between every trial" — only roles change, not partners |
| role_constancy | no | VERIFIED | "Speaker and listener roles were swapped between every trial" in the added test phase (§D.2, p.31), inheriting Pilot 1's per-block role alternation for the training phase — each participant is literally both describer and matcher over the course of the session |
| population | adult | UNKNOWN | No explicit age statement; same Prolific recruitment as Pilot 1 |
| confederates | no | VERIFIED (inferred) | No mention of confederates; recruited N matches pairs exactly |
| modality | written | VERIFIED | Inherits Pilot 1's chat-box/single-message text interface via "direct replication and extension" (§D.2, p.30) |
| feedback | limited | VERIFIED (inferred) | Inherits Pilot 1's correctness-only feedback ("received feedback after each trial indicating if the listener had responded correctly," §C.3, p.27) via "the procedure was a direct replication and extension" (§D.2, p.30); not independently re-stated in Appendix D, but no change to feedback design is mentioned — the only stated change is the added test-phase block |
| backchannel | none | VERIFIED (inferred) | Test phase: "Each tangram in the new context was given a single trial" (§D.2, p.31), continuing the single-message design of Pilot 1 |
| language | English | VERIFIED | "8 of whom were excluded based on the same criteria used in Pilot 1" (§D.1, p.30) → same English/US-UK inclusion criteria |

### 3. expt3 (main study, interactive phase — §3.3)

| Field | Value | Status | Evidence |
|---|---|---|---|
| group_size | 2 | VERIFIED | "We recruited 163 pairs of participants from Prolific" (§3.1, p.11); "N = 302" |
| prior_relationship | no | VERIFIED | "they were paired with a random partner to play a repeated reference game in the interactive phase" (§3.3, p.12) |
| partner_constancy | yes | VERIFIED (inferred) | Same quote (§3.3, p.12) — one partner assigned for the interactive phase; only "speaker"/"listener" roles swap each block, no re-pairing described |
| role_constancy | no | VERIFIED | "The participants were randomly assigned to be the speaker or the listener and swapped roles after each block" (§3.3, p.12) — each participant is literally both speaker/describer and listener/matcher across the five blocks (this is strictly about individual role-switching, distinct from partner_constancy, which concerns whether the partner/audience itself changed — it did not, per the partner_constancy row above) |
| population | adult | UNKNOWN | No explicit age statement; Prolific recruitment only (§3.1, p.11) |
| confederates | no | VERIFIED (inferred) | 163 pairs recruited = 326 participants (302 analyzed after exclusions); no mention of confederates |
| modality | written | VERIFIED | "Both participants were able to communicate freely through the chat box" (§3.3, p.12) |
| feedback | limited | **FLAG** | Paper states: "participants switched roles each block and **received full feedback** (i.e. the speaker saw the listener's selection, and the listener saw the intended target referent)" (§3.3, p.12; also §2.1, p.4: "received full feedback (i.e. the speaker saw the listener's selection, and the listener saw the intended target referent)"). This is a distinct sentence describing **post-trial outcome disclosure** (what happened after the listener selected), not the mid-trial chat/backchannel ability (that is a separate sentence: "Both participants were able to communicate freely through the chat box"). Confirmed this quote is about correctness-related feedback, not backchannel: after the trial, the paper's own framing groups this alongside the pilots' "responded correctly" feedback as the analogous stage of the trial cycle (target reveal + selection reveal directly tells both parties whether the choice was correct, and more). Since this is richer than the pilots' correctness-only ("limited") feedback, our recorded value of "limited" for expt3 conflicts with the paper's explicit "full feedback" wording — it should likely be corrected to **"full."** |
| backchannel | full | VERIFIED | "Both participants were able to communicate freely through the chat box. If the timer had decreased to under 30 seconds, sending a message reset the timer... so that both participants could communicate as much as necessary" (§3.3, p.12) — this is exactly what differs from expt1/2's single-message design: expt3's interactive phase supports open-ended multi-turn chat with no reply restriction, confirming backchannel=full is correct and clearly distinguished from expt1/2's backchannel=none. |
| language | English | VERIFIED | "based on preregistered inclusion criteria (English as first language and location based in US or UK)" (§3.1, p.11) |

## Summary of flags

- **33 fields checked** across 3 conditions (11 fields × 3 conditions).
- **VERIFIED: 27** (including several marked "inferred" where the paper doesn't use the exact
  relationship word but the described procedure directly entails the value).
- **Dataset owner confirms (2026-08-27)**: population=adult is reasonable given Prolific recruitment across all three studies, now documented in readme.md.
- **UNKNOWN: 4** (population/adult status for all three conditions is never explicitly stated in
  the paper — only assumed via Prolific recruitment).
- **FLAG: 1** — `expt3` feedback is recorded as "limited" but the paper explicitly and repeatedly
  describes it as **"full feedback"** (speaker sees listener's selection; listener sees the
  intended target), which is richer than expt1/expt2's correctness-only "limited" feedback. This
  is a post-trial correctness-disclosure quote, not a description of mid-trial backchannel/chat
  ability (that ability is described in a separate sentence and is correctly captured by the
  `backchannel` field). This value should likely be corrected to "full."
- **role_constancy** (re-checked against the corrected definition — individual describer/matcher
  role-switching, not partner change): "no" is VERIFIED for all three conditions. In every
  condition every participant is both a describer and a matcher at different points (roles
  alternate every block, or every trial in Pilot 2's test phase), regardless of the fact that the
  partner/audience stays constant (see `partner_constancy`, unaffected by this correction).
