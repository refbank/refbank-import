# dale2011_tangram

**Source:** Dale, R., Kirkham, N. Z., & Richardson, D. C. (2011). "How two people become a tangram recognition system." Fetched from https://co-mind.org/rdmaterials/pdfs/proceedings/dale_kirkham_richardson_duet.pdf

## Citation check

- **Title/authors** — VERIFIED. Title page: "How two people become a tangram recognition system / Rick Dale ... Natasha Z. Kirkham ... Daniel C. Richardson." Matches recorded `short_cite` "Dale et al. (2011)".
- **Venue ("Proceedings of the European Conference on Computer-Supported Cooperative Work")** — UNKNOWN. The fetched PDF text (title page through references) contains no venue/proceedings line or year stamp; only the source URL path (`.../pdfs/proceedings/...`) hints it is a proceedings paper. Cannot confirm or deny the specific venue string from the document text itself.
- **Year (2011)** — UNKNOWN from the paper's own text (no date printed on the pages retrieved), though consistent with a co-authored self-citation in the references list ("Dale, R., A. S Warlaumont, and D. C Richardson. 2011...").

## Condition-level fields

- **group_size = 2** — VERIFIED. "20 pairs of participants were recruited, and performed the tangram task for class credit. One participant in a pair was randomly assigned to the director role, and the other was assigned to matcher." (Methods, Participants)

- **language = English** — UNKNOWN from the paper text. Never states the language of instruction/interaction. Authors/institution (University of Memphis, Birkbeck, UCL) make English highly likely. **Dataset owner confirms (2026-08-27)** this as a reasonable assumption based on the English-speaking university setting, now documented in readme.md.

- **prior_relationship = no** — UNKNOWN from the paper text. No mention anywhere of whether paired participants were acquainted, friends, or strangers before the study. **Dataset owner confirms (2026-08-27)** "no" as the default assumption given no mention of a prior relationship, now documented in readme.md.

- **partner_constancy = yes** — VERIFIED (by strong implication). The Methods describe one continuous session per pair across all three rounds with no mention of partner reassignment, and the analysis explicitly aggregates "for each dyad, round, and modality combination" (Data and analysis section), treating each dyad as a fixed pairing across all three rounds.

- **role_constancy = yes** — VERIFIED (re-checked against corrected definition: no participant switched between describer/director and matcher roles). "Once participants obtained their respective director/matcher role (remaining constant throughout the experiment), they proceeded to identify the six shapes in random order." (Methods, Procedure) This is a direct statement that each individual kept a single role (director or matcher) for the entire session; no role-swap occurred. No change from prior check.

- **population = adult** — VERIFIED. "20 pairs of participants were recruited, and performed the tangram task for class credit" (Methods, Participants) — indicates a university student/course-credit sample, i.e., adults. (Note: the Apparatus section mentions one eye-tracking lab "was designed for infants under a year old," but this describes lab equipment/facility history, not the population tested in this study.)

- **confederates = no** — VERIFIED (by implication). "One participant in a pair was randomly assigned to the director role, and the other was assigned to matcher" — both members of each pair are drawn from the same recruited participant pool; no confederate is mentioned anywhere in the paper.

- **modality = oral-remote** — VERIFIED. "Two eye tracking labs on different floors of a building were used... Participants communicated through hands-free headsets which used an intercom feature on 2.4Ghz wireless phones." (Methods, Apparatus) This confirms the two members of a pair were physically separated (different labs/floors) and communicated only via audio headset — i.e., spoken but not co-present, matching "oral-remote."

- **feedback = none** — UNKNOWN (re-checked against corrected definition: strictly whether the matcher's selection was reported as right/wrong by experimenter, computer, or scoring signal — separate from backchannel talk-back). The paper does not describe any correctness/accuracy signal given to participants. The only relevant text is "When the matcher identified the 6th (of 6) shape, a new round was initiated by the software" (Methods, Procedure) — this shows the software tracks whether the matcher has identified all six shapes (needed to trigger round transitions), but nowhere states that participants were told, or the system indicated, whether any individual selection was correct or incorrect. No backchannel-related quotes were used to reach this conclusion. Cannot confirm "none" (vs. "full"/"limited") from the text — remains UNKNOWN. No change from prior check. **Dataset owner confirms (2026-08-27)** "none" as the standard assumption for this kind of ordering experiment absent any mention, already documented in readme.md.

- **backchannel = full** — VERIFIED (by implication). "Participants communicated through hands-free headsets which used an intercom feature on 2.4Ghz wireless phones" (Methods, Apparatus) — a live two-way intercom/headset connection permits open, real-time spoken exchange in both directions during a trial, consistent with "full" backchannel. No restriction on matcher-to-director communication is mentioned.

## Summary of flags

No FLAGs — no recorded value contradicts the paper text. role_constancy and feedback were re-checked against corrected field definitions (role_constancy: individual role-switching only, not partner-switching; feedback: correctness signal only, not backchannel) — both conclusions are unchanged.

(Exact tally: VERIFIED = group_size, partner_constancy, role_constancy, population, confederates, modality, backchannel = 7 fields; UNKNOWN = language, prior_relationship, feedback = 3 fields; FLAG = 0.)
