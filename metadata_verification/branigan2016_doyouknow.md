# branigan2016_doyouknow

**Source:** https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2016.00213/full (Branigan, Bell, & McLean, 2016, "Do You Know What I Know? The Impact of Participant Role in Children's Referential Communication," *Frontiers in Psychology*, 7, 213), fetched via WebFetch (open access, Frontiers in Psychology). Quotes below are drawn from the Participants and Procedure sections.

## Citation check

- **Authors/order:** Matches — "Branigan, H. P., Bell, J., & McLean, J. F." corresponds exactly to the recorded full_cite.
- **Title:** Matches (case-only difference) — paper title "Do You Know What I Know? The Impact of Participant Role in Children's Referential Communication" vs. recorded "Do you know what i know? The impact of participant role in children's referential communication."
- **Journal/volume/article number:** Matches — "Frontiers in Psychology, 7, 213" (Frontiers uses article numbers rather than page ranges; 213 is the article number, consistent with recorded "213").
- **short_cite:** "Branigan et al. (2016)" — consistent with the full_cite year and author list.
- No citation-level flags.

## Condition-level fields

The three recorded rows ("side participant," "overhearer," "naive participant") all carry identical values for every field below except condition_label. This matches the paper's design: the three labels distinguish only the *passive* third child's (Matcher B's) exposure during rounds A1–A4, not the fields below, which describe overall session structure and apply uniformly across the manipulation. Findings therefore apply identically to all three condition rows.

**group_size = 3**
VERIFIED — "Groups of three children were taken into the experimental room" (Procedure); "Seventy-two children ... participated in the experiment (i.e., 8 groups per condition)" (Participants) confirms triads of 3.

**prior_relationship = yes**
UNKNOWN from the paper text — Participants section states only: "Seventy-two children aged between 8 and 10 years (mean: 9 years 7 months) recruited from a junior school in Nottinghamshire, UK, participated in the experiment." There is no statement about whether the three children in a triad knew each other beforehand. **Confirmed (2026-08-27) directly by the dataset owner** — children are assumed to know each other since they're recruited from the same (junior) school — now documented in readme.md.

**partner_constancy = no**
VERIFIED — The Director's active partner switches mid-session: rounds A1–A4 are run with Matcher A ("The Director and Matcher A took their seats" ... they completed four rounds), then "The Director would then do the same with Matcher B for a further four rounds" — "Matcher B took the place of Matcher A." The Director interacts with two different partners across the session, confirming partner_constancy = no.

**role_constancy = yes**
VERIFIED (re-checked against corrected definition — strictly describer-vs-matcher switching, not audience/partner composition) — "The Director and Matcher A would do this for four envelopes ... The Director would then do the same with Matcher B for a further four rounds," confirming the Director describes for all 8 rounds and never matches. Matcher A is the active matcher for A1–4, then "took the overhearer's seat" for B1–4 — she never becomes the Director/describer. Matcher B occupies a passive third-party role (side participant/overhearer/naive participant) for A1–4, then becomes the active matcher for B1–4 — she also never describes. No participant is described as being a describer at one point and a matcher at another; the Director-vs-matcher role assignment is fixed for every child. (Note: my earlier FLAG on this field conflated audience/partner-composition changes — who is active vs. passive — with describer/matcher role-switching; per the corrected definition, those are not the same thing, and this field is correctly recorded as "yes.")

**population = child**
VERIFIED — "Seventy-two children aged between 8 and 10 years (mean: 9 years 7 months) ... participated in the experiment."

**confederates = no**
VERIFIED (by inference from procedure) — "The children drew lots to decide roles" — all three triad members are drawn from the recruited pool of 72 children and randomly assigned roles; no confederate is described or implied anywhere in the Procedure.

**modality = oral-in-person**
VERIFIED — "Groups of three children were taken into the experimental room" together, with the Director and Matcher seated at the same table ("A table divider in the middle of the table prevented Director and Matcher from seeing each other's cards"), confirming co-present spoken interaction.

**feedback = limited**
VERIFIED (re-checked against corrected definition — correctness feedback about the matcher's selection, not conversational backchannel) — "After each round, the experimenter checked the accuracy of the card positions, and provided feedback about how many were correctly placed." This confirms correctness feedback was given, but only as an aggregate count per round (how many of the four cards were correctly placed), delivered after the round ended by the experimenter — not item-by-item or real-time. This partial/aggregate form of correctness feedback is consistent with "limited" rather than "full" or "none." (Note: my earlier FLAG on this field incorrectly used the "they could talk as much as needed" quote, which describes conversational/backchannel ability during a trial, not correctness feedback. Per the corrected definition those are separate constructs, and the recorded "limited" is supported by the experimenter's round-end accuracy-count feedback.)

**backchannel = full**
VERIFIED — Coding-scheme text distinguishes backchannel from feedback while confirming both were freely possible: "backchannel responses (e.g., *yeah*) that encouraged the director to continue were not classified as feedback," alongside "they could talk as much as needed" — indicating the active matcher could backchannel freely throughout a trial.

**language = English**
UNKNOWN from the paper text — never stated explicitly (no occurrence of "English"). Strongly implied by the recruitment site ("a junior school in Nottinghamshire, UK") and matches the (English) example transcripts. **Confirmed (2026-08-27) directly by the dataset owner** — English, consistent with the UK study site and the transcripts — now documented in readme.md.

## Summary of flags

- Verified from paper text: 8 fields x 3 conditions (group_size, partner_constancy, role_constancy, population, confederates, modality, feedback, backchannel) = 24
- Confirmed by dataset owner (not stated in paper text): 2 fields x 3 conditions (prior_relationship, language) = 6
- Flagged: 0 condition fields; 0 citation-level flags.
