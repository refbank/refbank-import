# leung2024_scaffolding

**Source:** Leung, A., Yurovsky, D., & Hawkins, R. D. Parents spontaneously scaffold the formation of conversational pacts with their children. PDF at `/home/vboyce/Zotero/storage/2DV3EMHX/Leung et al. - Parents spontaneously scaffold the formation of conversational pacts with their children.pdf` (DOI 10.1111/cdev.14186).

## Citation check

**FLAG — year mismatch between `full_cite` and `short_cite`/`dataset_id`.**

The PDF's own citation box (p. 16) reads:

> "How to cite this article: Leung, A., Yurovsky, D., & Hawkins, R. D. (2024). Parents spontaneously scaffold the formation of conversational pacts with their children. *Child Development*, 00, 1–16. https://doi.org/10.1111/cdev.14186"

The title page footer (p. 1) similarly says "© 2024 The Authors. *Child Development* published by Wiley Periodicals LLC..." and the article is paginated "1–16" with a placeholder volume/issue of "00" — this is the Wiley **Early View / online-first** version, dated 2024.

Our recorded `full_cite` gives "(2025). ... Child Development, 96(2), 546–561" — a fully assigned volume/issue/page range, which is the citation format used once a paper is slotted into a final print issue. This is consistent with the same article going to Early View in 2024 (matching this PDF and our `short_cite`/`dataset_id` of "2024") and then formally appearing in print in early 2026's Child Development, 96(2) dated 2025. This is a common Wiley behavior (Early View year ≠ final issue year) and not necessarily an error, but it **is an internal inconsistency**: `full_cite` says 2025 while `short_cite` and `dataset_id` say 2024. Recommend picking one year consistently across `full_cite`/`short_cite`/`dataset_id` (the PDF in hand only supports the 2024 online-first citation; the 2025/96(2)/546–561 details could not be verified against this file and should be checked against the final published version if that's the intended citation of record).

## Condition-level fields

### Condition 1: "adult-child" (parent–child dyads, Experiment 1)

| Field | Value | Status | Evidence |
|---|---|---|---|
| group_size | 2 | VERIFIED | "Pairs of participants were brought into the laboratory to play a cooperative director–matcher game." (p.3, Design and procedure) — dyads throughout; final sample "63 pairs" (p.3, Participants) |
| prior_relationship | yes | VERIFIED | Population is parents and their own children: "Children (ages 4, 6, and 8) and their parents were recruited from a database of families in the local community" (p.3, Participants) — a pre-existing parent–child relationship is definitional to this recruitment, not a separately stated fact |
| partner_constancy | yes | VERIFIED | "...each dyad played the game for four blocks" (Figure 1 caption, p.4); no mention of partner reassignment across the four repetition blocks — the same parent–child pair plays throughout |
| role_constancy | no | VERIFIED (re-checked against corrected definition: individual role-switching, not partner-switching) | "Parents and children alternated roles on each trial." (Figure 1 caption, p.4); also "we constructed the trial sequence to ensure that players both alternated roles from trial to trial..." (p.4, Preprocessing/Design). Each individual parent and each individual child personally took both the describer (director) role and the matcher role at different trials within the session — this is genuine within-person role-switching, not merely a partner change, so "no" is correct under the corrected definition |
| population | child-parent | VERIFIED | "Children (ages 4, 6, and 8) and their parents were recruited..." (p.3) |
| confederates | no | VERIFIED | Methods describe only genuine parent–child dyads as director/matcher; no experimenter or confederate ever plays a game role in Experiment 1 |
| modality | oral-in-person | VERIFIED | "Pairs of participants were brought into the laboratory..."; "They were seated in front of iPads at opposite ends of a table, with a divider preventing them from seeing the other's screen... they were explicitly instructed to use words only" (p.3, Design and procedure) |
| feedback | none | VERIFIED (re-checked against corrected definition: correctness feedback only, not backchannel) | "Importantly, neither the matcher nor the director received explicit feedback about accuracy: the same sound played whether or not the matcher's selection was correct." (p.4). This quote is specifically about correctness/accuracy feedback (a scoring signal), distinct from the separate backchannel quote about transcribers not counting "yes, mmhm, I see" as interruptions — no conflation between the two fields |
| backchannel | full | VERIFIED | Live, unrestricted spoken conversation was possible/observed: "Transcribers were instructed that backchannels (e.g., yes, mmhm, I see) should not count as interruptions, unless they led the speaker to stop talking." (p.4, Preprocessing) |
| language | English | UNKNOWN | Not explicitly stated for Experiment 1. The paper only states English explicitly for the Experiment 2 audio stimuli ("All recordings were by female native English speakers," p.8); US-based recruitment (Madison, WI / Chicago area) makes English highly likely but this is not directly stated for the parent–child sessions themselves |

### Condition 2: "adult-adult" (comparison/control dyads, Experiment 1)

| Field | Value | Status | Evidence |
|---|---|---|---|
| group_size | 2 | VERIFIED | "...a convenience sample of adult participants were recruited from a Psychology Department subject pool to achieve a planned control group size of 20 adult-adult pairs." (p.3, Participants) |
| prior_relationship | no | VERIFIED (with caveat) | Same sentence confirms these are **not** the parents from the parent–child condition, but an independently-recruited "convenience sample of adult participants... from a Psychology Department subject pool." Subject-pool recruitment into experimenter-assigned pairs is standard practice for unacquainted strangers, but the paper never states explicitly "these participants did not know each other" — see note below |
| partner_constancy | yes | VERIFIED | Same four-block repeated-reference design applies to this group ("our adult-adult control condition," p.5, Reduction in length of referential expression); no mention of partner reassignment |
| role_constancy | no | VERIFIED | General procedure applies to all pairs: "Participants were told that they would take turns playing director and matcher roles." (p.3); Figure 2/3 report "adult director" data implying alternation within adult dyads consistent with the general alternation rule |
| population | adult | VERIFIED | "a convenience sample of adult participants were recruited from a Psychology Department subject pool" (p.3) |
| confederates | no | VERIFIED | Same subject-pool adult participants play both roles; no confederate or experimenter is described as playing director/matcher |
| modality | oral-in-person | VERIFIED | Same lab-based Design and procedure section applies ("brought into the laboratory," iPads + divider, verbal-only instruction); adult compensation ("$5 each or course credit for their participation," p.3) confirms in-person lab attendance |
| feedback | none | VERIFIED | Same procedure-wide statement: "neither the matcher nor the director received explicit feedback about accuracy" (p.4) |
| backchannel | full | VERIFIED | Same transcription/backchannel handling applies across all sessions, adult and parent–child alike (p.4, Preprocessing) |
| language | English | UNKNOWN | Not explicitly stated for the adult-adult sessions; same reasoning as the adult-child condition above |

## Summary of flags

- **Citation year FLAG**: `full_cite` (2025, *Child Development* 96(2), 546–561) conflicts with the PDF's own "how to cite" box and copyright line, both dated 2024 (matches `short_cite`/`dataset_id`). Likely an Early View (2024) vs. final-issue (2025) discrepancy — needs a decision on which to use consistently.
- **adult-adult comparison group**: Confirmed to be an **independent adult subject-pool sample**, NOT the same parents paired with each other. This directly supports `prior_relationship=no` (they are not the parent-child dyad's parents paired together), though the paper never explicitly states the subject-pool adults were strangers to one another before pairing — flagged as a caveat rather than a hard conflict.
- **Field counts**: 18 VERIFIED, 2 UNKNOWN (language, both conditions), 0 FLAG at the field level; 1 citation-level FLAG (year mismatch).
