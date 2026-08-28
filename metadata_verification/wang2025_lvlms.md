# wang2025_lvlms

**Source:** Wang, Z., Li, W., Kaliosis, P., Rambow, O., & Brennan, S. E. (2025). LVLMs are Bad at Overhearing Human Referential Communication. In *Proceedings of the 2025 Conference on Empirical Methods in Natural Language Processing*, pp. 16769-16793. Fetched from https://aclanthology.org/2025.emnlp-main.849.pdf and read directly (main text pp. 1-9, plus appendix/references pp. 10-15).

## Citation check

- **MAJOR FLAG — human data provenance.** The paper is explicit that the human referential-communication corpus it uses is *not* newly collected for this paper. Section 4 ("Corpus"), Overview: "Our corpus comprises 80 human-to-human dialogues totalling 27,902 words, **collected by Calion B. Lockridge and Susan E. Brennan in 2001 at Stony Brook University and not previously published**. Ten pairs of native-English-speaking undergraduates (20 speakers in total) did repeated rounds of a referential communication task (Krauss and Glucksberg, 1969; Clark and Wilkes-Gibbs, 1986)."
  - The human dyad data (the object of our condition-level coding) was collected in **2001** by Lockridge & Brennan — a full 24 years before this 2025 paper. Susan E. Brennan is a co-author of both the 2001 data collection and the present 2025 paper, which is why the corpus is now available/published here.
  - There is no separate reference-list entry for "Lockridge & Brennan (2001)" since that data was never published as a standalone paper — it is only described narratively in Section 4.
  - **Recommendation:** if refbank's condition-level metadata is meant to characterize the human data-collection procedure (participants, modality, roles, etc.), the citation/attribution should note that the human corpus originates from an unpublished 2001 Lockridge & Brennan study, with Wang et al. (2025) as the paper that published/released it and ran the LVLM overhearing experiments on top of it. `full_cite`/`short_cite` pointing solely to "Wang et al. (2025)" is technically correct for *where this row's information appears in print*, but readers should not assume Wang et al. ran the human experiment — they did not.
- Author list, title, venue, page range in our `full_cite`: VERIFIED against the PDF header ("LVLMs are Bad at Overhearing Human Referential Communication," Zhengxiang Wang, Weiling Li, Panagiotis Kaliosis, Owen Rambow, Susan E. Brennan; EMNLP 2025).
- `short_cite` "Wang et al. (2025)": VERIFIED, consistent with author/year.

## Condition-level fields

| Field | Our value | Verdict | Evidence |
|---|---|---|---|
| group_size = 2 | 2 | VERIFIED | "Ten pairs of native-English-speaking undergraduates (20 speakers in total)" (Sec. 4, Overview); "speakers were recruited in pairs, with one partner randomly assigned to the role of director (D) and the other to the role of matcher (M)" (Sec. 4, Task and Materials). |
| language = English | English | VERIFIED | "Ten pairs of **native-English-speaking** undergraduates" (Sec. 4, Overview). |
| prior_relationship = no | no | UNKNOWN | The paper never states whether the paired undergraduates knew each other before the study or were recruited/paired as strangers. No mention of a friendship/acquaintance manipulation anywhere in Sections 4-7 or the Appendix. |
| partner_constancy = yes | yes | VERIFIED | "Each pair completed a total of eight rounds of the referential communication task in a one-hour session — four rounds with the same set of pictures of dogs, and four rounds with the same set of pictures of baskets" (Sec. 4, Task and Materials). Same director/matcher pair completes the whole session; no repartnering is described. |
| role_constancy = yes | yes | VERIFIED (re-checked against corrected definition: no participant switched between describer/director and matcher role) | "one partner randomly assigned to the role of director (D) and the other to the role of matcher (M); **they remained in their assigned role throughout the experiment**" (Sec. 4, Task and Materials). This is explicitly about each individual's own role never changing (director stays director, matcher stays matcher, for all 8 rounds) — not merely about partners staying paired together, which is the separate `partner_constancy` field. No participant is ever described as being both a director and a matcher. |
| population = adult | adult | VERIFIED | "Ten pairs of native-English-speaking **undergraduates**" (Sec. 4, Overview). |
| confederates = no | no | UNKNOWN | Not explicitly addressed. The description ("speakers were recruited in pairs, with one partner randomly assigned to the role of director... and the other to the role of matcher") implies both members of each pair were genuine participants rather than a confederate + naive participant design, but the paper never uses the word "confederate" or explicitly denies their use. |
| modality = oral-remote | oral-remote | VERIFIED | "Partners sat in **separate rooms** and communicated via an **audio channel**" (Sec. 4, Task and Materials). This is oral, mediated (not face-to-face/in-person), consistent with oral-remote. |
| feedback = none | none | UNKNOWN (re-checked against corrected definition: correctness feedback specifically, not backchannel) | Searched the full paper (Sections 4-8, Appendix A-D, including the results/stats tables in Appendix C) for any statement that human matchers were told — by an experimenter, a scoring device, or any signal — whether their card selections were right or wrong during the original 2001 session. None found. The only correctness-related statement is a post-hoc research finding, not a feedback mechanism given to participants: "All pairs successfully completed the matching task in all rounds, achieving 100% accuracy" (Sec. 6, "LVLMs underperform human matchers"). Section 7's "+Feedback" is explicitly a follow-up manipulation applied only to the LVLMs in the 2025 experiments (revealing the correct answers to the model after each round to test whether it self-corrects) — it is not part of, and gives no evidence about, the original human procedure. Per the corrected definition, this quote must not be used to justify a feedback value, and no other quote establishes one, so the field stays UNKNOWN. |
| backchannel = full | full | VERIFIED | "During each round, the pairs **spoke freely** while they matched duplicate sets of picture cards" (Sec. 4, Overview). The example dialogue (Figure 2) shows the matcher asking clarifying questions and giving acknowledgments in real time ("M: It has two handles? D: No, one handle. M: And it's kinda long? D: Yes..."), consistent with full two-way live conversation. |

## Summary of flags

1 major provenance/citation FLAG (human corpus originally collected by Lockridge & Brennan in 2001, not by Wang et al. 2025 — attribution should note this). No field-value conflicts (FLAGs) were found among the 10 condition-level fields themselves.

**Dataset owner confirms (2026-08-27)**: confederates=no, prior_relationship=no
(not stated in the paper, assumed default), and feedback=none (no mention in the
paper) are all reasonable, now documented in readme.md.

**Counts:** 7 VERIFIED (group_size, language, partner_constancy, role_constancy, population, modality, backchannel), 3 UNKNOWN (prior_relationship, confederates, feedback), 0 field-level FLAGs, 1 citation-provenance FLAG.
