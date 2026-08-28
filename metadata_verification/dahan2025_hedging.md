# dahan2025_hedging

**Note:** This dataset was split (2026-08-27) from a combined `dahan2023_collaboration`
dataset that previously held this sample and the 2023 college-student sample as two
conditions of one dataset. This file now covers only this dataset's single condition,
`color_boxes` (20 games). See `dahan2023_collaboration.md` for the companion 2023
sample.

**Source:** Dahan, D. (2025). When hedging helps, rather than impedes, communication:
collaboration in the referential communication task. *Discourse Processes, 62*(2),
89-111. https://doi.org/10.1080/0163853X.2024.2437738. PDF read directly from
`/home/vboyce/Zotero/storage/T9UWYHR6/Dahan - 2025 - When hedging helps, rather than
impedes, communication collaboration in the referential communicati.pdf` (read in
full).

## Citation check

- Author/title/journal: VERIFIED — matches PDF header and running head exactly:
  "Delphine Dahan (2025) When hedging helps, rather than impedes, communication:
  collaboration in the referential communication task, Discourse Processes, 62:2,
  89-111" (p. 89).
- `short_cite` "Dahan (2025)": VERIFIED, consistent with author/year.
- **Resolved (was previously flagged when this was a combined dataset):** before the
  split, this condition's `full_cite`/`short_cite` duplicated the 2023 paper's
  citation instead of citing Dahan (2025). Now that this is its own dataset with its
  own `import.R`, `full_cite`/`short_cite` correctly cite Dahan (2025) directly.

## Condition-level fields (condition_label = "color_boxes")

**Context on this sample (per the dataset's `readme.md`, not newly discovered here):**
Dahan (2025), p. 92, states directly: "The present study describes a corpus of 20
conversations... These conversations were collected during the same period as the
conversations whose analyses were reported in Dahan (2023), using the same materials
and the same procedure. While the dataset reported in Dahan (2023) involved
conversations among pairs of college students, the present corpus consists of
conversations among individuals recruited from the Philadelphia community."

| Field | Status | Evidence |
|---|---|---|
| group_size = 2 | VERIFIED | "Participants were tested in pairs" (p. 96, Procedure); "Across the 960 trials (2 tangram sets x **10 dyads** x 16 tangrams x 3 mentions)" (p. 101) |
| language = English | Confirmed by dataset owner | Not stated explicitly in the paper — same situation as the 2023 paper: all transcript examples (Table 1, Table 3, p. 98–99) are in English; Philadelphia-based recruitment. Confirmed (2026-08-27) directly by the dataset owner (US-based study, audio/transcripts are English) and now documented in readme.md. |
| prior_relationship = yes | **FLAG** | Mixed sample, not uniformly acquainted: "Forty individuals were recruited from the Philadelphia community via internet-based listing (i.e., Craigslist)... Half of these individuals were randomly paired with each other. For the other half, individuals who responded to the ad were asked to bring a friend with whom to complete the study." (p. 95). 10 of 20 dyads were strangers (paired via Craigslist), 10 were friends. |
| partner_constancy = yes | VERIFIED (by inference) | "The procedure was identical to that reported in Dahan (2023)" (p. 96); same fixed-dyad structure, no repartnering mentioned |
| role_constancy = no | VERIFIED (individual role-switching, not partner-switching) | "Participants switched roles every four sequences (i.e., every 12 trials)" (p. 96, Procedure — identical wording to 2023); every participant served as both director and matcher |
| population = adult | VERIFIED | "the average age was 33 (with a median of 29), ranging from 19 to 62" (p. 95) |
| confederates = no | VERIFIED | No confederate design is mentioned; the corpus is explicitly "a corpus of conversations between pairs of participants, a director and a matcher" (p. 95), all real community recruits, paired with each other or a self-brought friend — no third-party confederate role appears anywhere in Participants/Procedure |
| modality = oral-in-person | VERIFIED | "sat across from each other at a table, with an opaque barrier obstructing their view of each other and their respective workspace... were able to interact verbally" (p. 96, Procedure) |
| feedback = none | VERIFIED (correctness feedback only) | "Some studies try to minimize the risk of errors by providing participants with immediate feedback on their performance... Here, **no feedback was provided** so as to not interfere with participants' spontaneous behaviors in the task" (p. 92); also "Participants were not made aware of their errors" (p. 96, Procedure) |
| backchannel = full | VERIFIED | Directors paused, "seemingly waiting for the matcher to acknowledge it with a **backchannel** before proceeding" (p. 98); rich 9-category matcher response set including offering candidate understanding, yes/no and content questions, negative evidence, mumbling (Table 4, p. 100) |

## Summary of flags

- **`prior_relationship` FLAG:** the sample is a 50/50 mix of randomly-paired
  strangers (via Craigslist) and self-brought friends, so uniform "yes"
  oversimplifies it — consistent with the dataset readme's own caveat that the true
  split can't be recovered per-dyad.

**Counts:** 8 VERIFIED from paper text, 1 confirmed by dataset owner (language), 1 FLAG (prior_relationship).
