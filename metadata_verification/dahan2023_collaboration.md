# dahan2023_collaboration

**Note:** This dataset was split (2026-08-27) from a combined `dahan2023_collaboration`
dataset that previously held both this sample and the 2025 community sample as two
conditions. This file now covers only this dataset's single condition, `color_boxes`
(20 games). See `dahan2025_hedging.md` for the companion 2025 sample, now its own
dataset.

**Source:** Dahan, D. (2023). Collaboration Under Uncertainty in Unscripted
Conversations: The Role of Hedges. *Journal of Experimental Psychology: Learning,
Memory, and Cognition*. Advance online publication, Feb 9, 2023.
https://dx.doi.org/10.1037/xlm0001210. PDF fetched from
https://bpb-us-w2.wpmucdn.com/web.sas.upenn.edu/dist/f/584/files/2023/02/Dahan_2023_JEPLMC.pdf
and read directly (16 pages).

## Citation check

- Author/title/journal: VERIFIED — matches PDF header exactly: "Dahan, D. (2023).
  Collaboration Under Uncertainty in Unscripted Conversations: The Role of Hedges.
  *Journal of Experimental Psychology: Learning, Memory, and Cognition*. Advance
  online publication."
- Volume/issue/page (49(2), 320) in our `full_cite`: UNKNOWN — the fetched PDF is
  the "Advance online publication" version (no volume/issue/page assigned yet; it
  shows only the DOI). The final print pagination in our `full_cite` could not be
  confirmed from this source, though it is plausible it was added after final
  publication.
- `short_cite` "Dahan (2023)": VERIFIED, consistent with author/year.

## Condition-level fields (condition_label = "color_boxes")

| Field | Status | Evidence |
|---|---|---|
| group_size = 2 | VERIFIED | "Participants were tested in pairs" (p. 4, Procedure); "2 tangram sets × **10 dyads**" (p. 9) |
| language = English | Confirmed by dataset owner | Not stated explicitly in the paper — transcript examples (Table 1, Table 3, p. 7–8) are in English; US institution. Confirmed (2026-08-27) directly by the dataset owner (US-based study, audio/transcripts are English) and now documented in readme.md. |
| prior_relationship = yes | **FLAG** | Mixed sample, not uniformly acquainted: "Half of these individuals participated in the study for course credit and were randomly paired with each other. The other half, 10 students were recruited from the same pool as above but asked to bring a friend..." (p. 4). 10 of 20 dyads were strangers; 10 were friends. Per the dataset readme, `prior_relationship="yes"` was chosen because the true per-dyad split can't be recovered from the data, not because all dyads were actually acquainted. |
| partner_constancy = yes | VERIFIED (by inference) | Fixed dyads throughout, only roles switch; no repartnering mentioned in Method/Procedure/Design (p. 4, 6) |
| role_constancy = no | VERIFIED (individual role-switching, not partner-switching) | "Participants switched roles every four sequences (i.e., every 12 trials)" (p. 4); every participant served as both director and matcher |
| population = adult | VERIFIED | "Forty students enrolled at my institution" (p. 4), college students |
| confederates = no | VERIFIED | Paper contrasts its design with confederate-director approaches without adopting one (p. 4); "unscripted conversations between two naïve participants" (p. 14, Discussion) |
| modality = oral-in-person | VERIFIED | "sat across from each other at a table with an opaque barrier... were able to interact verbally" (p. 4) |
| feedback = none | VERIFIED (correctness feedback only) | "participants were not made aware of their errors" (p. 4) |
| backchannel = full | VERIFIED | Directors paused for the matcher "to acknowledge it with a **backchannel** before proceeding" (p. 6); rich matcher response set (Table 4, p. 8) |

## Summary of flags

- **`prior_relationship` FLAG:** the sample is a 50/50 mix of randomly-paired
  strangers and self-brought friends, so uniform "yes" oversimplifies it — consistent
  with the dataset readme's own caveat that the true split (friends / probable
  classmates / strangers) can't be recovered per-dyad.

**Counts:** 8 VERIFIED from paper text, 1 confirmed by dataset owner (language), 1 FLAG (prior_relationship).
