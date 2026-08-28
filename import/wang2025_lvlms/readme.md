# Readme

## Citation
Wang, Z., Li, W., Kaliosis, P., Rambow, O., & Brennan, S. E. (2025, November). LVLMs are Bad at Overhearing Human Referential Communication. In Proceedings of the 2025 Conference on Empirical Methods in Natural Language Processing (pp. 16769-16793).

## Abstract
During spontaneous conversations, speakers collaborate on novel referring expressions, which they can then re-use in subsequent conversations. Understanding such referring expressions is an important ability for an embodied agent, so that it can carry out tasks in the real world. This requires integrating and understanding language, vision, and conversational interaction. We study the capabilities of seven state-of-the-art Large Vision Language Models (LVLMs) as overhearers to a corpus of spontaneous conversations between pairs of human discourse participants engaged in a collaborative object-matching task. We find that such a task remains challenging for current LVLMs and they all fail to show a consistent performance improvement as they overhear more conversations from the same discourse participants repeating the same task for multiple rounds. We release our corpus and code for reproducibility and to facilitate future research. 

## Study details

* 10 pairs of participants (20 participants total) did 4 rounds of baskets and 4 rounds of dogs (we have 20 games, 10 and 10)
* each stimulus set had 10 targets + 3 extras
* no role switches
* modality: "Partners sat in separate rooms and communicated via an audio channel."
* each pair did both dogs and baskets, counterbalanced for order (but I don't think we have which came first per group)
* backchannel: free 
* feedback: no mention in paper, we assume that means no feedback
* confederates: no
* prior_relationship: no mention in paper, we assume no prior relationship since it's not stated


## Processing/import
* we treat the dog set and the basket set as separate games even though they do have the same participant pairs
* paper reports that all pairs got 100% accuracy each time
* we have image files
don't have demogs, have image files, selections, and text 