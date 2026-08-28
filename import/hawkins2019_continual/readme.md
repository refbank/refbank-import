# Readme

## Citation
Hawkins, R. D., Kwon, M., Sadigh, D., & Goodman, N. D. (2019). Continual adaptation for efficient machine communication. Proceedings of the 24th Conference on Computational Natural Language Learning.

## Abstract

To communicate with new partners in new contexts, humans rapidly form new linguistic
conventions. Recent neural language models are able to comprehend and produce the
existing conventions present in their training data, but are not able to flexibly
and interactively adapt those conventions on the fly as humans do. We introduce an
interactive repeated reference task as a benchmark for models of adaptation in 
communication and propose a regularized continual learning framework that allows
an artificial agent initialized with a generic language model to more accurately
and efficiently communicate with a partner over time. We evaluate this framework
through simulations on COCO and in real-time reference game experiments with human partners.

## Study details

per refbank inclusion criteria, we only include games with a human describer. 

human-human:
* recruited via Mechanical Turk; we assume adults, no prior relationship, and English based on this (paper doesn't state these explicitly)
* 108 participants (54 pairs) (we have 56 games, 49 post exclusions)
* 4 images x 6 blocks (24 trials)
* backchannel: paper says "interactive chat", but we don't see any matcher messages, so we have coded it as "none"
* feedback - full "Both agents then receive feedback about the listener’s selection and the identity of the target."

we also have 56 games (52 post exclusions) of a "human-human-easy" condition that is not reported; we believe this has the same structure as 
the other human-human games, but with easier image contexts

human speaker - model listener:
* 57 participants (we have 60 games)
* single message / trial (so backchannel = "none")
* again 4 images x 6 blocks 
* we coded this as having "confederate" because of the model, and only 1 participant, but idk
* feedback - full "Both agents then receive feedback about the listener’s selection and the identity of the target."

model speaker - human listener (reported in Appendix) is not imported here


## Processing/import
Data pulled from github. 

Images are from COCO, we do not copy the images to refbank (but could consider), 
but they can be found in COCO. 

We have text and selections. (we don't include model selections)

We don't have demographics (might exist?)

there is some time info in original data but unclear how to map it