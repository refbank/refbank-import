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
* 108 participants (54 pairs) 
* 4 images x 6 blocks (24 trials)
* interactive chat

human speaker - model listener:
* 57 participants 
* single message / trial
* again 4 images x 6 blocks 

model speaker - human listener (reported in Appendix) is not imported here


## Processing/import
Data pulled from github. 

Images are from COCO, we do not copy the images to refbank (but could consider), 
but they can be found in COCO. 

We have text and selections. (we don't include model selections)

We don't have demographics (might exist?)