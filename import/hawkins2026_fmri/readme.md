# Readme

## Citation

Hawkins, R. D., et al (2026) Unpublished hyperscanning study.

## Abstract

NA

## Study details

19 pairs of participants did the following (in fmri scanner)

* pre exposure round (look at images w/o communication)
* round 1: refer to all 36 images 
* rounds 2-7: 6 rounds of the same 18 images each round (random half of the 36 -- pairs have reversed halves)
* round 8: refer to all 36 images again
* post exposure round (look at the images again)

we import behavioral data from rounds 1-8

## Processing/import
Response information comes from empirica; 

* audio communication came from raw audio through an auto transcription + hand labelled of speaker/listener and
hand identification of trial boundaries
note: it was in an frmi so is noisy; some clean up has occurred, but there are still some 
mistranscriptions & may also be diarization errors 

Some pairs of participants had prior relationship -- we have marked it as mixed.

From the experiment code, full feedback after each trial. 

Age and gender information is available, but requires linking which participant was in which scanner. 

Other condition-level fields not stated in any write-up (there isn't one), confirmed
directly instead:
* group_size = 2 -- counted directly from the task code (always exactly one describer,
  one listener per game)
* language = English -- based on the (English) transcripts
* population = adult -- confirmed by the dataset owner
* partner_constancy = yes -- confirmed by the dataset owner (same two participants for
  the whole session)
* confederates = no -- confirmed by the dataset owner
* backchannel = full -- confirmed by the dataset owner (matches the fully-open audio
  intercom link between the two scanners)