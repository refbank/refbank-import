# Readme

## Citation
Ji, A., Bergey, C. A., Eliav, R., Artzi, Y., & Hawkins, R. D. (2025). Ad hoc conventions generalize to new referents. arXiv preprint arXiv:2509.05566.

## Abstract

     How do people talk about things they've never talked about before? One view suggests that a new shared naming system establishes an arbitrary link to a specific target, like proper names that cannot extend beyond their bearers. An alternative view proposes that forming a shared way of describing objects involves broader conceptual alignment, reshaping each individual's semantic space in ways that should generalize to new referents. We test these competing accounts in a dyadic communication study (N=302) leveraging the recently-released KiloGram dataset containing over 1,000 abstract tangram images. After pairs of participants coordinated on referential conventions for one set of images through repeated communication, we measured the extent to which their descriptions aligned for undiscussed images. We found strong evidence for generalization: partners showed increased alignment relative to their pre-test labels. Generalization also decayed nonlinearly with visual similarity (consistent with Shepard's law) and was robust across levels of the images' nameability. These findings suggest that ad hoc conventions are not arbitrary labels but reflect genuine conceptual coordination, with implications for theories of reference and the design of more adaptive language agents. 

## Study details

We have 3 studies:

All three studies recruit from Prolific -- we assume adults based on this (paper doesn't state this explicitly).

expt1 (Eliav 2023 expt 1; called pilot 1 in appendix of Ji et al 2025): 
* participants from Prolific 
* 60 pairs, including 9 excluded (we have 60)
* each pair has a context of 10 tangrams, 5 high-nameability, 5 low nameability
* half of images are targets each trial, half only targets once 
* 5 blocks of 6 trials each 
* roles swap each block 
* describer sends single message (45 second limit; + 15 seconds for matcher to select)
* --> no backchannel
* feedback: "Both participants received feedback after each trial indicating if the listener had responded correctly."

expt2 (Eliav 2023 expt 2; called pilot 2 in appendix of Ji et al 2025)
* presumably from Prolific
* 60 pairs, including 8 excluded pairs (we have 59)
* same as expt 1 above, plus a 6th round that was added with 10 new stims
* roles swap each block 
* describer sends single message (45 second limit; + 15 seconds for matcher to select)
* --> no backchannel
* feedback: "Both participants received feedback after each trial indicating if the listener had responded correctly."


expt3 (main experiment in Ji et al 2025): 
* recruitment via Prolific 
* 163 pairs, of whom 12 excluded (we have 151, suggesting post-exclusions)
* each game has 10 targets
* 5 blocks of 6 targets each; 5 targets occur every block, other 5 only occur once 
* roles swap every block
* describer has 45 seconds, but messages reset timer to 30 seconds
* backchannel: both partners can use chatbox freely
* feedback: "After the listener selected a tangram, both participants would receive feedback on whether the target was correctly selected."

## Processing/import
we only import the interactive trials (not pre and post tests)

We have images, text, and responses. 

We seem to have time_stamps for the selections and for the expt 1 and 2 messages, but not for expt 3 messages.

These data are available at https://github.com/lil-lab/tangrams-ref/tree/main but at least for
main experiment = experiment 3, it's processed slightly differently in a way that makes it hard to extract the sequence of 
describer/matcher messages. 

The demographics (https://github.com/lil-lab/tangrams-ref/blob/main/data/demographics.csv) 
seem to interface with the full data on the github but don't have the same types of ids that we have. 
Demographics are potentially importable, but since it would take significant wrangling, we'll hold off until there's a use case. 