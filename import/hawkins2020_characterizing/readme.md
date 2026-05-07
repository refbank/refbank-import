# Readme

## Citation

Hawkins, R. D., Frank, M. C., & Goodman, N. D. (2020). Characterizing the dynamics of learning in repeated reference games. Cognitive science, 44(6), e12845.

## Abstract

The language we use over the course of conversation changes as we establish common
ground and learn what our partner finds meaningful. Here we draw upon recent advances
in natural language processing to provide a finer-grained characterization of the 
dynamics of this learning process. We release an open corpus (>15,000 utterances) 
of extended dyadic interactions in a classic repeated reference game task where 
pairs of participants had to coordinate on how to refer to initially difficult-to-describe
tangram stimuli. We find that different pairs discover a wide variety of idiosyncratic 
but efficient and stable solutions to the problem of reference. Furthermore, these 
conventions are shaped by the communicative context: words that are more discriminative 
in the initial context (i.e., that are used for one target more than others) are more 
likely to persist through the final repetition. Finally, we find systematic structure
in how a speaker’s referring expressions become more efficient over time: Syntactic 
units drop out in clusters following positive feedback from the listener, eventually 
leaving short labels containing open-class parts of speech. These findings provide 
a higher resolution look at the quantitative dynamics of ad hoc convention formation 
and support further development of computational models of learning in communication.

## Study details

Note that this paper reports on 2 similar experiments:
* one uses an ordering paradigm so doesn't have pre-marked divisions for what text goes with which target 
* one uses a matching paradigm

Ordering experiment:
* 218 recruited 
* 6 rounds of 12 targets each
* 56 games post exclusions
* free chat via textbox
* feedback limited (after each set of 12 "batched feedback on their score (out of 12)")

Matching experiment:
* 268 recruited
* 6 rounds of 12 targets each
* 83 games post exclusions (we have 262 player ids, 131 games, suggesting pre-exclusions)
* free chat via textbox
* full feedback ("participants were given full, immediate feedback: The director saw which tangram their partner clicked, and the matcher saw the intended tangram.")


## Processing/import
We pull data from github.

We have images, selections, messages. We do not have demographics (but they might exist?). 

We don't import timing info because we can't figure out how to process the timing info in the source. 
we are unsure of timestamps -- there are both msgTime and timeElapsed, 
but they don't obviously line up, so not sure we have reliable time to message
for selections, there is a time column, but I don't think we have a start of trial indicator to baseline with.

For the ordering experiment: Identification of what target position goes with each trial (and tagging of extremely irrelevant messages) was done by a contractor. 

