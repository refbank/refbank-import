# Readme

## Citation

Hawkins, R. D., Franke, M., Frank, M. C., Goldberg, A. E., Smith, K., Griffiths, T. L., & Goodman, N. D. (2023). From partners to populations: A hierarchical Bayesian account of coordination and convention. Psychological Review, 130(4), 977.

## Abstract

Languages are powerful solutions to coordination problems: they provide stable, 
shared expectations about how the words we say correspond to the beliefs and 
intentions in our heads. Yet language use in a variable and non-stationary social
environment requires linguistic representations to be flexible: old words acquire
new ad hoc or partner-specific meanings on the fly. In this paper, we introduce 
CHAI (Continual Hierarchical Adaptation through Inference), a hierarchical Bayesian
theory of coordination and convention formation that aims to reconcile the long-standing
tension between these two basic observations. We argue that the central computational
problem of communication is not simply transmission, as in classical formulations,
but continual learning and adaptation over multiple timescales. Partner-specific
common ground quickly emerges from social inferences within dyadic interactions,
while communitywide social conventions are stable priors that have been abstracted
away from interactions with multiple partners. We present new empirical data alongside
simulations showing how our model provides a computational foundation for several 
phenomena that have posed a challenge for previous accounts: (1) the convergence 
to more efficient referring expressions across repeated interaction with the same
partner, (2) the gradual transfer of partner-specific common ground to strangers, 
and (3) the influence of communicative context on which conventions eventually form.

## Study details

The paper has other simulations and experiments, but the only one relevant enough to import
is experiment 2 in the paper (labeled as experiment 3 in the github)

From the paper:
* recruitment from AMT -- we assume adults, no prior relationship, and no confederates based on this; English based on the (English) transcripts (paper doesn't state these explicitly)
* 92 participants in one of 23 fully connected networks of 4 people (we have 30 networks, presumably including exclusions)
* Each network gets one of 3 sets of contexts (images)
* Roles swapped each block, (4 blocks of 4 trials each) x 3 partners
* backchannel: free chat between the two members each block
* group size is coded as 4 because of network size, but is functionally pairs (at least within each stage)
* feedback full "both participants in a pair were given full feedback on each trial about their partner’s choice"

## Processing/import
We pull the data from github

Distractor identities were somewhat rederived based on the idea that the distractors are labeled in order from what's left of the set
see https://github.com/hawkrobe/conventions_model/blob/master/reference_game/experiment.py#L102 for why we think that

We have images, text and selections. We do not have demographics (although they may be available)
