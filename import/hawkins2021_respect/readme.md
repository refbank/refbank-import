# Readme

## Citation
Hawkins, R., Liu, I., Goldberg, A., & Griffiths, T. (2021). Respect the code: Speakers expect novel conventions to generalize within but not across social group boundaries. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 43, No. 43).

## Abstract

Speakers use different language to communicate with partners in different communities. 
But how do we learn and represent which conventions to use with which partners? 
In this paper, we argue that solving this challenging computational problem requires
speakers to supplement their lexical representations with knowledge of social group 
structure. We formalize this idea by extending a recent hierarchical Bayesian model
of convention formation with an intermediate layer explicitly representing the latent 
communities each partner belongs to, and derive predictions about how conventions 
formed within a group ought to extend to new in-group and out-group members. We 
then present evidence from two behavioral experiments testing these predictions 
using a minimal group paradigm. Taken together, our findings provide a first step
toward a formal framework for understanding the interplay between language use and
social group knowledge.

## Study details

(Experiment 1)
* recruitment from Prolific -- we assume adults, no prior relationship, and no confederates based on this (paper doesn't state these explicitly)
* 272 participants recruited, ended up with 33 groups of 4 (= 132 participants)
(imported dataset has 45 games and 178 players; applying exclusions we get 37 games;
possibly there were additional exclusions based on post-test that we don't import)
* each group has 1 of 2 target image sets
* swap roles at start of each block
* (4 trials x 4 blocks) x 3 partners in network 
* coded as groups of 4, but pairwise in each interaction 
* free communication via chatbox with partner
* 45 second timer per trial
* feedback: full ("both participants in a dyad were given full feedback")

## Processing/import
We do not include post-phase test. We have the text, selections, and images. We do not have demographics, although they might exist. 

This was a somewhat messy import. 

note that for player-role, original code implies that games.csv is canonical
the raw-chat has a few errors!
we assume errors are limited to chat metadata -- it appears to be at the ends of things,
so perhaps is getting meta-data from the next trial?
we assume chat messages are correctly assigned to trial and that playerid is correct which seems to be supported by spot checks
we also have to retrieve who the listener is in each room-game to get the right player labels on the selections

identify cases where we are missing speaker info because they didn't talk, these seem to be cases where we just don't have speaker info (incomplete games?), 
delete these 179 rows for now

 according to paper, after excluding incomplete, 33 groups, 132 participants
 this appears to be the exclusion used, but this keeps 37 (which does line up with the number of games in cleaned_messages)
 I can't identify other exclusion used -- possibly post-test something?