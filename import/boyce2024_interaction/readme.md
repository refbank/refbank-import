# Readme

## Citation

Boyce, V., Hawkins, R. D., Goodman, N. D., & Frank, M. C. (2024). Interaction structure constrains the emergence of conventions in group communication. Proceedings of the National Academy of Sciences, 121(28), e2403888121.

## Abstract

Real-world communication frequently requires language producers to address more
than one comprehender at once, yet most psycholinguistic research focuses on 
one-on-one communication. As the audience size grows, interlocutors face new 
challenges that do not arise in dyads. They must consider multiple perspectives 
and weigh multiple sources of feedback to build shared understanding. Here, we 
ask which properties of the group’s interaction structure facilitate successful 
communication. We used a repeated reference game paradigm in which directors 
instructed between one and five matchers to choose specific targets out of a set
of abstract figures. Across 313 games (N = 1,319 participants), we manipulated 
several key constraints on the group’s interaction, including the amount of feedback
that matchers could give to directors and the availability of peer interaction 
between matchers. Across groups of different sizes and interaction constraints, 
describers produced increasingly efficient utterances and matchers made increasingly
accurate selections. Critically, however, we found that smaller groups and groups 
with less-constrained interaction structures (“thick channels”) showed stronger
convergence to group-specific conventions than large groups with constrained 
interaction structures (“thin channels”), which struggled with convention formation. 
Overall, these results shed light on the core structural factors that enable 
communication to thrive in larger groups.

## Study details

A set of web-based iterated reference games 

paper reports 313 games, we have 342 in dataset (laxer exclusions + pilots). 
* all games included 6 rounds of 12 targets/trials each (to the same 12 targets)
* games varied in group size 2-6
* feedback and backchannels varied across conditions 

* participants from Prolific 
* group size: coded as assigned, note that actual group size may be smaller in the case of expt3
* partners are always constant (except if people drop out in expt 3, then group gets smaller)
* role constancy: either rotates by block (every 12 trials) (most cases), or never rotates (thick and no-rotate) (except experiment 3 if the speaker drops out, it's accomodated)
* backchannel: text box freely except for emoji/thin conditions in which case limited to four emoji buttons
* feedback: describer always sees who chose what; matchers sometimes just see if they were right or wrong (most cases); 
in full-feedback and thick, matchers see who chose what, the correct answer, and total correct

## Processing/import
* we record group size as intended, but note that in some expts games could start or continue with partial groups, so group size may not equal actually number of people participating at a given time (could revisit)
* there are some errors/oddities in Empirica (ex a few clearly incorrect time stamps, or games that were in weird non-working states) which we normalize or exclude 
* files are all pulled from (processed) data on the github repo of the original expt
* demogs are pre-processed in a separate .R file to make a demogs.csv
* pilot pre-processing is also split off in a separate file (sourced in import.R)
* timing info generally available for selections, but not for messages
Text, selections, demographics, and images are all included. 