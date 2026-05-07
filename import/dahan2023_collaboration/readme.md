# Readme

## Citation
Dahan, D. (2023). Collaboration under uncertainty in unscripted conversations: The role of hedges. Journal of Experimental Psychology: Learning, Memory, and Cognition, 49(2), 320.

and 

Dahan, D. (2025). When hedging helps, rather than impedes, communication: collaboration in the referential communication task. Discourse Processes, 62(2), 89-111.

## Abstract

Dahan 2023:
The present study examined the role of hedges in a referential communication task.
Pairs of participants received an identical set of cards, each card displaying a 
geometric configuration (a “tangram”). One participant, the director, instructed 
their partner, the matcher, to reproduce a series of predetermined tangram sequences
using their own cards. Directors sometimes included a hedge in their description 
of a tangram (e.g., “looks kinda like an eagle”), and more so the first time than 
on subsequent mentions. The present study tested the hypothesis that, by revealing
their uncertainty regarding the adequacy of their description in conveying the 
intended meaning, directors signal a possible difficulty in establishing reference. 
This in turn prompts their addressee to display, rather than merely assert, their 
understanding (by presenting a description for the tangram the matcher believes 
the director is referring to, for the director to evaluate). Analyses of matchers’
responses to descriptions that directors had hedged or not confirmed the hypothesis. 
This finding supports the view that conversational partners work together to reach
the mutual belief that they have coordinated what the speaker means and what their 
addressee takes them to mean. Conversational partners expend more joint effort when 
they deem the risk of misunderstanding to be high than when it is perceived to be low.

Dahan 2025:
In a referential communication task, where one participant, the director, instructs their partner, 
the matcher, to reproduce sequences of cards displaying geometric configurations (tangrams), 
directors sometimes include a hedge to their description when the tangram is hard to describe 
(e.g. “like an eagle, kinda”). Directors hedge their descriptions, I argue, to invite matchers 
to participate in the referring process by offering a candidate description, thereby mitigating
the risk of misunderstanding. This claim was examined in a corpus with large variability in 
matchers’ referential accuracy. Analysis of the matcher’s choices of referent on each trial 
revealed that their director’s propensity to include a hedge in their descriptions over the
course of the task was a significant predictor of the matcher’s success at the task. 
This finding supports the view that successful communication benefits from the actions 
that both conversational partners take to jointly establish the mutual belief that, for
each utterance, the addressee has understood what the speaker meant.

## Study details

Participants:
* (2023): 40 participants from university class (half played with classmate, half with a friend they brought)
* (2025): 40 participants recruited via Craiglist (half paired with each other, half brought a friend to play with)

Procedure (both):
* play face to face with visual barrier
* set of 16 cards (either Set 1 or Set 2)
* each rep there is a target sequence of 3: (marked as red / blue / green color boxes)
* switch roles ever 4 reps (12 trials)
* 48 total trials (16 targets x 3 each)
* no feedback on errors 

Targets: tangrams

## Processing/import

We received raw audio files and transcribed them using whisper (or similar); 
a contractor identified which target number each was referring to, tagged (highly) irrelevant messages, and
IDed what messages came from describer or matcher based on the transcripts.

We have classified this as order -- but it's really a pick 3 in order out of 16, which is different.
Because they could revisit previous choices within the rep, we've called it order. 

We theoretically have timing info because we have the recordings, but we don't have it per-trial without more work. 

For prior_relationship -- there is for sure a prior relationship in half of them (friends), 
probably for another quarter (classmates), and not for the last quarter. We've labelled it as yes because we can't tell which are which. 

Native language is presumably English in most cases; and at least for the 2023 data, education is presumably mostly "some-college", but we haven't marked this because it's uncertain. 

We have target images. I don't think these are in kilogram, but we could check. 

