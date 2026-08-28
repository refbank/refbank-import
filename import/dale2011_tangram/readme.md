# Readme

## Citation

Dale, R., Kirkham, N. Z., & Richardson, D. C. (2011). How two people become a tangram recognition system. In Proceedings of the European Conference on Computer-Supported Cooperative Work.

## Abstract

In the tangram task, two participants have the same set of abstract shapes, set
out in different orders. One participant must instruct the other to arrange their 
shapes so that the orders match. In the course of the task, participants must find
a way to refer to these abstract shapes. In our experiment, we tracked the eye 
movements of two participants engaged in a computerized version of the task. We 
found the canonical tangram effect: participants became faster at completing the 
task from round 1 to round 3. Also, eye-movements synchronize over time. We used 
cross recurrence analysis to quantify this coordination, and use it to show that 
as their words coalesce, their actions approximate a single coordinated system.

## Study details

20 pairs (40 participants) (we have 20 games imported)
6 targets x 3 rounds
Study is more about eye and mouse tracking (and alignment thereof), but we do not include eye or mouse data
* role constant throughout experiment
* paper does not report whether any feedback was given; we assume not given what is typical in ordering experiments
* paper does not report whether participants knew each other beforehand; we assume no prior relationship since it's not mentioned
* paper does not state the language used; we assume English based on the (English-speaking) university setting
* communication between describer and matcher was oral but remote ("Participants communicated through hands-free headsets which used an intercom feature on 2.4Ghz wireless phones.")
* paper does not explicitly report whether matchers were allowed to talk or not, we assume they were given how intercoms generally work

## Processing/import

We have selection information. We are confirmed not to have text data, and we don't seem to have images or demographics. 

DATA STRUCTURE NOTES (from email)
File naming: tan[round].[dyad].dat
Columns (multivariate time series):
   1. tangram_mouse: tangram moused over by matcher (1-6 valid, -1 = failed mouse data)
   2. correct_tangram: the correct tangram number (1-6)
   3. turn_time: time in milliseconds
   4. director_eye: tangram fixated by director (1-6 valid, 10 = not fixating, 11 = tracker lost)
   5. matcher_eye: tangram fixated by matcher (1-6 valid, 10 = not fixating, 11 = tracker lost)
 Each row is a time sample (not a trial)
 No transcripts available - study focused on perceptuomotor dynamics
