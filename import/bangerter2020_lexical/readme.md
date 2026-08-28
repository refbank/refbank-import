# Readme

## Citation

Bangerter, A., Mayor, E., & Knutsen, D. (2020). Lexical entrainment without conceptual pacts? Revisiting the matching task. Journal of Memory and Language, 114, 104129.

## Abstract

Conversational partners who repeatedly refer to the same objects require less and less collaborative effort to do
so. This is due to lexical entrainment, the fact that they come to re-use the same words. Lexical entrainment may
reflect the elaboration of conceptual pacts, partner-specific agreements about how to name objects which belong
to the conversational partners’ common ground. Can lexical entrainment occur even if conversational partners
cannot develop conceptual pacts about specific objects? In three experiments, we investigated whether lexical
entrainment occurs in the matching task even when cards change over trials and partners are not able to develop
pacts. We compared two conditions: a classic condition where cards remained the same for each trial, and a new
cards condition where cards changed on each trial. Lexical diversity decreased for pairs in the new cards con-
dition (albeit less than for classical pairs); inconsistent reductions in collaborative effort were also observed.
Pairs in the new cards condition also were better able to adapt to novel referring situations (involving novel
stimuli or new interaction partners) than classic pairs. The results suggest that lexical entrainment in the
matching task may be due in part to factors other than the elaboration of conceptual pacts. These may include
the development of an overarching meta-perspective on shared features of cards, reflecting category learning
processes resulting from reference negotiation

## Study details

3 experiments, each has 2 conditions "new cards" and "classic"

in classic the same 8 cards are used each rep (but which 8 varies / pair)
in new, each card only occurs once 

procedure:
* in person, with visual divider 

expt 1: 
* 14 pairs in new, 8 pairs in classic

expt 2: 
* adds a 6th block that is new for everyone
* 15 pairs in each condition

expt 3: 
* partner switch design!
* D and M1 do 4 blocks, then D and M2 do 4 more blocks
* 12 triads in new and 12 in classic 




## Processing/import

* native French speakers from a swiss university, strangers -- this applies
  to all three experiments, including expt 3's triads (recruited from the
  same student-body pool the same way as expt 1/2), not just the two
  experiments whose Methods sections restate it verbatim
* feedback = none for expt 3 is assumed, not independently confirmed --
  expt 3's Method section doesn't restate the no-feedback statement given
  for expt 1, but it does describe expt 3 as using "similar" materials and
  procedure to expt 1/2, so we're carrying the no-feedback assumption over
  rather than treating it as a documented fact for expt 3 specifically

* we do not have target <-> trial information, but do have the *overall* pools of images
(so with a lot of work, it might be possible to sort out)

* we do not have choice info

**Bug fixed (2026-08-27):** `player_id` was set directly to the raw `person`
column (values "A"/"B"/"C" -- a role label within a game, not a unique ID),
with no `gameid` prefix. This collapsed all 76 games' participants into just
3 global player_ids across the whole dataset, instead of one distinct set
per game -- caught because it produced an impossible Games > Players count
in a downstream summary table (Table 2 of the refbank dataset paper).
Fixed by namespacing `player_id` as `gameid_person` (matching the convention
every other dataset in this project uses); now 176 distinct players across
76 games (~2.3/game, consistent with the 2-3 person group sizes).

