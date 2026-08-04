# Readme

## Citation

Bögels, S., Li, T., Rasenberg, M., Eijk, L., Toni, I., & Pouw, W. (2026). There is a power law of joint communicative effort and it reflects communicative work. Cognition, 268(10637), 0.

## Abstract


A drive towards efficiency seems to regulate communicative processes and ultimately
language change. In line with efficiency principles, signed, spoken, and/or gestural
utterances tend to reduce in overall effort over repeated referrals in referential 
tasks. Although theories generally assume multimodality and interaction, this process
has mostly been operationalized as individual effort in a single communicative modality.
Here we seek to understand reduction of communicative effort in its natural environment,
i.e. during multimodal and collaborative face-to-face dialogues about displaced 
referents. We ascertain that the reduction in joint effort (y) over repeated 
referrals (x) follows a negative power relationship, y = a*x^c, where a and c 
are constants. This reduction in communicative effort is multimodal, occurring 
across gesture, speech, prosody, and turn taking, and it is interactive, based 
on joint effort. The pattern is robust, being confirmed through reanalyses of 
published datasets about (individual) effort reduction. Crucially, the pattern 
is communicatively relevant. The coefficient of the power relationship predicts 
change and convergence in interlocutors' conceptualizations of the communicative
referents over the interaction. The negative power relationship reflects therefore
how effort translates into mutual understanding - a process we call communicative
work. We suggest that the power function captures an exploration-exploitation 
trade-off during human dialogue which emerges from multiscale processes. Joint
conceptualization of novel referents benefits from early conceptual exploration 
followed by later exploitation of selected signals. The current report proposes
a novel ‘power law of joint communicative work’ that is relevant for linguistic
theory, agent-based modeling, and experimental psychology.

## Study details

This is part of the CABB dataset (although this is the paper where there is
a release of relevant-to-refbank data)

* 42 pairs (84 participants) (we have data for 47 -- matching how many have good transcription mentionedin Eijk et al 2022)
(Eijk et al 2022 suggests that there is data for more pairs (not released in as part of this paper?, possibly due to lower transcription quality or other missing data))
* in interactive portion, see the 16 Fribbles (=target shapes)
* describer has one highlighted, matcher has to identify
* roles switch each trial, (Eijk et al 2022: "After completing the Referential and Localisation tasks for one Fribble, participants switched roles for the next trial.")
* 16 targets x 6 blocks 
* full backchannel: Eijk et al 2022: "They were informed that they could communicate in any way they wanted (without explicitly mentioning speech and gesture)"
* correctness feedback is not mentioned (in either paper), so we presume that it was not given? 

each trial has both a identify the one being talked about and figure out if it's in the same place on the screen
From Eijk et al 2022: "Both participants saw the same 16 Fribbles in the same general spatial layout, but 50% of the Fribbles were not positioned in the same locations within this layout (see Fig. 4). On each trial, one of the 16 Fribbles was marked by a red square (the target for that trial) for one of the two participants (the “Director”)."
we're going to drop the localization part

there's also pre-tests and other measurements that we do not include 

## Processing/import
We have choice data, but don't know how to identify the target-selection alignment when participants (rarely)
get it wrong (but this could be figured out in future work) `given_answer`/`correct_answer` use a rotating per-round label scheme (e.g. "13", "F", "D"),
not the fixed f1-f16 target numbering, so we can only recover `selected_image` when the
matcher was correct (there it must equal the target). For the small number of trials
(34) where the matcher answered incorrectly, we know a selection was made but not which
image it names, so `selected_image = "unk"`. 

We have text data. 

We do not have the target -- image mapping (although it might be possible)



We do not have demographics (although we haven't asked)
