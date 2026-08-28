library(tidyverse)
library(here)
library(antiword)
library(readxl)
library(janitor)
raw_data_loc <- "import/bangerter2020_lexical/raw_data"


### For figuring out conditions
# * In experiment 1, there are games numbered 1-16 and then lettered A-F, but we expect 14 in one condition and 8 in the other as per paper
# ---Pairs 9-16 are in the condition with 8 pairs, Pairs 1-8 and A-F are in the condition with 14 pairs
#
# * In experiment 2, there are games 1-24 and then A-F, paper reports 15 in each condition.
# ---Here’s the breakdown by condition (A-F is groups 20 onwards)
# ---- Classic Condition Pairs 3       4            5            6            7             8            11          16          20          21          22          24               25          27          28
# ---- New Cards Condition Pairs 1               2            9            10          12          13          14          15          17          18          19               23          26          29          30
#
# * In experiment 3, there are games 1-24, paper reports 12 in each condition.
# ----1-12=new cars, 13-24=classic
#
#
# Best,
# Adrian

# source(here("import/bangerter2020_lexical/prep_segment.R"))
all <- read_csv(here("import/bangerter2020_lexical/raw_data/all_text.csv"))

alignment <- all |> select(source, role, person, expt, rep, gameid, game, grid, message_id_num)

tagged_transcripts <- read_csv(here("import/bangerter2020_lexical/raw_data/segmented_transcript.csv"))

messages <- tagged_transcripts |> fill(game)|> left_join(alignment) |> 
  mutate(action_type="message",
         message_irrelevant=ifelse(targetPosition_w=="chitchat", T, NA),
         targetPosition_w=as.numeric(targetPosition_w),
         targetPosition_w=case_when(targetPosition_w==9 ~ 8, T ~ targetPosition_w), # coding error
         trial_num=(grid-1)*8+targetPosition_w,
         round_num=grid,
         player_id=str_c(gameid, "_", person)) |>
  group_by(gameid,round_num) |> 
  fill(trial_num, .direction="downup") |> 
  group_by(gameid, trial_num) |> 
  mutate(message_num=row_number() |> as.numeric()) |> 
  ungroup() |> 
  filter(!is.na(role), !is.na(player_id)) |> 
  select(action_type, gameid, player_id, role, text=message, message_irrelevant, trial_num, round_num, expt, message_num)

# role is constant per person within a game (whoever describes, describes all game), so
# the real describer's player_id for a trial missing a describer transcript is recoverable
# from that game's other describer rows -- rather than fabricating a placeholder identity,
# which would show up as a phantom extra player in the game.
game_describer <- messages |> filter(role=="describer") |> distinct(gameid, player_id)

dummy_messages <- messages |> filter(role=="matcher") |> distinct(gameid, trial_num, round_num, expt) |>
  anti_join(messages |> filter(role=="describer") |> distinct(gameid, trial_num, round_num, expt)) |>
  left_join(game_describer, by="gameid") |>
  mutate(role="describer",
         text=NA,
         action_type="message",
         message_num=NA)

combined <- messages |> bind_rows(dummy_messages) |> 
  mutate(
    condition_label = case_when(
      expt == 1 & gameid %in% c("1_G9", "1_G10", "1_G11", "1_G12", "1_G13", "1_G14", "1_G15", "1_G16") ~ "expt1_classic",
      expt == 1 ~ "expt1_new",
      expt == 2 & gameid %in% c("2_3", "2_4", "2_5", "2_6", "2_7", "2_8", "2_11", "2_16", "2_20", "2_21", "2_22", "2_24", "2_A", "2_C", "2_D") ~ "expt2_classic",
      expt == 2 ~ "expt2_new",
      expt == 3 & gameid %in% c("3_13", "3_14", "3_15", "3_16", "3_17", "3_18", "3_19", "3_20", "3_21", "3_22", "3_23", "3_24") ~ "expt3_classic",
      expt == 3 ~ "expt3_new"
    ),
    dataset_id = "bangerter2020_lexical",
    full_cite = "Bangerter, A., Mayor, E., & Knutsen, D. (2020). Lexical entrainment without conceptual pacts? Revisiting the matching task. Journal of Memory and Language, 114, 104129.",
    short_cite = "Bangerter et al. (2020)",
    group_size = case_when(
      expt == 3 ~ 3,
      T ~ 2
    ),
    language="French",
    prior_relationship="no",
    partner_constancy=case_when(expt==3 ~ "no", T ~ "yes"),
    population="adult",
    role_constancy = "yes",
    confederates="no",
    modality="oral-in-person",
    feedback="none",
    backchannel="full",
    room_num=1,
    stage_num=case_when(
      expt==1 ~ 1,
      expt==2 & round_num==6 ~ 2,
      expt==2 ~ 1,
      expt==3 & round_num<=4 ~ 1,
      expt==3 & round_num>=5 ~2
    ),
    image_options="unk1;unk2;unk3;unk4;unk5;unk6;unk7;unk8",
    target_image="unk1",
    exclude=NA,
    exclusion_reason=NA_character_,
    order_match="order",
    gender=NA_character_,
    native_language="French",
    race=NA_character_,
    age=NA_real_,
    time_stamp=NA_real_,
    education="some-college",
    selected_image=NA) |> 
  rename(game_id=gameid) |> 
  select(-expt)
    
  


source("validate.R")
validate_dataset(combined, write = T)
