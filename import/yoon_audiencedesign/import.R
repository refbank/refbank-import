library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))


raw_data_dir <- "import/yoon_audiencedesign/raw_data/"

# TO ask for
# experimental materials if they have them -- images, trial order, distractors
# or just code for running expt

# if they have matcher's selections anywhere (for sort and/or cued)


# From paper, expt 1 has 28 groups of 4
# D & M1 do rounds of ordering 16 (x 4) -- appears to be open convo
# in some cases D & M2 do the same thing (2KN1)
# for test, D and some number of matchers (1 or 3)
# cued cued 1/4 choice (3 familiar, 1 new)
# Each old image was referred once during the test phase
# for a total of 16 critical and 8 filler trials (24 test trials total in each block).
# These experimental conditions were manipulated in a blocked,
# within-subjects design. Four different sorting-test blocks rotated
# each participant group through the four conditions;
# the order of blocks was counterbalanced across groups.
# A different set of 16 images was used in each of the four blocks of the task.
# Participants maintained their role (Director, Matcher 1, Matcher 2, Matcher 3)
# across all four blocks. The entire task lasted approximately 1.5 hr.
# we have post-exclusions here -- when last target wasn't described in sort, that
# whole sort round was excluded
# random tech difficulty exclusions in cued

# TrialID seems to correspond to target image at least
sort_expt1 <- read_tsv(here(raw_data_dir, "E1_sorting.txt")) |>
  rename(trialID = TrialID) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, image_set) |>
  mutate(presumed_view_order = row_number()) |>
  ungroup()

image_sort_sets <- sort_expt1 |>
  ungroup() |>
  select(image_set, trialID) |>
  unique() |>
  group_by(image_set) |>
  summarize(images = str_c("image_", trialID, collapse = ";"))

sort_expt1_clean <- sort_expt1 |>
  left_join(image_sort_sets) |> 
  mutate(
    gameid = str_c("expt1_group", subID, "_images", image_set),
    playerid = str_c("expt1_group", subID, "_d"),
    rep_num = ifelse(partner=="M1", round, 4+round),
    stage_num = ifelse(partner=="M1", 1, 2),
    trial_num = presumed_view_order,
    condition=str_c("expt1_",partner, "_sort"),
    image=str_c("image_", trialID),
    group_size=4, #TODO figure out how to handle group size here!!
  ) |> 
  select(gameid, playerid, rep_num, trial_num, images, image, stage_num, 
         text=transcription, condition, group_size)

cued_expt1 <- read_tsv(here(raw_data_dir, "Experiment1.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>   
  mutate(
    gameid = str_c("expt1_group", subID, "_images", image_set),
    playerid = str_c("expt1_group", subID, "_d"),
    images=str_c("image_", trialID, ";unk1;unk2;unk3"),
    image=str_c("image_", trialID),
    rep_num=case_when(
      condition=="2(2K1N)"~ 9,
      gameid=="expt1_group5_images2" ~ 9, #we expect that only in the above condition was there an M2 stage, but there's an anomaly
      T ~ 5),
    stage_num=case_when(
      condition=="2(2K1N)"~ 3,
      gameid=="expt1_group5_images2" ~ 3, # see above for this anomoly,
      T ~ 2),
    trial_num=(rep_num-1)*16+presumed_view_order,
    condition=str_c("expt1_",condition, "_match"),
    group_size=4) |> 
  select(gameid, playerid, rep_num, trial_num, images, image, stage_num,
         text=transcription, condition, group_size)
  
all_1 <- sort_expt1_clean |> bind_rows(cued_expt1)

# Expt 2
# 35 groups of 5
# 5 blocks of image stims, including the 4 from expt 1
# otherwise similar


sort_expt2 <- read_tsv(here(raw_data_dir, "E2_sorting.txt")) |>
  rename(trialID = TrialID) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, image_set) |>
  mutate(presumed_view_order = row_number()) |>
  ungroup()

image_sort_sets <- sort_expt2 |>
  ungroup() |>
  select(image_set, trialID) |>
  unique() |>
  group_by(image_set) |>
  summarize(images = str_c("image_", trialID, collapse = ";"))

sort_expt2_clean <- sort_expt2 |>
  left_join(image_sort_sets) |> 
  mutate(
    gameid = str_c("expt2_group", subID, "_images", image_set),
    playerid = str_c("expt2_group", subID, "_d"),
    rep_num = case_when(
      partner=="M1"~ round,
      partner=="M2" ~4+round,
      partner=="M3" ~ 8+round),
    stage_num = case_when(
      partner=="M1"~ 1,
      partner=="M2" ~ 2,
      partner=="M3" ~ 3),
    trial_num = presumed_view_order,
    image=str_c("image_", trialID),
    condition=str_c("expt2_",partner, "_sort"),
    group_size=5, #TODO figure out how to handle group size here!!
  ) |> 
  select(gameid, playerid, rep_num, trial_num, images, image, stage_num, 
         text=transcription, condition, group_size)

cued_expt2 <- read_tsv(here(raw_data_dir, "Experiment2.txt")) |> 
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>   
  mutate(
    gameid = str_c("expt2_group", subID, "_images", image_set),
    playerid = str_c("expt2_group", subID, "_d"),
    images=str_c("image_", trialID, ";unk1;unk2;unk3"),
    rep_num=case_when(
      condition=="2(3K1N)"~ 13,
      condition=="3(2K2N)"~ 9,
      T ~ 5),
    stage_num=case_when(
      condition=="2(3K1N)"~ 4,
      condition=="3(2K2N)"~ 3,
      T ~ 2),
    trial_num=(rep_num-1)*16+presumed_view_order,
    image=str_c("image_", trialID),
    condition=str_c("expt2_",condition, "_match"),
    group_size=5) |> 
  select(gameid, playerid, rep_num, trial_num, images, image, stage_num, 
         text=transcription, condition, group_size)

all_2 <- sort_expt2_clean |> bind_rows(cued_expt2)

# Expt 3
# 14 groups of 7
# sorting task done 5 times (note this was in booklet so presumably no acc data)
# go through 4 times with one Director
# then through all again with another director (different images)
# should spot check if these trials align with earlier expt trials!

sort_expt3 <- read_tsv(here(raw_data_dir, "E3_sorting.txt")) |>
  rename(trialID = TrialID) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, image_set) |>
  mutate(presumed_view_order = row_number()) |>
  ungroup()

image_sort_sets <- sort_expt3 |>
  ungroup() |>
  select(image_set, trialID) |>
  unique() |>
  group_by(image_set) |>
  summarize(images = str_c("image_", trialID, collapse = ";"))

sort_expt3_clean <- sort_expt3 |>
  left_join(image_sort_sets) |> 
  mutate(
    gameid = str_c("expt3_group", subID, "_images", image_set),
    playerid = str_c("expt3_group", subID, "_d"), #if we ever get matcher stuff, note that a matcher becomes a director with a different number since each group of 7 does everything twice
    rep_num =  round,
    trial_num = presumed_view_order,
    condition=str_c("expt3_M1M2M3_sort"),
    image=str_c("image_", trialID),
    stage_num=1,
    group_size=7, #TODO figure out how to handle group size here!!
  ) |> 
  select(gameid, playerid, rep_num, trial_num, images, image, stage_num,
         text=transcription, condition, group_size)

cued_expt3 <- read_tsv(here(raw_data_dir, "Experiment3.txt")) |> 
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>   
  mutate(
    gameid = str_c("expt3_group", subID, "_images", image_set),
    playerid = str_c("expt3_group", subID, "_d"),
    images=str_c("image_", trialID, ";unk1;unk2;unk3"),
    rep_num=6,
    stage_num=2,
    trial_num=(rep_num-1)*16+presumed_view_order,
    condition=str_c("expt3_",condition, "_match"),
    image=str_c("image_", trialID),
    group_size=7) |> 
  select(gameid, playerid, rep_num, trial_num, images, image, stage_num,
         text=transcription, condition, group_size)

all_3 <- sort_expt3_clean |> bind_rows(cued_expt3)

#### put everything together

all <- all_1 |> bind_rows(all_2) |> bind_rows(all_3) |> 
  mutate(dataset_id="yoon2019_audience",
         full_cite="Yoon, S. O., & Brown‐Schmidt, S. (2019). Audience design in multiparty conversation. Cognitive science, 43(8), e12774.",
         short_cite="Yoon & Brown-Schmidt (2019)",
         structure="naive-swap",
         language="English",
         exclude=F,
         exclusion_reason=as.character(NA),
         action_type="message",
         role="describer",
         time_stamp=as.numeric(NA),
         message_irrelevant=F,
         room_num=1,
         age=as.numeric(NA), #don't have demographics
         gender=as.character(NA), 
         choice_id=NA # we don't have choice data
         ) |> 
  group_by(rep_num, trial_num, gameid) |> 
  mutate(message_number=row_number() |> as.numeric()) |> 
  ungroup() |> 
  select(condition_label=condition, dataset_id, full_cite, short_cite,
         trial_num, rep_num, stage_num, 
         group_size, structure, language, game_id=gameid, room_num, option_set=images,
         target=image, exclude, exclusion_reason, action_type,player_id=playerid,
         age, gender,
         role, time_stamp, text, message_number, message_irrelevant, choice_id
         )

validate_dataset(all, write=T)

