library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))


raw_data_dir <- "import/yoon2019_audience/raw_data/"



### Experiment 1

# TrialID seems to correspond to target_image image at least
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
    round_num = ifelse(partner == "M1", round, 4 + round),
    stage_num = ifelse(partner == "M1", 1, 2),
    trial_num = presumed_view_order,
    image = str_c("image_", trialID),
    group_size = 4, # TODO figure out how to handle group size here!!
    order_match = "order",
    role = "describer"
  ) |>
  select(gameid, playerid, round_num, trial_num, images, image, stage_num,
    text = transcription, group_size, order_match, role
  )

# add dummy messages for matchers
sort_expt1_matcher <- sort_expt1 |>
  left_join(image_sort_sets) |>
  mutate(
    gameid = str_c("expt1_group", subID, "_images", image_set),
    playerid = str_c("expt1_group", subID, "_", partner),
    round_num = ifelse(partner == "M1", round, 4 + round),
    stage_num = ifelse(partner == "M1", 1, 2),
    trial_num = presumed_view_order,
    image = str_c("image_", trialID),
    group_size = 4,
    order_match = "order",
    text = NA,
    role = "matcher"
  ) |>
  select(
    gameid, playerid, round_num, trial_num, images, image, stage_num,
    text, group_size, order_match, role
  ) |>
  distinct()

cued_expt1 <- read_tsv(here(raw_data_dir, "Experiment1.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(
    # subID 5's cued-phase image_set tags for 1(1K)/2(2K1N) are swapped in the raw data:
    # M2's sort session (raw_data/E1_sorting.txt) is recorded at image_set 2, which collides
    # with this subject's 1(1K) game instead of their 2(2K1N) game -- no other subject has
    # this collision, and swapping just these two labels back makes both games internally
    # consistent again (2K1N ends up with the M1+M2 sort data it needs; 1K ends up with only
    # M1's, as expected)
    image_set = case_when(
      subID == 5 & condition == "1(1K)" ~ "3",
      subID == 5 & condition == "2(2K1N)" ~ "2",
      T ~ str_sub(trialID, 1, 1)
    )
  ) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>
  mutate(
    gameid = str_c("expt1_group", subID, "_images", image_set),
    playerid = str_c("expt1_group", subID, "_d"),
    images = str_c("image_", trialID, ";unk1;unk2;unk3"),
    image = str_c("image_", trialID),
    round_num = case_when(
      condition == "2(2K1N)" ~ 9,
      T ~ 5
    ),
    stage_num = case_when(
      condition == "2(2K1N)" ~ 3,
      T ~ 2
    ),
    trial_num = (round_num - 1) * 16 + presumed_view_order,
    condition = str_c("expt1_", str_sub(condition, 3, -2)),
    group_size = 4,
    order_match = "match",
    role = "describer"
  ) |>
  select(gameid, playerid, round_num, trial_num, images, image, stage_num,
    text = transcription, condition, group_size, order_match, role
  )

expt1_cued_matchers <- tribble(
  ~condition, ~matcher,
  "1(1K)", "M1",
  "2(2K1N)", "M1",
  "2(2K1N)", "M2",
  "2(2K1N)", "M3",
  "3(1K2N)", "M1",
  "3(1K2N)", "M2",
  "3(1K2N)", "M3",
  "4(1N)", "M2"
)
cued_expt1_matcher <- read_tsv(here(raw_data_dir, "Experiment1.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(
    # see the matching comment in cued_expt1 above: subID 5's image_set tags for
    # 1(1K)/2(2K1N) are swapped in the raw data
    image_set = case_when(
      subID == 5 & condition == "1(1K)" ~ "3",
      subID == 5 & condition == "2(2K1N)" ~ "2",
      T ~ str_sub(trialID, 1, 1)
    )
  ) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>
  left_join(expt1_cued_matchers, relationship = "many-to-many") |>
  mutate(
    gameid = str_c("expt1_group", subID, "_images", image_set),
    playerid = str_c("expt1_group", subID, "_", matcher),
    images = str_c("image_", trialID, ";unk1;unk2;unk3"),
    image = str_c("image_", trialID),
    round_num = case_when(
      condition == "2(2K1N)" ~ 9,
      T ~ 5
    ),
    stage_num = case_when(
      condition == "2(2K1N)" ~ 3,
      T ~ 2
    ),
    trial_num = (round_num - 1) * 16 + presumed_view_order,
    condition = str_c("expt1_", str_sub(condition, 3, -2)),
    group_size = 4,
    order_match = "match",
    role = "matcher",
    text = NA
  ) |>
  select(
    gameid, playerid, round_num, trial_num, images, image, stage_num,
    text, condition, group_size, order_match, role
  ) |>
  distinct()


condition_1 <- cued_expt1 |> distinct(condition, gameid)

all_1 <- sort_expt1_clean |>
  bind_rows(sort_expt1_matcher) |>
  left_join(condition_1) |>
  bind_rows(cued_expt1) |>
  bind_rows(cued_expt1_matcher)


##### Expt 2

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
    round_num = case_when(
      partner == "M1" ~ round,
      partner == "M2" ~ 4 + round,
      partner == "M3" ~ 8 + round
    ),
    stage_num = case_when(
      partner == "M1" ~ 1,
      partner == "M2" ~ 2,
      partner == "M3" ~ 3
    ),
    trial_num = presumed_view_order,
    image = str_c("image_", trialID),
    group_size = 5, # TODO figure out how to handle group size here!!
    order_match = "order",
    role = "describer"
  ) |>
  select(gameid, playerid, round_num, trial_num, images, image, stage_num,
    text = transcription, group_size, order_match, role
  )

sort_expt2_matcher <- sort_expt2 |>
  left_join(image_sort_sets) |>
  mutate(
    gameid = str_c("expt2_group", subID, "_images", image_set),
    playerid = str_c("expt2_group", subID, "_", partner),
    round_num = case_when(
      partner == "M1" ~ round,
      partner == "M2" ~ 4 + round,
      partner == "M3" ~ 8 + round
    ),
    stage_num = case_when(
      partner == "M1" ~ 1,
      partner == "M2" ~ 2,
      partner == "M3" ~ 3
    ),
    trial_num = presumed_view_order,
    image = str_c("image_", trialID),
    group_size = 5,
    order_match = "order",
    role = "matcher",
    text = NA
  ) |>
  select(
    gameid, playerid, round_num, trial_num, images, image, stage_num,
    text, group_size, order_match, role
  ) |>
  distinct()

cued_expt2 <- read_tsv(here(raw_data_dir, "Experiment2.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>
  mutate(
    gameid = str_c("expt2_group", subID, "_images", image_set),
    playerid = str_c("expt2_group", subID, "_d"),
    images = str_c("image_", trialID, ";unk1;unk2;unk3"),
    round_num = case_when(
      condition == "2(3K1N)" ~ 13,
      condition == "3(2K2N)" ~ 9,
      T ~ 5
    ),
    stage_num = case_when(
      condition == "2(3K1N)" ~ 4,
      condition == "3(2K2N)" ~ 3,
      T ~ 2
    ),
    trial_num = (round_num - 1) * 16 + presumed_view_order,
    image = str_c("image_", trialID),
    condition = str_c("expt2_", str_sub(condition, 3, -2)),
    group_size = 5,
    order_match = "match",
    role = "describer"
  ) |>
  select(gameid, playerid, round_num, trial_num, images, image, stage_num,
    text = transcription, condition, group_size, order_match, role
  )

expt2_cued_matchers <- tribble(
  ~condition, ~matcher,
  "1(1K)", "M1",
  "2(3K1N)", "M1",
  "2(3K1N)", "M2",
  "2(3K1N)", "M3",
  "2(3K1N)", "M4",
  "3(2K2N)", "M1",
  "3(2K2N)", "M2",
  "3(2K2N)", "M3",
  "3(2K2N)", "M4",
  "4(1K3N)", "M1",
  "4(1K3N)", "M2",
  "4(1K3N)", "M3",
  "4(1K3N)", "M4",
  "5(1N)", "M2"
)

cued_expt2_matcher <- read_tsv(here(raw_data_dir, "Experiment2.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>
  left_join(expt2_cued_matchers, relationship = "many-to-many") |>
  mutate(
    gameid = str_c("expt2_group", subID, "_images", image_set),
    playerid = str_c("expt2_group", subID, "_", matcher),
    images = str_c("image_", trialID, ";unk1;unk2;unk3"),
    round_num = case_when(
      condition == "2(3K1N)" ~ 13,
      condition == "3(2K2N)" ~ 9,
      T ~ 5
    ),
    stage_num = case_when(
      condition == "2(3K1N)" ~ 4,
      condition == "3(2K2N)" ~ 3,
      T ~ 2
    ),
    trial_num = (round_num - 1) * 16 + presumed_view_order,
    image = str_c("image_", trialID),
    condition = str_c("expt2_", str_sub(condition, 3, -2)),
    group_size = 5,
    order_match = "match",
    role = "matcher",
    text = NA
  ) |>
  select(
    gameid, playerid, round_num, trial_num, images, image, stage_num,
    text, condition, group_size, order_match, role
  ) |>
  distinct()

condition_2 <- cued_expt2 |> distinct(gameid, condition)
all_2 <- sort_expt2_clean |>
  bind_rows(sort_expt2_matcher) |>
  left_join(condition_2) |>
  bind_rows(cued_expt2) |>
  bind_rows(cued_expt2_matcher)

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
    playerid = str_c("expt3_group", subID, "_d"), # if we ever get matcher stuff, note that a matcher becomes a director with a different number since each group of 7 does everything twice
    round_num = round,
    trial_num = presumed_view_order,
    image = str_c("image_", trialID),
    stage_num = 1,
    group_size = 7, # TODO figure out how to handle group size here!!
    order_match = "order",
    role = "describer"
  ) |>
  select(gameid, playerid, round_num, trial_num, images, image, stage_num,
    text = transcription, group_size, order_match, role
  )

expt3_sort_matchers <- tribble(
  ~matcher,
  "M1",
  "M2",
  "M3"
)
sort_expt3_matcher <- sort_expt3 |>
  left_join(image_sort_sets) |>
  cross_join(expt3_sort_matchers) |>
  mutate(
    gameid = str_c("expt3_group", subID, "_images", image_set),
    playerid = str_c("expt3_group", subID, "_", matcher),
    round_num = round,
    trial_num = presumed_view_order,
    image = str_c("image_", trialID),
    stage_num = 1,
    group_size = 7,
    order_match = "order",
    role = "matcher",
    text = NA
  ) |>
  select(
    gameid, playerid, round_num, trial_num, images, image, stage_num,
    text, group_size, order_match, role
  ) |>
  distinct()

cued_expt3 <- read_tsv(here(raw_data_dir, "Experiment3.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>
  mutate(
    gameid = str_c("expt3_group", subID, "_images", image_set),
    playerid = str_c("expt3_group", subID, "_d"),
    images = str_c("image_", trialID, ";unk1;unk2;unk3"),
    round_num = 6,
    stage_num = 2,
    trial_num = (round_num - 1) * 16 + presumed_view_order,
    condition = str_c("expt3_", str_sub(condition, 3, -2)),
    image = str_c("image_", trialID),
    group_size = 7,
    order_match = "match",
    role = "describer"
  ) |>
  select(gameid, playerid, round_num, trial_num, images, image, stage_num,
    text = transcription, condition, group_size, order_match, role
  )


expt3_cued_matchers <- tribble(
  ~condition, ~matcher,
  "1(3K)", "M1",
  "1(3K)", "M2",
  "1(3K)", "M3",
  "2(2K1N)", "M1",
  "2(2K1N)", "M2",
  "2(2K1N)", "M4",
  "3(1K2N)", "M1",
  "3(1K2N)", "M4",
  "3(1K2N)", "M5",
  "4(3N)", "M4",
  "4(3N)", "M5",
  "4(3N)", "M6",
)

cued_expt3_matcher <- read_tsv(here(raw_data_dir, "Experiment3.txt")) |>
  mutate(block = str_sub(trialID, 1, 1)) |>
  mutate(image_set = str_sub(trialID, 1, 1)) |>
  group_by(subID, block) |>
  mutate(presumed_view_order = row_number()) |> # TODO we don't actually trust this order, but it's all we have
  ungroup() |>
  left_join(expt3_cued_matchers, relationship = "many-to-many") |>
  mutate(
    gameid = str_c("expt3_group", subID, "_images", image_set),
    playerid = str_c("expt3_group", subID, "_", matcher),
    images = str_c("image_", trialID, ";unk1;unk2;unk3"),
    round_num = 6,
    stage_num = 2,
    trial_num = (round_num - 1) * 16 + presumed_view_order,
    condition = str_c("expt3_", str_sub(condition, 3, -2)),
    image = str_c("image_", trialID),
    group_size = 7,
    order_match = "match",
    role = "matcher",
    text = NA
  ) |>
  select(
    gameid, playerid, round_num, trial_num, images, image, stage_num,
    text, condition, group_size, order_match, role
  ) |>
  distinct()

condition_3 <- cued_expt3 |> distinct(gameid, condition)
all_3 <- sort_expt3_clean |>
  bind_rows(sort_expt3_matcher) |>
  left_join(condition_3) |>
  bind_rows(cued_expt3) |>
  bind_rows(cued_expt3_matcher)

#### put everything together

all <- all_1 |>
  bind_rows(all_2) |>
  bind_rows(all_3) |>
  mutate(
    dataset_id = "yoon2019_audience",
    full_cite = "Yoon, S. O., & Brown‐Schmidt, S. (2019). Audience design in multiparty conversation. Cognitive science, 43(8), e12774.",
    short_cite = "Yoon & Brown-Schmidt (2019)",
    prior_relationship = "no",
    partner_constancy = "no",
    role_constancy = "yes",
    population = "adult",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "none",
    backchannel = "full",
    language = "English",
    exclude = F,
    exclusion_reason = as.character(NA),
    action_type = "message",
    time_stamp = as.numeric(NA),
    message_irrelevant = F,
    room_num = 1,
    age = as.numeric(NA), # don't have demographics
    gender = as.character(NA),
    native_language = as.character(NA),
    race = as.character(NA),
    education = as.character(NA),
    selected_image = NA # we don't have choice data
  ) |>
  group_by(round_num, trial_num, gameid) |>
  mutate(message_num = row_number() |> as.numeric()) |>
  ungroup() |>
  select(
    condition_label = condition, dataset_id, full_cite, short_cite,
    trial_num, round_num, stage_num,
    group_size, language, prior_relationship, partner_constancy, role_constancy, population,
    modality, confederates, feedback, backchannel, order_match,
    game_id = gameid, room_num, image_options = images,
    target_image = image, exclude, exclusion_reason, action_type, player_id = playerid,
    age, gender, race, education, native_language,
    role, time_stamp, text, message_num, message_irrelevant, selected_image
  )

validate_dataset(all, write = F)
