library(tidyverse)
library(here)
source(here("validate.R"))

raw_transcript <- read_csv(here("import/dahan2023_collaboration/raw_data/segmented_transcript.csv"))


set_2 <- "2_1_1;2_1_2;2_1_3;2_1_4;2_2_1;2_2_2;2_2_3;2_2_4;2_3_1;2_3_2;2_3_3;2_3_4;2_4_1;2_4_2;2_4_3;2_4_4"
set_1 <- "1_1_1;1_1_2;1_1_3;1_1_4;1_2_1;1_2_2;1_2_3;1_2_4;1_3_1;1_3_2;1_3_3;1_3_4;1_4_1;1_4_2;1_4_3;1_4_4"

choices_info <- read_tsv(here("import/dahan2023_collaboration/raw_data/Alignment_Data.txt")) |>
  filter(!str_detect(Segment, "_1")) |> # in a few cases, there are two rows for a segment, and in these cases we take the second one
  mutate(
    Segment = str_replace(Segment, "_2", ""),
    game_id = str_sub(Dyad, 2, 3) |> as.numeric(),
    player_id = str_c(game_id, "_", Matcher),
    choice_id = Matched,
    target = Target,
    trial_num = as.numeric(Segment),
    rep_num = ((trial_num - 1) %/% 3) + 1,
    option_set = case_when(
      str_sub(target, 1, 1) == "1" ~ set_1,
      str_sub(target, 1, 1) == "2" ~ set_2
    ),
    role = "matcher",
    action_type = "selection"
  ) |>
  select(game_id, player_id, choice_id, target, trial_num, rep_num, option_set, role, action_type)

target_info <- choices_info |> select(game_id, trial_num, rep_num, target, option_set)

text <- raw_transcript |>
  fill(game) |>
  mutate(
    message_irrelevant = case_when(
      is.na(targetPosition_w) ~ T,
      targetPosition_w == "chitchat" ~ T,
      T ~ NA
    ),
    targetPosition_w = ifelse(targetPosition_w == "chitchat", NA, targetPosition_w) |> as.numeric()
  ) |>
  group_by(game) |>
  fill(targetPosition_w) |>
  ungroup() |>
  filter(!is.na(targetPosition_w)) |>
  filter(!is.na(role_w), !role_w == "experimenter") |>
  mutate(
    role = case_when(
      role_w == "D" ~ "describer",
      role_w == "M" ~ "matcher",
      T ~ role_w
    ),
    trial_num = (grid_w - 1) * 3 + targetPosition_w,
    # role switches every 12 trials so trials 1-12 & 25-36 are described by "A"
    player_id = case_when(
      trial_num <= 12 & role == "describer" ~ str_c(game, "_A"),
      trial_num >= 13 & trial_num <= 24 & role == "matcher" ~ str_c(game, "_A"),
      trial_num >= 25 & trial_num <= 36 & role == "describer" ~ str_c(game, "_A"),
      trial_num >= 37 & role == "matcher" ~ str_c(game, "_A"),
      T ~ str_c(game, "_B")
    ),
    action_type = "message",
    game_id = as.numeric(game)
  ) |>
  group_by(game_id, trial_num) |>
  mutate(message_number = row_number() |> as.numeric()) |>
  ungroup() |>
  left_join(target_info) |>
  select(
    rep_num = grid_w, game_id, role, trial_num, option_set, target,
    action_type, player_id, message_irrelevant, text = message, message_number
  )

missing_describer_messages <- choices_info |>
  select(game_id, trial_num, rep_num, player_id, target, option_set) |>
  mutate(player_id = case_when(
    str_detect(player_id, "A") ~ str_c(game_id, "_B"),
    str_detect(player_id, "B") ~ str_c(game_id, "_A")
  )) |>
  mutate(role = "describer") |>
  anti_join(text |> filter(role == "describer")) |>
  mutate(text = NA, message_number = 1, action_type = "message")


all <- text |>
  bind_rows(choices_info) |>
  bind_rows(missing_describer_messages) |>
  mutate(
    condition_label = "dahan_color_boxes",
    dataset_id = "dahan2023_collaboration",
    full_cite = "Dahan, D. (2023). Collaboration under uncertainty in unscripted conversations: The role of hedges. Journal of Experimental Psychology: Learning, Memory, and Cognition, 49(2), 320.",
    short_cite = "Dahan (2023)",
    group_size = 2,
    language = "English",
    prior_relationship = "yes",
    partner_constancy = "yes",
    population = "adult",
    role_constancy = "no",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "none",
    backchannel = "full",
    room_num = 1,
    stage_num = 1,
    exclude = F,
    exclusion_reason = NA_character_,
    order_match = "order",
    time_stamp = NA_real_,
    age = NA_real_,
    gender = NA_character_,
    native_language = NA_character_,
    race = NA_character_,
    education = NA_character_,
  )
validate_dataset(all, write = T)
