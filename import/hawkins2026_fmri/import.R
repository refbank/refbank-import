library(tidyverse)
library(here)
library(jsonlite)
source(here("validate.R"))
raw_data_loc <- "import/hawkins2026_fmri/raw_data/behavioral/cleaned_behavioral/combined_clean.csv"

prep_behavior <- read_csv(here(raw_data_loc)) |>
  # filter(verb %in% c("RoundStarted", "clickedTangram")) |>
  filter(blockNum != "PreExposure", blockNum != "PostExposure") |>
  mutate(
    blockNum = case_when(
      # game 004 round 1 gets removed, so we only have 5 rounds -- maybe these should canonically be 2-6? going by audio
      str_detect(blockNum, "Repeated") & name == "sessid004" ~ 1 + (str_sub(blockNum, -2, -1) |> as.numeric()),
      str_detect(blockNum, "Repeated") ~ str_sub(blockNum, -2, -1) |> as.numeric(),
      blockNum == "PreTest" ~ 0,
      blockNum == "PostTest" ~ 7
    ),
    trial_position = case_when(
      blockNum > 0 & blockNum < 7 ~ (trialNum - 72) %% 18 + 1,
      blockNum == 0 ~ (trialNum - 36) + 1,
      blockNum == 7 ~ (trialNum - 180) + 1
    ),
    target_image = str_sub(target, 21, -5),
    selected_image = str_sub(response, 21, -5),
    game_id = str_sub(name, -3, -1) |> as.numeric()
  ) |>
  mutate(
    round_num = blockNum + 1,
    trial_num = case_when(
      round_num == 1 ~ trial_position,
      round_num > 1 ~ round_num * 18 + trial_position # rep2 starts at 37, etc
    )
  ) |>
  select(game_id, target_image, round_num, trial_num, target_image, selected_image, side, verb, at)


selections <- prep_behavior |>
  filter(verb == "clickedTangram") |>
  left_join(prep_behavior |> filter(verb == "RoundStarted") |>
    select(game_id, target_image, round_num, trial_num, starttime = at)) |>
  mutate(
    time_stamp = (at - starttime) |> as.numeric(),
    player_id = str_c(game_id, "_", side),
    action_type = "selection",
    role = "matcher"
  ) |>
  # the click handler sometimes double-fires, logging several identical clickedTangram
  # events ms apart for the same trial/player -- keep only the earliest
  group_by(game_id, round_num, trial_num, player_id) |>
  slice_min(time_stamp, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(game_id, target_image, round_num, trial_num, selected_image, time_stamp, player_id, action_type, role)

image_options <- prep_behavior |>
  select(game_id, target_image, round_num) |>
  distinct() |>
  group_by(game_id, round_num) |>
  summarize(image_options = str_c(target_image, collapse = ";")) |>
  left_join(prep_behavior |> select(game_id, trial_num, target_image, round_num) |> distinct())

# one games doesn't follow role switch by block because something goes off with 004, so we do this
# the janky hard way
pairs <- selections |>
  distinct(game_id, player_id) |>
  mutate(role = rep(c("describer", "matcher"), times = 21)) |>
  pivot_wider(names_from = role, values_from = player_id)

all_pairs <- pairs |>
  rename(describer = matcher, matcher = describer) |>
  bind_rows(pairs)

roles <- selections |>
  distinct(game_id, round_num, trial_num, player_id) |>
  rename(matcher = player_id) |>
  left_join(all_pairs) |>
  pivot_longer(c("matcher", "describer"), names_to = "role", values_to = "player_id") |>
  bind_rows(tribble( # manually add the missing problem children
    ~game_id, ~round_num, ~trial_num, ~role, ~player_id,
    2, 3, 59, "matcher", "2_Repi",
    2, 3, 59, "describer", "2_Minu",
    3, 1, 1, "matcher", "3_Repi",
    3, 1, 1, "describer", "3_Minu",
    3, 1, 2, "matcher", "3_Repi",
    3, 1, 2, "describer", "3_Minu",
    3, 1, 12, "matcher", "3_Repi",
    3, 1, 12, "describer", "3_Minu",
    3, 1, 17, "matcher", "3_Repi",
    3, 1, 17, "describer", "3_Minu",
    3, 8, 158, "matcher", "3_Minu",
    3, 8, 158, "describer", "3_Repi",
    4, 1, 4, "matcher", "4_RsX8D2ktXfdxYpYxB",
    4, 1, 4, "describer", "4_Wa7iFwCiBd6aiBdht",
    4, 1, 31, "matcher", "4_RsX8D2ktXfdxYpYxB",
    4, 1, 31, "describer", "4_Wa7iFwCiBd6aiBdht"
  ))

transcript <- read_csv(here("import/hawkins2026_fmri/raw_data/hawkins_segmented.csv")) |>
  filter(!role == "E") |>
  filter(!(game == 4 & grid == 1)) |> # remove the extra extra chitchatty nonsense
  rename(game_id = game) |>
  mutate(
    round_num = grid + 1,
    trial_num = case_when(
      round_num == 1 ~ targetPosition,
      round_num > 1 ~ round_num * 18 + targetPosition # rep2 starts at 37, etc
    ),
    message_irrelevant = case_when(
      is.na(chitchat) ~ F,
      chitchat == "x" ~ T,
      T ~ F
    ),
    role = case_when(
      role == "D" ~ "describer",
      role == "M" ~ "matcher"
    ),
    action_type = "message"
  ) |>
  rename(text = message) |>
  left_join(roles) |>
  group_by(trial_num, game_id, round_num) |>
  mutate(message_num = row_number() |> as.numeric()) |>
  ungroup() |>
  left_join(image_options) |>
  select(game_id, round_num, trial_num, player_id, text, image_options, target_image, action_type, message_num, role, message_irrelevant)

dummy_describers <- selections |>
  left_join(image_options) |>
  bind_rows(transcript |> filter(role == "matcher")) |>
  distinct(game_id, round_num, trial_num, target_image, image_options) |>
  anti_join(transcript |> filter(role == "describer")) |>
  mutate(role = "describer") |>
  left_join(roles) |>
  mutate(action_type = "message")

combined <- selections |>
  left_join(image_options) |>
  bind_rows(dummy_describers) |>
  bind_rows(transcript) |>
  mutate(
    condition_label = "fmri",
    dataset_id = "hawkins2026_fmri",
    full_cite = "Hawkins, R. D., et al (2026) Unpublished hyperscanning study.",
    short_cite = "Hawkins et al. (2026)",
    group_size = 2,
    language = "English",
    prior_relationship = "yes",
    partner_constancy = "yes",
    population = "adult",
    role_constancy = "no",
    confederates = "no",
    modality = "oral-in-person", # idk are scanners in person?
    feedback = "full",
    backchannel = "full",
    room_num = 1,
    stage_num = 1,
    exclude = F,
    exclusion_reason = NA_character_,
    order_match = "match",
    age = NA_real_,
    race = NA_character_,
    gender = NA_character_,
    education = NA_character_,
    native_language = NA_character_
  )

validate_dataset(combined, write = T)
