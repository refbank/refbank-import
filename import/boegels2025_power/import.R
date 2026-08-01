library(here)
library(tidyverse)
library(jsonlite)

#

raw_data_loc <- "import/boegels2025_power/raw_data"

results <- read_delim(here(raw_data_loc, "interaction_ToT_accuracy.txt"), delim = ";") |>
  select(-trial) |>
  filter(task == "ref")


selections <- results |>
  filter(correct_answer == given_answer) |>
  mutate(participant = ifelse(director == "A", "B", "A")) |>
  mutate(
    target_image = str_c("f", target),
    selected_image = ifelse(given_answer == correct_answer, target_image, NA)
  ) |>
  select(pair, round, trial_nr, participant, selected_image, target_image, onset_msec, offset_msec) |>
  mutate(action_type = "selection", role = "matcher", time_stamp = round((offset_msec - onset_msec) / 1000))

trial_start <- results |> select(pair, round, trial_nr, trial_onset = onset_msec)

transcripts <- list.files(here(raw_data_loc, "transcript")) |>
  as_tibble() |>
  mutate(data = map(value, \(v) {
    read_csv(here(raw_data_loc, "transcript", v), col_types = "c")
  })) |>
  unnest(data) |>
  filter(task == "ref") |>
  select(-value, -trial, -trial_id, -task) |>
  left_join(trial_start) |>
  mutate(
    action_type = "message",
    target_image = str_c("f", target),
    time_stamp = round((onset_msec - trial_onset) / 1000),
    role = ifelse(participant == director, "describer", "matcher")
  ) |>
  rename(text = speech) |>
  mutate(text = str_replace_all(text, "�", "")) |>
  group_by(pair, target_image, trial_nr, round) |>
  mutate(
    message_num = row_number() |> as.numeric(),
    message_irrelevant = F
  )

fill_describer <- transcripts |>
  anti_join(transcripts |> filter(role == "describer") |> distinct(pair, round, trial_nr)) |>
  distinct(pair, round, trial_nr, target_image, director, action_type) |>
  mutate(
    participant = director,
    role = "describer",
    text = NA,
  )


all <- selections |>
  bind_rows(transcripts) |>
  bind_rows(fill_describer) |>
  mutate(
    condition_label = "expt1",
    dataset_id = "boegels2025_power",
    full_cite = "Bögels, S., Li, T., Rasenberg, M., Eijk, L., Toni, I., & Pouw, W. (2026). There is a power law of joint communicative effort and it reflects communicative work. Cognition, 268(10637), 0.",
    short_cite = "Bögels et al. (2026)",
    group_size = 2,
    language = "Dutch",
    prior_relationship = "no",
    partner_constancy = "yes",
    role_constancy = "no",
    population = "adult",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "none", # not explicitly mentioned, guessing from dataset paper
    backchannel = "full",
    room_num = 1,
    image_options = "f1;f2;f3;f4;f5;f6;f7;f8;f9;f10;f11;f12;f13;f14;f15;f16",
    round_num = round,
    trial_num = trial_nr + 16 * (round - 1),
    stage_num = 1,
    exclude = F,
    exclusion_reason = as.character(NA),
    order_match = "match",
    player_id = str_c(pair, "_", participant),
    age = as.numeric(NA),
    gender = as.character(NA),
    native_language = "Dutch", # per paper
    race = as.character(NA),
    education = as.character(NA),
  ) |>
  rename(game_id = pair) |>
  select(
    -round, -trial_nr, -participant, -onset_msec, -offset_msec,
    -director, -onset, -offset, -trial_onset, -target
  )

source(here("validate.R"))

validate_dataset(all, write = T)
