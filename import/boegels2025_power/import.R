library(here)
library(tidyverse)
library(jsonlite)

# each trial has both a identify the one being talked about and
# a figure out if it's in the same place on the screen
# we're going to drop the localization part

raw_data_loc <- "import/boegels2025_power/raw_data"

results <- read_delim(here(raw_data_loc, "interaction_ToT_accuracy.txt"), delim = ";") |>
  select(-trial) |>
  filter(task == "ref")

results |>
  select(pair, round, correct_target = target, given_answer = correct_answer) |>
  distinct()

results |>
  select(pair, correct_target = target, given_answer = correct_answer) |>
  distinct()

# there are not very many trials without correct choices and we can't easily reconstruct choice id for most of them, so just dropping

choices <- results |>
  filter(correct_answer == given_answer) |>
  mutate(participant = ifelse(director == "A", "B", "A")) |>
  mutate(
    target = str_c("f", target),
    choice_id = ifelse(given_answer == correct_answer, target, NA)
  ) |>
  select(pair, round, trial_nr, participant, choice_id, target, onset_msec, offset_msec) |>
  mutate(action_type = "selection", role = "matcher", time_stamp = round((offset_msec - onset_msec) / 1000))

# todo figure out what the choices mapping is when they got it wrong!

transcripts <- list.files(here(raw_data_loc, "transcript")) |>
  as_tibble() |>
  mutate(data = map(value, \(v) {
    read_csv(here(raw_data_loc, "transcript", v), col_types = "c")
  })) |>
  unnest(data) |>
  filter(task == "ref") |>
  select(-value, -trial, trial_id, -task) |>
  left_join(trial_start) |>
  mutate(
    action_type = "message",
    target = str_c("f", target),
    time_stamp = round((onset_msec - trial_onset) / 1000),
    role = ifelse(participant == director, "describer", "matcher")
  ) |>
  rename(text = speech) |>
  mutate(text = str_replace_all(text, "�", "")) |>
  group_by(pair, target, trial_nr, round) |>
  mutate(
    message_number = row_number() |> as.numeric(),
    message_irrelevant = F
  )

fill_describer <- transcripts |>
  anti_join(transcripts |> filter(role == "describer") |> distinct(pair, round, trial_nr)) |>
  distinct(pair, round, trial_nr, target, director, action_type) |>
  mutate(
    participant = director,
    role = "describer",
    text = NA,
  )





all <- choices |>
  bind_rows(transcripts) |>
  bind_rows(fill_describer) |>
  mutate(
    condition_label = "",
    dataset_id = "boegels2025_power",
    full_cite = "Bögels, S., Li, T., Rasenberg, M., Eijk, L., Toni, I., & Pouw, W. (2026). There is a power law of joint communicative effort and it reflects communicative work. Cognition, 268(10637), 0.",
    short_cite = "Bögels et al (2026)",
    group_size = 2,
    language = "Dutch",
    prior_relationship = "no",
    partner_constancy = "yes",
    role_constancy = "no",
    population = "adults",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "none", # not explicitly mentioned, guessing from dataset paper
    backchannel = "full",
    room_num = 1,
    option_set = "f1;f2;f3;f4;f5;f6;f7;f8;f9;f10;f11;f12;f13;f14;f15;f16",
    rep_num = round,
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
    -round, -trial_nr, -participant, -onset_msec, -offset_msec, -trial,
    -trial_id, -task, -director, -onset, -offset, -trial_onset
  )

source(here("validate.R"))

validate_dataset(all, write = T)
