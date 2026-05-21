library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

data_source <- "https://raw.githubusercontent.com/ashleychuikay/tangramgame/refs/heads/master/data/experiment1/combined_clean.csv"

combined_df <- read_csv(url(data_source)) |>
  mutate(role = case_when(
    subid == 107 & trial == 21 & person == "right" ~ "director",
    T ~ role
  ))

selections <- combined_df |>
  select(subid, trial, person, role, target, rep_num, age, experiment, correct) |>
  filter(role == "matcher") |>
  unique() |>
  mutate(
    choice_id = ifelse(correct, target, "unk1"),
    action_type = "selection"
  )

messages <- combined_df |>
  select(-correct, -director) |>
  filter(!is.na(utterance)) |>
  group_by(subid, trial) |>
  mutate(
    message_number = row_number() |> as.numeric(),
    text = utterance,
    message_irrelevant = NA,
    action_type = "message"
  ) |>
  ungroup()

missing_messages <- combined_df |>
  select(subid, trial, person, role, director, age, experiment, target, rep_num) |>
  anti_join(messages |> filter(role == "director") |> select(subid, trial, person) |> unique()) |>
  mutate(
    person = director,
    role = "director",
    message_number = as.numeric(NA),
    text = NA,
    message_irrelevant = NA,
    action_type = "message"
  ) |>
  select(-director)


all <- messages |>
  bind_rows(selections) |>
  bind_rows(missing_messages) |>
  mutate(
    game_id = as.character(subid) |> str_trim(),
    role = ifelse(role == "matcher", "matcher", "describer"),
    player_id = str_c(game_id, "_", person),
    option_set = str_c(target, "unk1", sep = ";"), # we don't know what the distractor is per trial!
    age = ifelse(person == "child", age, NA) |> as.numeric(),
    gender = as.character(NA),
    race = as.character(NA),
    education = ifelse(person == "child", "less-than-high-school", as.character(NA)),
    native_language = as.character(NA),
    stage_num = 1,
    room_num = 1,
    group_size = 2,
    prior_relationship = ifelse(experiment == "adult-adult", "no", "yes"),
    partner_constancy = "yes",
    role_constancy = "no",
    population = ifelse(experiment == "adult-adult", "adult", "child-parent"),
    modality = "oral-in-person",
    feedback = "none",
    backchannel = "full",
    order_match = "match",
    confederates = "no",
    language = "English",
    full_cite = "Leung, A., Yurovsky, D., & Hawkins, R. D. (2025). Parents spontaneously scaffold the formation of conversational pacts with their children. Child Development, 96(2), 546-561.",
    short_cite = "Leung et al. (2024)",
    dataset_id = "leung2024_scaffolding",
    exclude = F,
    exclusion_reason = as.character(NA),
    time_stamp = as.numeric(NA),
  ) |>
  rename(
    trial_num = trial,
    condition_label = experiment,
  ) |>
  select(-subid, -person, -utterance, -correct)

source(here("validate.R"))

validate_dataset(all, write = T)
