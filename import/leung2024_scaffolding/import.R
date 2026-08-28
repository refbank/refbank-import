library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(here)

base_url <- "https://raw.githubusercontent.com/ashleychuikay/tangramgame/refs/heads/master/data/experiment1/"

# combined_clean.csv already has the researchers' final exclusion criteria applied (its subid list is
# exactly the 63 child-parent + 19 adult-adult participants the readme reports keeping) and reliable
# subject-level age/experiment metadata -- but its own trial-level selection data is badly lossy: the
# script that built it drops most matcher/selection events (median ~20% of real trials survive per
# subject, some subjects 0%). So we use it only for the canonical subject list + metadata, and re-pull
# the real trial-by-trial events from each subject's individual raw log, which is complete.
subjects <- read_csv(url(str_c(base_url, "combined_clean.csv"))) |>
  distinct(subid, age, experiment)

read_subject_log <- function(subid, experiment) {
  subdir <- ifelse(experiment == "adult-adult", "adult/", "")
  read_csv(url(str_c(base_url, subdir, subid, ".csv"))) |>
    mutate(
      subid = as.numeric(subid),
      trial = as.numeric(trial),
      # raw logs encode this as "Y"/"N" text, not logical -- ifelse(correct, ...)
      # further down needs a real logical or it silently produces NA throughout
      correct = correct == "Y"
    )
}

combined_df <- map2_dfr(subjects$subid, subjects$experiment, read_subject_log) |>
  filter(type == "test") |>
  mutate(round_num = ceiling(trial / 10)) |>
  left_join(subjects, by = "subid") |>
  rename(target_image = target)

# the person who made the selection on a trial is the matcher; the other of the pair's two people is
# the director. A handful of trials have data errors in the source logs: no selection was recorded at
# all, or both people were recorded as selecting (contradictory) -- we can't reconstruct a real matcher
# identity for these, so they're excluded rather than guessed at (this supersedes the old one-off guess
# for subid 107 trial 21, which turns out to be one of three trials with this same ambiguity, not a
# unique case)
selection_events <- combined_df |> filter(!is.na(selection))

ambiguous_trials <- selection_events |>
  distinct(subid, trial, person) |>
  count(subid, trial) |>
  filter(n > 1) |>
  select(subid, trial) |>
  mutate(exclusion_reason = "ambiguous matcher identity")

no_selection_trials <- combined_df |>
  distinct(subid, trial) |>
  anti_join(selection_events |> distinct(subid, trial), by = c("subid", "trial")) |>
  mutate(exclusion_reason = "no selection recorded")

bad_trials <- bind_rows(ambiguous_trials, no_selection_trials) |> mutate(exclude = TRUE)

matcher_id <- selection_events |>
  anti_join(ambiguous_trials, by = c("subid", "trial")) |>
  distinct(subid, trial, matcher = person, correct)

# excluded trials still need *some* valid matcher/director assignment to produce schema-valid rows
# (every trial needs exactly one describer); since these rows are marked exclude = TRUE this arbitrary
# pick doesn't affect any analysis
placeholder_matcher <- combined_df |>
  distinct(subid, trial) |>
  semi_join(bad_trials, by = c("subid", "trial")) |>
  left_join(combined_df |> distinct(subid, person) |> group_by(subid) |> slice_min(person, n = 1) |> rename(matcher = person), by = "subid") |>
  mutate(correct = NA)

matcher_id <- bind_rows(matcher_id, placeholder_matcher)

# each subid has exactly 2 fixed people across all trials (verified against the raw logs), so the
# director is whichever of the pair isn't the matcher -- this has to come from the subject-level pair,
# not from who has a row in this specific trial, since a director who stayed silent *and* didn't select
# on a given trial otherwise wouldn't appear in that trial's rows at all
people_per_subject <- combined_df |> distinct(subid, person)

director_id <- matcher_id |>
  select(subid, trial, matcher) |>
  left_join(people_per_subject, by = "subid", relationship = "many-to-many") |>
  filter(person != matcher) |>
  distinct(subid, trial, director = person)

roled <- combined_df |>
  left_join(matcher_id |> select(subid, trial, matcher), by = c("subid", "trial")) |>
  left_join(director_id, by = c("subid", "trial")) |>
  mutate(role = ifelse(person == matcher, "matcher", "director"))

selections <- roled |>
  filter(role == "matcher", !is.na(correct)) |>
  select(subid, trial, person, role, target_image, round_num, age, experiment, correct, director) |>
  unique() |>
  mutate(
    selected_image = ifelse(correct, target_image, "unk1"),
    action_type = "selection"
  )

messages <- roled |>
  select(-correct, -matcher) |>
  filter(!is.na(utterance)) |>
  group_by(subid, trial) |>
  mutate(
    message_num = row_number() |> as.numeric(),
    text = utterance,
    message_irrelevant = NA,
    action_type = "message"
  ) |>
  ungroup()

missing_messages <- roled |>
  select(subid, trial, person, role, director, age, experiment, target_image, round_num) |>
  anti_join(messages |> filter(role == "director") |> select(subid, trial, person) |> unique()) |>
  mutate(
    person = director,
    role = "director",
    message_num = as.numeric(NA),
    text = NA,
    message_irrelevant = NA,
    action_type = "message"
  ) |>
  select(-director)


all <- messages |>
  bind_rows(selections) |>
  bind_rows(missing_messages) |>
  select(-director) |>
  left_join(bad_trials |> select(subid, trial, exclude, exclusion_reason), by = c("subid", "trial")) |>
  mutate(
    exclude = replace_na(exclude, FALSE),
    game_id = as.character(subid) |> str_trim(),
    role = ifelse(role == "matcher", "matcher", "describer"),
    player_id = str_c(game_id, "_", person),
    image_options = str_c(target_image, "unk1", sep = ";"), # we don't know what the distractor is per trial!
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
    short_cite = "Leung et al. (2025)",
    dataset_id = "leung2024_scaffolding",
    time_stamp = as.numeric(NA),
  ) |>
  rename(
    trial_num = trial,
    condition_label = experiment,
  ) |>
  select(-subid, -person, -utterance, -correct, -selection, -type)

source(here("validate.R"))

validate_dataset(all, write = T)
