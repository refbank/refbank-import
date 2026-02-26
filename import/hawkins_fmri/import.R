library(tidyverse)
library(here)
library(jsonlite)

raw_data_loc <- "import/hawkins_fmri/raw_data/behavioral/cleaned_behavioral/combined_clean.csv"

foo <- read_csv(here(raw_data_loc)) |>
  filter(verb == "clickedTangram") |>
  filter(str_detect(blockNum, "Repeated"))


pull_targets <- read_csv(here(raw_data_loc)) |>
  distinct(target, trialNum, blockNum, name) |>
  filter(str_detect(blockNum, "Repeated")) |>
  mutate(
    target = str_sub(target, 21, -5),
    blockNum = str_sub(blockNum, -2, -1),
    trial_position = (trialNum - 72) %% 18 + 1
  ) |>
  select(-trialNum)

pull_targets |>
  group_by(name, blockNum) |>
  tally() |>
  filter(n != 18)
pull_targets |>
  distinct(name, blockNum) |>
  group_by(name) |>
  tally() |>
  filter(n != 6)

pull_targets |>
  distinct(target) |>
  View()

pull_targets |>
  filter(name == "sessid004") |>
  select(trial_position, target, everything()) |>
  View()

# game 004 round 1 gets removed, so we only have 5 rounds -- maybe these should canonically be 2-6? going by audio
