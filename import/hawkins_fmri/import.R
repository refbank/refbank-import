library(tidyverse)
library(here)
library(jsonlite)

raw_data_loc <- "import/hawkins_fmri/raw_data/behavioral/cleaned_behavioral/combined_clean.csv"


pull_targets <- read_csv(here(raw_data_loc)) |>
  distinct(target, trialNum, blockNum, name) |>
  filter(blockNum != "PreExposure", blockNum != "PostExposure") |>
  mutate(
    blockNum = case_when(
      str_detect(blockNum, "Repeated") ~ str_sub(blockNum, -2, -1) |> as.numeric(),
      blockNum == "PreTest" ~ 0,
      blockNum == "PostTest" ~ 7
    ),
    trial_position = case_when(
      blockNum > 0 & blockNum < 7 ~ (trialNum - 72) %% 18 + 1,
      blockNum == 0 ~ (trialNum - 36) + 1,
      blockNum == 7 ~ (trialNum - 180) + 1
    ),
    target = str_sub(target, 21, -5),
  )


# game 004 round 1 gets removed, so we only have 5 rounds -- maybe these should canonically be 2-6? going by audio
