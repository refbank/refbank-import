library(tidyverse)
library(here)
library(antiword)
library(readxl)


transcripts <- list.files(here(raw_data_loc, "transcriptions-with-group-names")) |>
  as_tibble() |>
  mutate(transcript = map(value, process_transcript)) |>
  select(-value) |>
  unnest(transcript)

full_describers <- transcripts |>
  select(group, condition, round) |>
  distinct() |>
  expand_grid(trial = 1:8, speaker = "D") |>
  mutate(trial = as.character(trial))

ordering <- read_csv(here(raw_data_loc, "ordering.csv")) |>
  select(Round, starts_with("tangram"), starts_with("words")) |>
  filter(!is.na(Round)) |>
  mutate(round_label = ifelse(str_detect(Round, "A|B"), Round, NA)) |>
  fill(round_label) |>
  pivot_longer(
    cols = c(starts_with("words"), starts_with("tangram")), cols_vary = "slowest",
    names_to = c(".value", "set"),
    names_pattern = "([a-z]+)([1-8])"
  ) |>
  mutate(game = ifelse(str_detect(words, "G"), words, NA)) |>
  fill(game) |>
  filter(!str_detect(Round, "A|B")) |>
  mutate(group = str_sub(game, 2, -1)) |>
  select(-game) |>
  separate(round_label, into = c("round", "condition"), sep = "-") |>
  mutate(
    round = str_squish(round),
    condition = str_squish(condition)
  ) |>
  rename(trial = Round)

# best guesses assuming that tangram ordering is consistent
# 1=g, 2=h, 3=i, 4=j, 5=a, 6=b, 7=c, 8=d



source("validate.R")
validate_dataset(combined, write = T)
