library(tidyverse)
library(here)
library(jsonlite)

raw_data_loc <- "import/hawkins_fmri/raw_data/hawkins_outputs"


full <- here(raw_data_loc) |>
  list.files() |>
  as_tibble() |>
  filter(str_detect(value, "run")) |>
  mutate(data = map(value, \(f) {
    fromJSON(here(raw_data_loc, f))$segments |>
      as_tibble() |>
      select(-words) |>
      mutate(source = f)
  })) |>
  unnest(data) |>
  mutate(
    role = "",
    grid = "",
    targetPosition = "",
    message = text
  ) |>
  mutate(game = as.factor(source) |> as.numeric()) |>
  mutate(
    game = str_sub(source, 7, 9),
    rep = str_sub(source, -6, -6),
    rep = ifelse(rep == "v", 2, rep), # one file with a weird naming
    message_id_num = row_number()
  ) |>
  mutate(
    grid = as.numeric(rep),
    targetPosition = "",
    role = "",
    message = text
  ) |>
  select(game, grid, targetPosition, role, message)

pre_post <- here(raw_data_loc) |>
  list.files() |>
  as_tibble() |>
  filter(str_detect(value, "Test")) |>
  mutate(data = map(value, \(f) {
    fromJSON(here(raw_data_loc, f))$segments |>
      as_tibble() |>
      select(-words) |>
      mutate(source = f)
  })) |>
  unnest(data) |>
  mutate(game = as.factor(source) |> as.numeric()) |>
  mutate(
    game = str_sub(source, 7, 9),
    rep = case_when(
      str_detect(source, "Pre") ~ 0,
      str_detect(source, "Post") ~ 7
    ),
  ) |>
  mutate(
    grid = as.numeric(rep),
    targetPosition = "",
    role = "",
    message = text
  ) |>
  select(game, grid, targetPosition, role, message) |>
  write_csv(here("segmentation/remainder/hawkins_fmri_prepost.csv"))


# specs
# game (numeric from 1)
# grid (= round, numeric from 1)
# targetPosition blank
# role -- describer/matcher (if known)
# person A/B
# message

# then save out as
# sample.csv
# remainder.csv


# write_csv(full, here("segmentation/remainder/hawkins_fmri.csv"))

# spec
# 21 games
# 2268 trials

full |>
  mutate(words = str_count(message, "\\S+")) |>
  summarize(sum = sum(words))

# probably 18 / round ?
# deffo 18 targets/game, in one of two sets!
