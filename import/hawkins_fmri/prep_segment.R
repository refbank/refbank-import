library(tidyverse)
library(here)
library(jsonlite)

raw_data_loc <- "import/hawkins_fmri/raw_data/behavioral/raw_transcripts"


full <- here(raw_data_loc) |>
  list.files(recursive = TRUE) |>
  as_tibble() |>
  filter(str_detect(value, ".tsv"), str_detect(value, "Repeated")) |>
  mutate(data = map(value, \(f){
    here(raw_data_loc, f) |>
      read_tsv() |>
      mutate(source = f) |>
      mutate(
        game = str_sub(source, 7, 9),
        rep = str_sub(source, -5, -5)
      )
  })) |>
  unnest(data)



ready_for_segment <- full |>
  mutate(
    game = as.factor(game) |> as.numeric(),
    grid = as.numeric(rep),
    targetPosition = "",
    role = "",
    message = text
  ) |>
  select(game, grid, targetPosition, role, message)


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


write_csv(ready_for_segment |> filter(game == 1), here("segmentation/sample/hawkins_fmri.csv"))
write_csv(ready_for_segment |> filter(game != 1), here("segmentation/remainder/hawkins_fmri.csv"))


# probably 18 / round ?
# deffo 18 targets/game, in one of two sets!
