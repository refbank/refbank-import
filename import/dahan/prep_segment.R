library(tidyverse)
library(here)
library(jsonlite)

raw_data_loc <- "import/dahan/dahan_outputs_diarised"

sample <- fromJSON(here(raw_data_loc, "D40SET2_ALL_diarised.json"))$segments |>
  as_tibble() |>
  select(-words)

full <- here(raw_data_loc) |>
  list.files() |>
  map(\(f) {
    fromJSON(here(raw_data_loc, f))$segments |>
      as_tibble() |>
      select(-words) |>
      mutate(source = f)
  }) |>
  bind_rows() |>
  mutate(
    person = case_when(
      speaker == "SPEAKER_00" ~ "A",
      speaker == "SPEAKER_01" ~ "B",
      speaker == "SPEAKER_02" ~ "C",
      speaker == "SPEAKER_03" ~ "D",
      speaker == "SPEAKER_04" ~ "E",
      T ~ speaker
    ), role = "",
    grid = "",
    targetPosition = "", message = text
  ) |>
  mutate(game = as.factor(source) |> as.numeric()) |>
  mutate(
    game = ifelse(str_detect(source, "D01SET1"), 1, game),
    game = as.factor(game) |> as.numeric(),
    message_id_num=row_number()
  )



ready_for_segment <- full |> select(game, grid, targetPosition, role,message_id_num, message)
# selecting red / blue / green from a larger set


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


write_csv(ready_for_segment |> filter(game == 1), here("segmentation/sample/dahan.csv"))
write_csv(ready_for_segment |> filter(game != 1), here("segmentation/remainder/dahan.csv"))
