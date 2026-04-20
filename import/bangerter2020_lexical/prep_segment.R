library(tidyverse)
library(here)
library(readxl)
library(janitor)

raw_data_loc <- "import/bangerter2020_lexical/raw_data"

study_1_loc <- (here(raw_data_loc, "Transcripts Study 1.xls"))

study_2_loc <- here(raw_data_loc, "Transcriptions complètes Study 2.xlsx")

study_3_loc <- here(raw_data_loc, "transcripts Study 3")

study_1_raw <- study_1_loc |>
  excel_sheets() %>%
  set_names() |>
  as_tibble() |>
  mutate(data = map(value, \(v){
    read_excel(v, path = study_1_loc, col_types = "text") |> nest()
  })) |>
  unnest(data) |>
  unnest(data)

study_1_clean <- study_1_raw |>
  clean_names() |>
  separate(value, into = c("game", "round")) |>
  select(game, round, role = x2, text = x3) |>
  filter(!is.na(text)) |>
  mutate(text = str_replace_all(text, "\\+", "") |> str_replace_all("\\s+", " ") |> trimws()) |>
  rename(source = game) |>
  mutate(
    person = case_when(role == "M" ~ "A", role == "D" ~ "B", T ~ NA),
    role = case_when(
      role == "M" ~ "matcher",
      role == "D" ~ "describer",
      T ~ NA
    ),
    expt = 1,
    rep = as.numeric(round)
  ) |>
  select(-round)

# expt 1 seems to not have switching of M/D roles
# 5 trials, classic v new conditions -- 14 in new 8 in classic (so G# in new and Gletter in classic??)

# expt 2

study_2_raw <- study_2_loc |>
  excel_sheets() %>%
  set_names() |>
  as_tibble() |>
  mutate(data = map(value, \(v){
    read_excel(v, path = study_2_loc, col_names = F, col_types = "text") |> nest()
  })) |>
  unnest(data) |>
  unnest(data)

study_2_clean <- study_2_raw |>
  clean_names() |>
  rename(turn = x1, role = x2, text = x3) |>
  separate_wider_delim(cols = value, delim = " ", too_few = "align_start", names = c(NA, "source", "rep", "foo")) |>
  mutate(
    rep = ifelse(is.na(rep), "", rep),
    foo = ifelse(is.na(foo), "", foo),
    rep =
      str_c(rep, foo) |> str_replace_all("-", "")
  ) |>
  select(-foo) |>
  filter(role != "Role") |>
  mutate(text = str_replace_all(text, "\\(.+\\)", "") |> str_replace_all("\\s+", " ") |> trimws()) |>
  filter(text != "") |>
  select(-turn) |>
  mutate(
    person = case_when(role == "M" ~ "A", role == "D" ~ "B", T ~ NA),
    role = case_when(
      role == "M" ~ "matcher",
      role == "D" ~ "describer",
      T ~ NA
    ),
    expt = 2,
    rep = as.numeric(rep)
  )


# expt 2 -- no role switches?
# again 5 trials for either classic or new -- but groups at now 1-24 and A-F but we think it should be a 15/15 split so who knows!
# then a 6th trial that is always new

study_3_raw <- study_3_loc |>
  list.files() |>
  map(\(f) {
    excel_sheets(paste0(study_3_loc, "/", f)) %>%
      set_names() |>
      as_tibble() |>
      mutate(data = map(value, \(v){
        read_excel(v, path = paste0(study_3_loc, "/", f), col_names = F, col_types = "text") |> nest()
      })) |>
      unnest(data) |>
      unnest(data) |>
      mutate(source = f)
  }) |>
  bind_rows()

study_3_clean <- study_3_raw |>
  clean_names() |>
  rename(turn = x2, role = x3, text = x4) |>
  separate_wider_delim(value, delim = "-", too_few = "align_start", names = c("group", "partner", "rep")) |>
  filter(role != "Rôle") |>
  mutate(text = str_replace_all(text, "\\(.+\\)", "") |> str_replace_all("\\+", "") |>
    str_replace_all("\\[.+\\]", "") |>
    str_replace_all("\\s+", " ") |> trimws()) |>
  filter(text != "") |>
  filter(!is.na(partner)) |>
  select(group, partner, role, rep, text) |>
  mutate(
    person = case_when(
      role == "M" & partner == "1" ~ "A",
      role == "D" ~ "B",
      role == "M" & partner == "2" ~ "C",
      T ~ NA
    ),
    role = case_when(
      role == "M" ~ "matcher",
      role == "D" ~ "describer",
      T ~ NA
    ),
    rep = case_when(partner == "1" ~ as.numeric(rep), partner == "2" ~ as.numeric(rep) + 4),
    expt = 3
  ) |>
  rename(source = group) |>
  select(-partner)

# expt 3 = 24 groups of 3 people, D and M1 play for 4 reps, then D and M2 play for 4 reps,
# again half classic, half mixed, but we don't which (numbered 1-24)


all <- study_1_clean |>
  bind_rows(study_2_clean) |>
  bind_rows(study_3_clean) |>
  mutate(
    gameid = str_c(expt, "_", source),
    game = as.factor(gameid) |> as.numeric()
  ) |>
  rename(message = text) |>
  mutate(
    grid = rep,
    targetPosition = "",
    message_id_num = row_number()
  )

ready_for_segment <- all |> select(game, grid, targetPosition, role, message_id_num, message)
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

ready_for_segment |> distinct(game)
ready_for_segment |> distinct(game, grid)

ready_for_segment |>
  mutate(words = str_count(message, "\\S+")) |>
  summarize(sum = sum(words))

write_csv(ready_for_segment |> filter(game == 1), here("segmentation/sample/bangerter2020.csv"))
write_csv(ready_for_segment |> filter(game != 1), here("segmentation/remainder/bangerter2020.csv"))
