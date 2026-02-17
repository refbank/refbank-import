library(tidyverse)
library(here)
library(antiword)
library(readxl)
library(janitor)
raw_data_loc <- "import/bangerter2020_lexical/raw_data"


### For figuring out conditions
# * In experiment 1, there are games numbered 1-16 and then lettered A-F, but we expect 14 in one condition and 8 in the other as per paper
# ---Pairs 9-16 are in the condition with 8 pairs, Pairs 1-8 and A-F are in the condition with 14 pairs
#
# * In experiment 2, there are games 1-24 and then A-F, paper reports 15 in each condition.
# ---Here’s the breakdown by condition (A-F is groups 20 onwards)
# ---- Classic Condition Pairs 3       4            5            6            7             8            11          16          20          21          22          24               25          27          28
# ---- New Cards Condition Pairs 1               2            9            10          12          13          14          15          17          18          19               23          26          29          30
#
# * In experiment 3, there are games 1-24, paper reports 12 in each condition.
# ----1-12=new cars, 13-24=classic
#
# Regarding the 2000 data, I’ll have to do some more digging. I’ll get back to you Monday or Tuesday.
#
# Best,
# Adrian
study_1_loc <- (here(raw_data_loc, "Transcripts Study 1.xls"))


study_1_raw <- study_1_loc |>
  excel_sheets() %>%
  set_names() |>
  as_tibble() |>
  mutate(data = map(value, \(v){
    read_excel(v, path = study_1_loc) |> nest()
  })) |>
  unnest(data) |>
  unnest(data)

study_1_clean <- study_1_raw |>
  clean_names() |>
  separate(value, into = c("game", "round")) |>
  select(game, round, role = x2, text = x3) |>
  filter(!is.na(text)) |>
  rowwise() |>
  write_csv(here(raw_data_loc, "study1.csv"))


source("validate.R")
validate_dataset(combined, write = T)
