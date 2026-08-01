library(tidyverse)
library(here)
library(antiword)
library(readxl)

# Overall experiment structure:
# 3 participant role conditions (SP = Side Participant; OH = Overhearer; NP = Naïve Participant)
# On each trial, director describes (same, randomised) set of 8 tangrams to matcher.
# Trials A1-4 – director describes to matcher A; Trials B1-4 – director describes to matcher B
# 24 groups of participants (8/participant role condition)

# Within transcription:
#   A/B[1-4] = Matcher A/B[Trial number]
# • Eg A4 = Matcher A, Trial 4; B2 = Matcher B, Trial 2
# D = Director, A = Matcher A, B = Matcher B

raw_data_loc <- "import/branigan2016_doyouknow/raw_data"

process_transcript <- function(filename) {
  group <- str_extract(filename, "[0-9]+")
  condition <- str_extract(filename, "(NP|OH|SP)")

  sample_data <- antiword(here(raw_data_loc, "transcriptions-with-group-names", filename)) |>
    read_lines() |>
    as_tibble() |>
    mutate(value = str_replace(value, "–", "-")) |>
    separate(value, into = c("speaker", "description"), sep = ":", fill = "left") |>
    separate("speaker", into = c("trial", "speaker"), sep = "-", fill = "left") |>
    mutate(
      round = case_when(
        str_detect(description, "A1") ~ "A1",
        str_detect(description, "A2") ~ "A2",
        str_detect(description, "A3") ~ "A3",
        str_detect(description, "A4") ~ "A4",
        str_detect(description, "B1") ~ "B1",
        str_detect(description, "B2") ~ "B2",
        str_detect(description, "B3") ~ "B3",
        str_detect(description, "B4") ~ "B4"
      ),
      description = str_replace_all(description, "(A|B)(1|2|3|4)", "")
    ) |>
    fill(c("trial", "speaker", "round")) |>
    filter(!is.na(speaker)) |>
    filter(!str_detect(description, "F[0-9]")) |>
    mutate(
      description = str_squish(description),
      trial = str_squish(trial),
      round = str_squish(round),
      speaker = str_squish(speaker)
    ) |>
    filter(!is.na(description)) |>
    filter(!description == "") |>
    mutate(group = group, condition = condition)

  return(sample_data)
}


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

combined <- transcripts |>
  full_join(full_describers) |>
  left_join(ordering) |>
  group_by(trial, group, round) |>
  mutate(message_num = row_number() |> as.numeric()) |>
  ungroup() |>
  mutate(
    dataset_id = "branigan2016_doyouknow",
    full_cite = "Branigan, H. P., Bell, J., & McLean, J. F. (2016). Do you know what i know? The impact of participant role in children's referential communication. Frontiers in psychology, 7, 213.",
    short_cite = "Branigan et al. (2016)",
    group_size = 3,
    language = "English",
    prior_relationship = "yes",
    partner_constancy = "no", # could revisit
    population = "child",
    role_constancy = "yes",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "limited",
    backchannel = "full",
    room_num = 1,
    image_options = "A;B;C;D;G;H;I;J",
    target_image = case_when(
      tangram == 1 ~ "G",
      tangram == 2 ~ "H",
      tangram == 3 ~ "I",
      tangram == 4 ~ "J",
      tangram == 5 ~ "A",
      tangram == 6 ~ "B",
      tangram == 7 ~ "C",
      tangram == 8 ~ "D"
    ),
    stage = str_sub(round, 1, 1),
    round = str_sub(round, 2, -1) |> as.numeric(),
    stage_num = ifelse(stage == "A", 1, 2),
    round_num = (stage_num - 1) * 4 + round,
    trial_num = as.numeric(trial) + 8 * (round_num - 1),
    exclude = F,
    exclusion_reason = as.character(NA),
    order_match = "order",
    action_type = "message",
    player_id = str_c(group, "_", speaker),
    role = ifelse(speaker == "D", "describer", "matcher"),
    time_stamp = as.numeric(NA),
    age = as.numeric(NA),
    gender = as.character(NA),
    native_language = as.character(NA), # presumably mostly English for most kids
    race = as.character(NA),
    gender = as.character(NA),
    education = "less-than-high-school",
    selected_image = NA,
    message_irrelevant = FALSE,
    condition_label = case_when(
      condition == "NP" ~ "naive participant",
      condition == "SP" ~ "side participant",
      condition == "OH" ~ "overhearer"
    )
  ) |>
  rename(game_id = group, text = description) |>
  select(-trial, -words, -speaker, -round, -tangram, -stage, -set, -condition)

source("validate.R")
validate_dataset(combined, write = T)
