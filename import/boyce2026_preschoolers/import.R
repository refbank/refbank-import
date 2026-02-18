library(tidyverse)
library(here)
source(here("validate.R"))


expt_1_transcript <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_1/transcripts.csv"
expt_1_responses <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_1/clean_data.csv"
expt_1_demog <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_1/clean_demog.csv"
expt_1_link <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_1/link_transcripts.csv"


expt_2_transcript <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_2/transcripts.csv"
expt_2_responses <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_2/clean_data.csv"
expt_2_link <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_2/link_transcripts.csv"
expt_2_demog <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_2/clean_demog.csv"


expt_3_transcript <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_3/transcripts.csv"
expt_3_responses <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_3/clean_data.csv"
expt_3_link <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_3/link_transcripts.csv"
expt_3_demog <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_3/clean_demog.csv"



expt_1_data <- read_csv(url(expt_1_responses)) |>
  rename(trial = trialNum) |>
  mutate(type = case_when(
    trial < 2 ~ "practice",
    trial < 6 ~ "block 1",
    trial < 10 ~ "block 2",
    trial < 14 ~ "block 3",
  )) |>
  filter(!is.na(response)) |>
  mutate(correct = as.numeric(correct)) |>
  mutate(expt = "expt1") |>
  mutate(trial = trial - 2)

expt_1_transcript <- read_csv(url(expt_1_transcript)) |>
  filter(on_topic == "Y") |>
  mutate(type = case_when(
    trial < 2 ~ "practice",
    trial < 6 ~ "block 1",
    trial < 10 ~ "block 2",
    trial < 14 ~ "block 3",
  )) |>
  mutate(expt = "expt1") |>
  mutate(trial = trial - 2)

expt_2_data <- read_csv(url(expt_2_responses)) |>
  rename(trial = trialNum) |>
  mutate(type = case_when(
    trial < 4 ~ "practice",
    trial < 8 ~ "block 1",
    trial < 12 ~ "block 2",
    trial < 16 ~ "block 3",
    trial < 20 ~ "block 4"
  )) |>
  filter(!is.na(response)) |>
  mutate(correct = as.numeric(correct)) |>
  mutate(expt = "expt2") |>
  mutate(trial = trial - 4)

expt_2_transcript <- read_csv(url(expt_2_transcript)) |>
  filter(on_topic == "Y") |>
  mutate(type = case_when(
    trial < 4 ~ "practice",
    trial < 8 ~ "block 1",
    trial < 12 ~ "block 2",
    trial < 16 ~ "block 3",
    trial < 20 ~ "block 4"
  )) |>
  mutate(expt = "expt2") |>
  mutate(trial = trial - 4)



expt_3_data <- read_csv(url(expt_3_responses)) |>
  rename(trial = trialNum) |>
  mutate(type = case_when(
    trial < 4 ~ "practice",
    trial < 8 ~ "block 1",
    trial < 12 ~ "block 2",
    trial < 16 ~ "block 3",
    trial < 20 ~ "block 4"
  )) |>
  filter(!is.na(response)) |>
  mutate(correct = as.numeric(correct)) |>
  mutate(expt = "expt3") |>
  mutate(trial = trial - 4)

expt_3_transcript <- read_csv(url(expt_3_transcript)) |>
  filter(on_topic == "Y") |>
  mutate(type = case_when(
    trial < 4 ~ "practice",
    trial < 8 ~ "block 1",
    trial < 12 ~ "block 2",
    trial < 16 ~ "block 3",
    trial < 20 ~ "block 4"
  )) |>
  mutate(expt = "expt3") |>
  mutate(trial = trial - 4)


demogs <- read_csv(url(expt_1_demog)) |>
  mutate(expt = as.character(expt)) |>
  bind_rows(read_csv(url(expt_2_demog)) |> mutate(expt = as.character(expt))) |>
  bind_rows(read_csv(url(expt_3_demog)) |>
    mutate(expt = as.character(expt))) |>
  mutate(
    age = floor(age_month / 12),
    hispanic = case_when(
      hispanic == "yes" ~ "hispanic",
      T ~ ""
    ),
    race = str_c(ethnicity, " ", hispanic),
    expt = str_c("expt", expt),
    playerId = str_c(expt, "_", id)
  ) |>
  select(-id)

# construct appropriate chunks

messages <- expt_1_transcript |>
  bind_rows(expt_2_transcript) |>
  bind_rows(expt_3_transcript) |>
  select(game, text, speaker, trial, role, type, expt) |>
  group_by(game, trial, expt) |>
  mutate(message_number = row_number()) |>
  ungroup() |>
  mutate(
    action_type = "message",
    role = ifelse(role == "S", "describer", "matcher"),
    message_irrelevant = NA
  ) |>
  filter(type != "practice") |>
  select(game,
    playerId = speaker, role, trial, action_type,
    message_number, text, message_irrelevant, expt
  )

choices <- expt_1_data |>
  bind_rows(expt_2_data) |>
  bind_rows(expt_3_data) |>
  inner_join(read_csv(url(expt_1_link)) |>
    mutate(expt = "expt1") |>
    bind_rows(
      read_csv(url(expt_2_link)) |> mutate(expt = "expt2")
    ) |>
    bind_rows(
      read_tsv(url(expt_3_link)) |> mutate(expt = "expt3")
    ) |> select(game, expt)) |>
  filter(type != "practice") |>
  select(game, trial, response, listener, distractor, target, expt, time) |>
  rename(playerId = listener, choice_id = response) |>
  mutate(choice_id = case_when(
    is.na(choice_id) ~ "timed_out",
    choice_id == "baby" ~ "H",
    choice_id == "zombie" ~ "A",
    choice_id == "dancer" ~ "G",
    choice_id == "bunny" ~ "E",
    T ~ choice_id
  )) |>
  mutate(
    action_type = "selection",
    role = "matcher",
    time_stamp=round(time/1000) 
  )

no_talk <- expt_1_data |>
  bind_rows(expt_2_data) |>
  bind_rows(expt_3_data) |>
  filter(type != "practice") |>
  select(expt, game, trial, response, playerId = speaker) |>
  anti_join(messages) |>
  mutate(text = NA, role = "describer", action_type = "message")


exclude <- read_csv(url(expt_1_link)) |>
  mutate(expt = "expt1") |>
  bind_rows(
    read_csv(url(expt_2_link)) |> mutate(expt = "expt2")
  ) |>
  bind_rows(
    read_tsv(url(expt_3_link)) |> mutate(expt = "expt3")
  ) |>
  filter(!is.na(exclude)) |>
  mutate(exclude = T)

no_talk_exclude <- no_talk |>
  select(game, trial, expt) |>
  anti_join(exclude |> select(game, expt)) |>
  mutate(exclude = T, exclusion_reason = "no describer talk")

echo_exclude <- expt_2_transcript |>
  bind_rows(expt_3_transcript) |>
  filter(!is.na(echo)) |>
  select(game, trial, expt) |>
  anti_join(exclude |> select(game, expt)) |>
  anti_join(no_talk_exclude |> select(game, trial, expt)) |>
  mutate(exclude = T, exclusion_reason = "experimentor echoed description")

exclude_all <- exclude |> bind_rows(no_talk_exclude, echo_exclude)

options <- choices |>
  mutate(
    target = case_when(
      target == "baby" ~ "H",
      target == "zombie" ~ "A",
      target == "dancer" ~ "G",
      target == "bunny" ~ "E",
      T ~ target
    ),
    distractor = case_when(
      distractor == "baby" ~ "H",
      distractor == "zombie" ~ "A",
      distractor == "dancer" ~ "G",
      distractor == "bunny" ~ "E",
      T ~ distractor
    )
  ) |>
  select(target, distractor, trial, game, expt) |>
  mutate(option_set = str_c(target, distractor, sep = ";")) |>
  select(-distractor)

all <- messages |>
  bind_rows(choices) |>
  bind_rows(no_talk) |>
  select(-target) |>
  left_join(exclude_all) |>
  left_join(options) |>
  filter(!is.na(target)) |>
  rename(condition_label = expt) |>
  mutate(
    dataset_id = "boyce2026_preschoolers",
    full_cite = "Boyce, V., Sparks, R. Z., & Frank, M. C. (2026). Preschoolers can coordinate with each other to communicate about novel referents. Preprint",
    short_cite = "Boyce et al (2026)",
    group_size = 2,
    language = "English",
    prior_relationship = "yes",
    partner_constancy = "yes",
    population = "child",
    role_constancy = "no",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "full",
    backchannel = "full",
    order_match = "match",
    room_num = 1,
    stage_num = 1,
    rep_num = (trial) %/% 4 + 1,
    trial_num = trial + 1,
    native_language = as.character(NA),
    education = "less-than-high-school",
    message_number = as.numeric(message_number),
    game_id = str_c(condition_label, "_", game),
    player_id = str_c(condition_label, "_", playerId),
  ) |> # note that there are some limited cases where a participant is repeated
  mutate(player_id = case_when(
    # duplicate within experiment
    player_id == "expt3_id186" ~ "expt2_id67",
    player_id == "expt3_id185" ~ "expt3_id182",
    player_id == "expt3_id210" ~ "expt3_id192",
    player_id == "expt3_id269" ~ "expt3_id183",
    player_id == "expt3_id213" ~ "expt3_id159",
    player_id == "expt3_id214" ~ "expt2_id95",

    # across expt 2 and 3 was allowed
    player_id == "expt3_id204" ~ "expt2_id61",
    player_id == "expt3_id181" ~ "expt2_id67",
    player_id == "expt3_id252" ~ "expt2_id71",
    player_id == "expt3_id159" ~ "expt2_id92",
    player_id == "expt3_id160" ~ "expt2_id95",
    player_id == "expt3_id176" ~ "expt2_id98",
    player_id == "expt3_id157" ~ "expt2_id140",
    player_id == "expt3_id238" ~ "expt2_id148",

    # across expt 1 and 2 was not intended
    player_id == "expt2_id113" ~ "expt1_id5",
    player_id == "expt2_id112" ~ "expt1_id10",
    player_id == "expt2_id110" ~ "expt1_id36",
    player_id == "expt2_id129" ~ "expt1_id55",
    player_id == "expt2_id133" ~ "expt1_id57",
    T ~ player_id,
  )) |>
  left_join(demogs |> select(-expt) |> rename(player_id = playerId), by = c("player_id")) |>
  select(
    dataset_id, full_cite, short_cite, group_size, language, condition_label,
    prior_relationship, partner_constancy, population, role_constancy,
    confederates, modality, feedback, backchannel, order_match,
    game_id, room_num, option_set,
    trial_num,
    rep_num, stage_num, action_type, target,
    exclude, exclusion_reason,
    role, time_stamp,
    native_language, player_id, age, gender, race, education,
    text, message_number, message_irrelevant, choice_id
  )

validate_dataset(all, write = T)
