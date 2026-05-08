library(tidyverse)
library(here)
library(antiword)
library(readxl)

# From the report I uploaded in October, I was able to piece together the fact that
# pairs had an initial set of cards (“Ausgang”) for the first six trials and then
# a set of new cards that were either more (“proximal”) or less (“distal”) similar
# to the original set. We also counterbalanced the setup, in that half of pairs got
# the “proximal” or “distal” set first and then the “Ausgang” set. This is indicated
# in the header of each transcript (“Bedingung” = condition). So for example in Team5,
# the condition is “1. ausg- 2.distal”, meaning they got the “Ausgang” set first and
# then the “distal” set.

original_proximal <- c(1, 2, 6, 10)
original_distal <- c(3, 4, 5)
proximal_original <- c(12, 15, 17, 19)
distal_original <- c(13, 14, 16, 18)

source(here("import/bangerter2000_reuse/prep_segment.R"))
original_labels <- intermediate |>
  distinct(game, source, role, person, grid) |>
  filter(game < 16) |>
  mutate(game_id = str_replace(source, "TEAM_", "") |> str_replace(".docx.txt", "") |> as.numeric()) |>
  select(game, game_id, grid, role, person)
transcripts <- read_csv(here("import/bangerter2000_reuse/raw_data/segmented_transcript.csv")) |>
  inner_join(original_labels) |>
  select(-game) |>
  mutate(
    message_irrelevant = ifelse(targetPosition_w == "chitchat", T, NA),
    trial_num = as.numeric(targetPosition_w)
  ) |>
  group_by(game_id) |>
  fill(trial_num, .direction = "downup") |>
  ungroup() |>
  mutate(
    rep_num = as.numeric(grid),
    trial_num = (grid - 1) * 8 + trial_num,
    action_type = "message",
    player_id = str_c(game_id, "_", person),
    text = message
  ) |>
  group_by(trial_num, game_id) |>
  mutate(message_number = row_number() |> as.numeric()) |>
  ungroup() |>
  select(game_id, player_id, role, text, message_irrelevant, message_number, rep_num, trial_num, action_type)

dummy_messages <- transcripts |> filter(role=="matcher") |> distinct(game_id, trial_num, rep_num) |> 
  anti_join(transcripts |> filter(role=="describer") |> distinct(game_id, trial_num, rep_num)) |> 
  mutate(role="describer") |> left_join(original_labels, by=c("game_id", "rep_num"="grid", "role")) |> 
  mutate(player_id=str_c(game_id, "_", person)) |> 
  select(-game, -person) |> 
  mutate(action_type="message")

combined <- transcripts |> bind_rows(dummy_messages) |> 
  mutate(
    condition_label = case_when(
      game_id %in% original_proximal ~ "original_proximal",
      game_id %in% original_distal ~ "original_distal",
      game_id %in% proximal_original ~ "proximal_original",
      game_id %in% distal_original ~ "distal_original"
    ),
    dataset_id = "bangerter2000_reuse",
    full_cite = "Bangerter, Adrian and Smolenski, Carola (2000). Die Wiederverwendung konzeptueller Pakte bei der Bezugnahme auf neue Gegenstände. Unpublished research report.",
    short_cite = "Bangerter & Smokenski (2000)",
    group_size = 2,
    language = "Swiss German",
    prior_relationship = "no",
    partner_constancy = "yes",
    population = "adult",
    role_constancy = "no",
    confederates = "no",
    modality = "oral-in-person",
    feedback = "none",
    backchannel = "full",
    room_num = 1,
    stage_num = case_when(
      rep_num <= 6 ~ 1,
      rep_num >= 7 ~ 2
    ),
    exclude = NA,
    exclusion_reason = NA_character_,
    order_match = "order",
    time_stamp = NA_real_,
    age = NA_real_,
    gender = NA_character_,
    native_language = NA_character_,
    race = NA_character_,
    education = "some-college",
    choice_id = NA,
    option_set = "unk1;unk2;unk3;unk4;unk5;unk6;unk7;unk8",
    target = "unk1"
  )


source("validate.R")
validate_dataset(combined, write = T)
