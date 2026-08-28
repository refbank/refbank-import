library(tidyverse)
library(here)
source(here("validate.R"))

raw <- read_csv(here("import/beatty-martinez2026_tangrams/raw_data/tcc_utterance_osf.csv")) |>
  mutate(round_no = case_when(
    game_no == 14 & utterance_no == 149 ~ 55, # seems like a typo, this is the number it should be going in order
    T ~ round_no
  ))

role_parity <- raw |>
  mutate(rep_parity = rep_no %% 2) |>
  filter(role == "director") |>
  select(game_no, participant, role, rep_parity) |>
  distinct()

roles <- role_parity |>
  mutate(rep_parity = (rep_parity + 1) %% 2, role = "matcher") |>
  bind_rows(role_parity)

selections <- raw |>
  select(game_no, round_no, rep_no, target_image = target, response) |>
  distinct() |> # raw is one row per utterance, so target_image/response repeat across a trial's utterances
  mutate(
    selected_image = case_when(
      response == "FALSE" ~ "timed_out",
      T ~ response
    ), role = "matcher",
    rep_parity = rep_no %% 2
  ) |>
  left_join(roles) |>
  mutate(action_type = "selection") |>
  select(game_no, round_no, rep_no, target_image, selected_image, action_type, participant, role)


messages <- raw |>
  mutate(message_irrelevant = ifelse(utterance_related == 0, T, F)) |>
  group_by(game_no, round_no, rep_no) |>
  mutate(message_num = row_number() |> as.numeric()) |>
  ungroup() |>
  mutate(
    action_type = "message",
    role = case_when(
      role == "director" ~ "describer",
      T ~ role
    )
  ) |>
  filter(!is.na(utterance)) |>
  select(game_no, round_no, rep_no, target_image = target, participant, role, text = utterance, message_irrelevant, message_num, action_type)

missing_messages <- selections |>
  distinct(game_no, round_no, rep_no, target_image) |>
  anti_join(
    messages |> filter(role == "describer") |> distinct(game_no, round_no, rep_no, target_image)
  ) |>
  mutate(action_type = "message", rep_parity = rep_no %% 2) |>
  mutate(role = "director") |>
  left_join(roles) |>
  mutate(role = "describer") |>
  select(-rep_parity)

all <- selections |>
  bind_rows(messages) |>
  bind_rows(missing_messages) |>
  mutate(
    condition_label = "codeswitch",
    dataset_id = "beatty-martinez2026_tangrams",
    full_cite = "Beatty-Martínez, A. L., Jiaze, L., Shen, Y., Mulík, S., Hawkins, R. D., Guzzardo Tamargo, R. E., & Dussias, P. E. (2026). The Tangrams Codeswitching Corpus.",
    short_cite = "Beatty-Martinez et al. (2026)",
    group_size = 2,
    language = "Spanish/English",
    prior_relationship = "yes",
    partner_constancy = "yes",
    population = "adult",
    role_constancy = "no",
    confederates = "no",
    modality = "written",
    feedback = "full",
    backchannel = "full",
    room_num = 1,
    image_options = "A;B;C;D;E;F;G;H;I;J;K;L",
    trial_num = round_no + 1,
    round_num = rep_no + 1,
    stage_num = 1,
    exclude = F,
    exclusion_reason = NA_character_,
    order_match = "match",
    time_stamp = NA_real_,
    age = NA_real_,
    gender = NA_character_,
    native_language = NA_character_,
    race = NA_character_,
    education = NA_character_
  ) |>
  rename(game_id = game_no, player_id = participant) |>
  select(-round_no, -rep_no)
validate_dataset(all, write = T)
