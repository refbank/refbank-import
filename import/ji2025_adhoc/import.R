library(here)
library(tidyverse)
library(jsonlite)

data_dir <- "import/ji2025_adhoc/raw_data"

ParseJSONColumn <- function(x) {
  x |>
    str_replace_all("\\\\", "") |>
    str_replace_all('"', "'") |>
    str_replace_all(fixed("{'"), '{"') |>
    str_replace_all(fixed("'}"), '"}') |>
    str_replace_all("':", '":') |>
    str_replace_all(": '", ': "') |>
    str_replace_all("', '", '", "') |>
    fromJSON(flatten = T)
}


expt_1 <- read_csv(here(data_dir, "exp1_data.csv")) |>
  mutate(condition_label = "expt1_singlemessage") |>
  mutate(sec_until_click = as.double(sec_until_click)) |>
  mutate(stage_num = 1)

expt_2 <- read_csv(here(data_dir, "exp2_data.csv")) |>
  mutate(condition_label = "expt2_singlemessage") |>
  mutate(sec_until_click = as.double(sec_until_click)) |>
  mutate(stage_num = ifelse(block == 5, 2, 1))

expt_3 <- read_csv(here(data_dir, "exp3_data.csv")) |>
  mutate(condition_label = "expt3") |>
  mutate(sec_until_click = as.double(sec_until_click)) |>
  mutate(stage_num = 1)

messages_single <- expt_1 |>
  bind_rows(expt_2) |>
  select(
    game_id, trial_index, block, target_image = target, controlled,
    speaker_id, context, description, time_to_message,
    condition_label, stage_num
  ) |>
  mutate(
    player_id = speaker_id,
    role = "describer",
    time_stamp = time_to_message,
    message_num = 1,
    message_irrelevant = F,
    action_type = "message",
    text = description
  )

messages_complex <- expt_3 |>
  select(
    game_id, block, target_image = target, trial_index,
    condition_label, description, context, speaker_id, listener_id, stage_num
  ) |>
  mutate(description = map(description, ParseJSONColumn)) |>
  unnest(description) |>
  group_by(game_id, block, target_image, trial_index) |>
  mutate(message_num = row_number()) |>
  ungroup() |>
  mutate(
    role = ifelse(role == "speaker", "describer", "matcher"),
    player_id = ifelse(role == "describer", speaker_id, listener_id),
    action_type = "message"
  )


selections <- expt_1 |>
  bind_rows(expt_2) |>
  bind_rows(expt_3) |>
  select(
    game_id, trial_index, block, target_image = target, controlled,
    listener_id, context,
    sec_until_press, sec_until_click, response, condition_label, stage_num
  ) |>
  mutate(
    player_id = listener_id, time_stamp = ifelse(!is.na(sec_until_press), sec_until_press, sec_until_click),
    action_type = "selection", selected_image = response, role = "matcher"
  ) |>
  mutate(selected_image = ifelse(is.na(selected_image), "timed_out", selected_image))

### combine everything
all <- messages_single |>
  bind_rows(selections) |>
  bind_rows(messages_complex) |>
  mutate(
    dataset_id = "ji2025_adhoc",
    full_cite = "Ji, A., Bergey, C. A., Eliav, R., Artzi, Y., & Hawkins, R. D. (2025). Ad hoc conventions generalize to new referents. arXiv preprint arXiv:2509.05566.",
    short_cite = "Ji et al. (2025)",
    language = "English",
    prior_relationship = "no",
    partner_constancy = "yes",
    role_constancy = "no",
    population = "adult",
    confederates = "no",
    modality = "written",
    backchannel = ifelse(condition_label == "expt3", "full", "none"),
    feedback = ifelse(condition_label == "expt3", "full", "limited"),
    order_match = "match",
    room_num = 1, # only 1 group / game
    age = as.numeric(NA),
    native_language = "English",
    gender = as.character(NA),
    race = as.character(NA),
    education = as.character(NA),
    trial_num = trial_index + 1,
    round_num = 1 + block,
    group_size = 2,
    target_image = str_replace(target_image, ".svg", "") |> str_replace_all("page-", ""),
    selected_image = str_replace(selected_image, ".svg", "") |> str_replace_all("page-", ""),
    exclude = as.logical(NA), # TODO figure out
    exclusion_reason = as.character(NA), # TODO figure out
    image_options = str_replace_all(context, ".svg", "") |> str_replace_all("', '", ";") |>
      str_replace_all(fixed("['"), "") |> str_replace_all(fixed("']"), "") |> str_replace_all("page-", ""),
  ) |>
  select(
    dataset_id, full_cite, short_cite, language, prior_relationship,
    partner_constancy, role_constancy, population, confederates, modality, backchannel,
    feedback, order_match,
    condition_label, time_stamp, stage_num,
    game_id, room_num,
    player_id, age, gender, race, education, native_language,
    stage_num,
    trial_num, round_num,
    role, target_image,
    action_type, exclude, exclusion_reason,
    message_num, text,
    selected_image, image_options,
    group_size, message_irrelevant,
  )

source(here("validate.R"))

validate_dataset(all, write = T)
