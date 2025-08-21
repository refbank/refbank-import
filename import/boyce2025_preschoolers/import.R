library(tidyverse)
library(here)
source(here("validate.R"))

demogs <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/clean_demog_1_2.csv"

expt_1_transcript <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_1/timed_transcript.csv"
expt_1_responses <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_1/clean_data.csv"

expt_2_transcript <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_2/transcripts.csv"
expt_2_responses <- "https://raw.githubusercontent.com/vboyce/kid-tangrams/refs/heads/main/data/expt_2/clean_data.csv"

expt_1_messages <- read_csv(url(expt_1_transcript)) |>
  filter(role %in% c("S", "L")) |>
  select(game, text, description, speaker, trial, role) |>
  group_by(game, trial) |>
  mutate(message_number = row_number()) |>
  ungroup() |>
  mutate(
    trialNum = trial - 1,
    repNum = (trialNum - 1) %/% 4,
    action_type = "message",
    message_irrelevant = ifelse(is.na(description), T, F),
    role = ifelse(role == "S", "describer", "matcher")
  ) |>
  filter(trialNum > 0) |>
  select(game,
    playerId = speaker, role, trialNum, repNum, action_type,
    message_number, text, message_irrelevant
  )

expt_1_choices <- read_csv(url(expt_1_responses)) |>
  filter(!is.na(repNum)) |>
  select(game, trialNum, response, listener, distractor, target) |>
  mutate(
    trialNum = trialNum - 1,
    repNum = (trialNum - 1) %/% 4
  ) |>
  rename(playerId = listener, choice_id = response) |>
  mutate(choice_id = ifelse(is.na(choice_id), "timed_out", choice_id)) |>
  mutate(
    action_type = "selection",
    role = "matcher"
  )

expt_1_no_talk <- read_csv(url(expt_1_responses)) |>
  filter(!is.na(repNum)) |>
  select(game, trialNum, response, speaker) |>
  mutate(
    trialNum = trialNum - 1,
    repNum = (trialNum - 1) %/% 4
  ) |>
  anti_join(expt_1_messages) |>
  mutate(text = NA, playerId = speaker, role = "describer", action_type = "message")


expt_1_exclude <- expt_1_no_talk |>
  select(game, trialNum) |>
  mutate(exclude = T, exclusion_reason = "no describer talk")

expt_1_options <- expt_1_choices |>
  select(target, distractor, trialNum, game) |>
  mutate(option_set = str_c(target, distractor, sep = ";")) |>
  select(-distractor)

expt_1 <- expt_1_messages |>
  bind_rows(expt_1_choices) |>
  bind_rows(expt_1_no_talk) |>
  mutate(condition_label = "expt_1") |>
  select(-target) |>
  left_join(expt_1_options) |>
  left_join(expt_1_exclude)


id_sub <- tribble(
  ~playerId, ~actual,
  "apple123", "id74",
  "pear124", "id75",
  "apple127", "id76",
  "pear128", "id77",
  "80", "id78",
  "81", "id79",
  "apple111", "id90",
  "pear222", "id91",
  "apple333", "id92",
  "pear444", "id93",
  "apple255", "id94",
  "pear257", "id95"
)

expt_2_messages <- read_csv(url(expt_2_transcript)) |>
  filter(speaker != "R") |> # note that this also indicates transcript error!
  filter(role %in% c("S", "L")) |>
  select(game = gameConfig, gameId, description, text, speaker, trial, role) |>
  group_by(game, gameId, trial) |>
  mutate(message_number = row_number()) |>
  ungroup() |>
  mutate(
    trialNum = trial - 3,
    repNum = (trialNum - 1) %/% 4,
    action_type = "message",
    message_irrelevant = ifelse(is.na(description), T, F),
    role = ifelse(role == "S", "describer", "matcher")
  ) |>
  filter(trialNum > 0) |>
  select(game, gameId,
    playerId = speaker, role, trialNum, repNum, action_type,
    message_number, text, message_irrelevant
  ) |>
  left_join(id_sub) |>
  mutate(actual = case_when(
    !is.na(actual) ~ actual,
    str_detect(playerId, "id") ~ playerId,
    T ~ str_c("id", playerId)
  )) |>
  select(-playerId) |>
  rename(playerId = actual)

expt_2_game_mapping <- expt_2_messages |>
  select(game, gameId) |>
  unique()

expt_2_choices <- read_csv(url(expt_2_responses)) |>
  filter(!is.na(repNum)) |>
  select(gameId, trialNum, response, listener, target, distractor) |>
  mutate(
    trialNum = trialNum - 3,
    repNum = (trialNum - 1) %/% 4
  ) |>
  rename(playerId = listener, choice_id = response) |>
  mutate(action_type = "selection", role = "matcher") |>
  mutate(choice_id = ifelse(is.na(choice_id), "timed_out", choice_id)) |>
  left_join(expt_2_game_mapping) |>
  left_join(id_sub) |>
  mutate(actual = case_when(
    !is.na(actual) ~ actual,
    str_detect(playerId, "id") ~ playerId,
    T ~ str_c("id", playerId)
  )) |>
  select(-playerId) |>
  rename(playerId = actual)

expt_2_no_talk <- read_csv(url(expt_2_responses)) |>
  filter(!is.na(repNum)) |>
  select(gameId, trialNum, response, speaker) |>
  left_join(expt_2_game_mapping) |>
  mutate(
    trialNum = trialNum - 3,
    repNum = (trialNum - 1) %/% 4
  ) |>
  anti_join(expt_2_messages) |>
  mutate(text = NA, playerId = speaker, role = "describer", action_type = "message")

echoing <- read_csv(url(expt_2_transcript)) |>
  filter(!is.na(echo)) |>
  mutate(
    trialNum = trial - 3,
    repNum = (trialNum - 1) %/% 4
  ) |>
  filter(trialNum > 0) |>
  select(game = gameConfig, trialNum) |>
  mutate(exclude = T, exclusion_reason = "researcher echoing")

expt_2_exclude <- expt_2_no_talk |>
  select(game, trialNum) |>
  mutate(exclude = T, exclusion_reason = "no describer talk") |>
  bind_rows(echoing)

expt_2_options <- expt_2_choices |>
  select(target, distractor, trialNum, game) |>
  mutate(option_set = str_c(target, distractor, sep = ";")) |>
  select(-distractor)

expt_2 <- expt_2_messages |>
  bind_rows(expt_2_choices) |>
  bind_rows(expt_2_no_talk) |>
  mutate(condition_label = "expt_2") |>
  select(-gameId) |>
  select(-target) |>
  left_join(expt_2_options) |>
  left_join(expt_2_exclude) |>
  mutate(playerId = case_when(
    game == "game76" & playerId == "id130" ~ "id139",
    T ~ playerId
  )) # fix a typo


demographics <- read_csv((url(demogs))) |>
  mutate(
    age = age_month / 12,
    hispanic = ifelse(hispanic == "yes", "hispanic", ""),
    race = str_c(hispanic, ethnicity),
    education = as.character(NA), # we are not making a "preschool" category
    player_id = id
  )


all <- expt_1 |>
  bind_rows(expt_2) |>
  rename(game_id = game, player_id = playerId, trial_num = trialNum)


all_fix_role <- all |>
  select(condition_label, game_id, player_id, trial_num, role) |>
  mutate(trial_parity = trial_num %% 2 == 0) |>
  group_by(trial_parity, condition_label, game_id, player_id, role) |>
  tally() |>
  pivot_wider(values_from = n, names_from = role, values_fill = 0) |>
  mutate(role = ifelse(describer > matcher, "describer", "matcher")) |>
  select(trial_parity, condition_label, game_id, player_id, role)



all_fixed <- all |>
  mutate(trial_parity = trial_num %% 2 == 0) |>
  select(-role) |>
  left_join(all_fix_role) |>
  select(-trial_parity)


missing_describers <- all_fixed |>
  select(game_id, repNum, trial_num, condition_label, target, option_set) |>
  unique() |>
  left_join(all_fixed |> filter(role == "describer")) |>
  filter(is.na(role)) |>
  mutate(role = "describer") |>
  mutate(trial_parity = trial_num %% 2 == 0) |>
  select(trial_parity, repNum, trial_num, condition_label, game_id, role, target, option_set) |>
  left_join(all_fix_role) |>
  mutate(action_type = "message")

fixed <- all_fixed |>
  bind_rows(missing_describers) |>
  left_join(demographics) |>
  mutate(
    dataset_id = "boyce2025_preschoolers",
    full_cite = "Boyce, V., Sparks, R., Mofor, Y., & Frank, M. C. Preschoolers can form conventional pacts with each other to communicate about novel referents. Preprint. 2025.",
    short_cite = "Boyce et al. (2025)",
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
    time_stamp = as.numeric(NA),
    native_language = as.character(NA),
    message_number = as.numeric(message_number),
    game_id = str_c(condition_label, "_", game_id),
  ) |>
  select(dataset_id, full_cite, short_cite, group_size, language, condition_label,
    prior_relationship, partner_constancy, population, role_constancy,
    confederates, modality, feedback, backchannel, order_match,
    game_id, room_num, option_set,
    trial_num,
    rep_num = repNum, stage_num, action_type, target,
    exclude, exclusion_reason,
    role, time_stamp,
    native_language, player_id, age, gender, race, education,
    text, message_number, message_irrelevant, choice_id
  )

# we think there are rare transcription errors in role, but we know that role alternated by trialnum
# so until V fixes in original, do with majority fix




validate_dataset(fixed, write=T)
