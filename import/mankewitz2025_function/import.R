library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))

raw_data_dir <- "import/mankewitz2025_function/raw_data/raw_data/"
preprocessed_raw_data_dir <- "import/mankewitz2025_function/raw_data/preprocessed_data/"

target_experiment_names <- list.files(here(raw_data_dir))

old_conditions <- c("noncomp", "comp-within", "comp-between")
new_conditions_long <- c(
  "No Competitor",
  "Within Trial Competitor",
  "Across Trial Competitor"
)

load_data <- function(data_name, study_list) {
  source_files <- list.files(
    path = here(preprocessed_raw_data_dir),
    recursive = TRUE,
    pattern = paste0(data_name, ".csv$"),
    full.names = TRUE
  )
  # Bind the rows of only the data files in the target experiment/study list
  data_files <- source_files[Reduce("|", lapply(study_list, function(x) grepl(x, source_files)))]
  if (data_name == "round") { # the round.csv needs cleaning up
    do.call(bind_rows, lapply(data_files, function(file) {
      df <- read.csv(file) |>
        select(
          roundID = id, correct, gameID, index, numTrials, repNum,
          response, target, targetNum, trialNum, tangramURLs, director
        )
      df$correct <- as.logical(df$correct) # Convert to logical
      return(df)
    }))
  } else {
    do.call(bind_rows, lapply(data_files, read.csv))
  }
}

# sort free-response gender prompt into male/female/nonbinary
bin_gender <- function(gender_val) {
  low_gender <- tolower(gender_val)
  return_val <- case_when(
    low_gender %in% c("female", "female ", "f", "woman", "femal", "femals", "femail", "famale", "females", "femaile", "femalr", "female/woman", "demigirl", "cis woman") ~ "female",
    low_gender %in% c("male", "malw", "man", " male", "male ", "man", "m", "boy", "trans-masc", "cis male") ~ "male",
    low_gender %in% c("nonbinary", "non-binary", "nb", "genderfluid") ~ "nonbinary",
    .default = as.character(low_gender)
  )
  return(return_val)
}

# I forgot to add partner information in my preprocessing script, so let's recover that...
load_participant_data <- function(data_name, study_list) {
  source_files <- list.files(
    path = here(raw_data_dir),
    recursive = TRUE,
    pattern = paste0(data_name, ".csv$"),
    full.names = TRUE
  )
  data_files <- source_files[Reduce("|", lapply(study_list, function(x) grepl(x, source_files)))]
  d_player_raw <- do.call(bind_rows, lapply(data_files, read.csv))
  d_players <- d_player_raw |>
    mutate(
      URLParams = map(urlParams, ~ possibly(function(x) {
        if (is.na(x)) {
          return(data.frame())
        }
        fromJSON(x) %>% as.data.frame()
      }, otherwise = data.frame())(.)),
      ExitSurvey = map(exitSurvey, ~ possibly(function(x) {
        if (is.na(x)) {
          return(data.frame())
        }
        fromJSON(x) %>% as.data.frame()
      }, otherwise = data.frame())(.))
    ) |>
    unnest(URLParams) %>%
    unnest(ExitSurvey) |>
    select(
      playerID = id, gender, age, education, prolificID = participantKey,
      bonus, exitStepDone, gameID, partnerID = partner
    )
  return(d_players)
}

d_game <- load_data("games", target_experiment_names)
d_round <- load_data("round", target_experiment_names)
d_chat <- load_data("chats", target_experiment_names)
d_players <- load_participant_data("player", target_experiment_names)

d_game$condition_label <- factor(d_game$contextStructure,
  levels = old_conditions,
  labels = new_conditions_long
)

d_game_final <- d_game |>
  mutate(
    dataset_id = "mankewitz2025_function",
    full_cite = "Mankewitz, J., & Hawkins, R. (2025). Function shapes form: Compositionality emerges from communicative needs, not environmental structure alone. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 47).",
    short_cite = "Mankewitz & Hawkins (2025)",
    group_size = 2,
    structure = "thick",
    language = "English",
    game_id = gameID
  )

d_messages_final <- d_chat |>
  group_by(roundID) |>
  left_join(d_round |>
    select(roundID, gameID)) |>
  mutate(
    message_number = 1:n(),
    message_irrelevant = chit_chat
  ) |>
  ungroup() |>
  mutate(
    action_type = "message",
    role = ifelse(director_msg, "describer", "matcher"),
    time_stamp = NA,
    player_id = playerID,
    choice_id = NA
  ) |>
  select(
    roundID, gameID, action_type, player_id, role, time_stamp,
    message_number, message_irrelevant, choice_id, text
  )

# add dummy messages for rounds where director didn't talk

# game director
d_director <- d_round |>
  select(gameID, director, index) |> # director appears to alternate on a trial by trial basis
  mutate(parity = index %% 2) |>
  select(gameID, derived_director = director, parity) |>
  unique() |>
  filter(derived_director != "")

d_no_talk <- d_round |>
  as_tibble() |>
  select(roundID, gameID, director, index) |>
  unique() |>
  mutate(parity = index %% 2) |>
  filter(!is.na(gameID)) |>
  filter(!is.na(roundID)) |>
  left_join(d_director) |>
  mutate(director = ifelse(director == "", derived_director, director)) |>
  anti_join(d_messages_final |> filter(role == "describer") |> select(roundID, gameID) |> unique()) |>
  mutate(
    action_type = "message",
    player_id = director,
    role = "describer",
    time_stamp = NA,
    message_number = NA,
    message_irrelevant = NA,
    choice_id = NA,
    text = NA
  )


d_actions_final <- d_round |>
  mutate(
    action_type = "selection",
    role = "matcher",
    time_stamp = as.numeric(NA),
    choice_id = ifelse(response == "", "timed_out", response),
    text = NA,
    message_number = as.numeric(NA),
    message_irrelevant = NA
  ) |>
  left_join(d_players |> select(gameID, director = playerID, player_id = partnerID)) |>
  select(
    roundID, gameID, action_type, player_id, role, time_stamp,
    message_number, message_irrelevant, choice_id, text
  )

d_actions <- bind_rows(d_messages_final, d_actions_final, d_no_talk) |> left_join(d_players |> select(gameID, age, gender, education, player_id = playerID), by = c("player_id", "gameID"))

# Round Info

d_trial_info <- d_round |>
  as_tibble() |>
  mutate(
    option_set = tangramURLs |>
      str_remove_all('\\[|\\]|"') |>
      str_replace_all(",", ";"),
    trial_num = index + 1,
    rep_num = repNum + 1,
    stage_num = 1,
    exclude = FALSE,
    exclusion_reason = as.character(NA)
  ) |>
  select(roundID, gameID, option_set, trial_num, rep_num, stage_num, exclude, exclusion_reason, target)

d_full <- d_actions |>
  left_join(d_trial_info) |>
  left_join(d_game_final) |>
  mutate(
    room_num = 1,
    age = ifelse(age != "", as.numeric(age), as.numeric(NA)),
    gender = ifelse(gender != "", gender |> as.character() |> bin_gender(), as.character(NA)),
    education = ifelse(education != "", as.character(education), as.character(NA)),
    race = as.character(NA),
    native_language = "English", # paper says recruited native English speakers from US UK Canada
    prior_relationship = "no",
    partner_constancy = "yes",
    role_constancy = "no",
    population = "adult",
    confederates = "no",
    modality = "written",
    feedback = "full", # confirmed this with Jess
    backchannel = "full",
    order_match = "match"
  ) |>
  select(
    condition_label, dataset_id, full_cite, short_cite,
    trial_num, rep_num, stage_num, room_num,
    group_size, language, prior_relationship, partner_constancy, population, role_constancy,
    confederates, modality, feedback, backchannel, order_match,
    game_id, option_set,
    target, exclude, exclusion_reason, action_type,
    player_id, age, gender, education, race, native_language,
    role, time_stamp, text, message_number, message_irrelevant, choice_id
  )


validate_dataset(d_full, write = T)
