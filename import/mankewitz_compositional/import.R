library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))

raw_data_dir <- "import/mankewitz_compositional/raw_data/raw_data/"
preprocessed_raw_data_dir <- "import/mankewitz_compositional/raw_data/preprocessed_data/"

target_experiment_names <- list.files(here(raw_data_dir))

old_conditions <- c('noncomp','comp-within','comp-between')
new_conditions_long <- c('No Competitor',
                         'Within Trial Competitor',
                         'Across Trial Competitor')

load_data <- function(data_name, study_list) {
  source_files <- list.files(path = here(preprocessed_raw_data_dir),
                             recursive = TRUE,
                             pattern = paste0(data_name, ".csv$"),
                             full.names = TRUE)
  # Bind the rows of only the data files in the target experiment/study list
  data_files <- source_files[Reduce("|", lapply(study_list, function(x) grepl(x, source_files)))]
  if (data_name == "round") { #the round.csv needs cleaning up
    do.call(bind_rows, lapply(data_files, function(file) {
      df <- read.csv(file) |> 
        select(roundID = id, correct, gameID, index, numTrials, repNum, 
               response, target, targetNum, trialNum, tangramURLs, director)
      df$correct <- as.logical(df$correct)  # Convert to logical
      return(df)
    }))
  } else {
    do.call(bind_rows, lapply(data_files, read.csv))
  }
}

# I forgot to add partner information in my preprocessing script, so let's recover that...
load_participant_data <- function(data_name, study_list){
  source_files <- list.files(path = here(raw_data_dir),
                             recursive = TRUE,
                             pattern = paste0(data_name, ".csv$"),
                             full.names = TRUE)
  data_files <- source_files[Reduce("|", lapply(study_list, function(x) grepl(x, source_files)))]
  d_player_raw <- do.call(bind_rows, lapply(data_files, read.csv))
  
  d_players <- d_player_raw |> 
    mutate(URLParams = map(urlParams, ~ possibly(function(x) {
      if (is.na(x)) return(data.frame())
      fromJSON(x) %>% as.data.frame()
    }, otherwise = data.frame())(.)
    )) |>
    unnest(URLParams) |>
    select(playerID = id, prolificID = participantKey,
           bonus, exitStepDone, gameID, partnerID = partner) 
  return(d_players)
}

d_game <- load_data("games", target_experiment_names)
d_round <- load_data("round", target_experiment_names)
d_chat <- load_data("chats", target_experiment_names)
d_players <- load_participant_data("player", target_experiment_names)

d_game$condition_label <- factor(d_game$contextStructure, 
                                    levels=old_conditions,
                                    labels=new_conditions_long)

d_game_final <- d_game |> 
  mutate(dataset_id = "mankewitz2025_compositional",
         full_cite = "Mankewitz & Hawkins (2025) unpublished",
         short_cite = "Mankewitz & Hawkins (2025)",
         group_size = 2,
         structure = "thick",
         language = "English",
         game_id = gameID)

d_messages_final <- d_chat |> 
  group_by(roundID) |> 
  left_join(d_round |> 
              select(roundID, gameID)) |> 
  mutate(message_number = 1:n(),
         message_irrelevant = chit_chat) |> 
  ungroup() |> 
  mutate(action_type = "message",
         role = ifelse(director_msg, "describer", "matcher"),
         time_stamp = NA,
         player_id = playerID,
         choice_id = NA) |> 
  select(roundID, gameID, action_type, player_id, role, time_stamp, 
         message_number, message_irrelevant, choice_id, text)

d_actions_final <- d_round |> 
  mutate(action_type = "selection",
         role = "matcher",
         time_stamp = as.numeric(NA),
         choice_id = ifelse(response == "", "timed_out", response), 
         text = NA, 
         message_number = as.numeric(NA), 
         message_irrelevant = NA) |> 
  left_join(d_players |> select(gameID, director=playerID, player_id=partnerID)) |> 
  select(roundID, gameID, action_type, player_id, role, time_stamp, 
         message_number, message_irrelevant, choice_id, text)

d_actions <- rbind(d_messages_final, d_actions_final)
  
# Round Info

d_trial_info <- d_round |> 
  mutate(option_set = tangramURLs |> 
                    str_remove_all('\\[|\\]|"') |> 
                    str_replace_all(',', ';'),
         trial_num = index +1,
         rep_num = repNum + 1,
         stage_num = 1,
         exclude = FALSE,
         exclusion_reason = as.character(NA))

d_full <- d_actions |> left_join(d_trial_info) |> left_join(d_game_final) |> 
  mutate(room_num=1,
         age=as.numeric(NA),
         gender=as.character(NA)) |> 
  select(condition_label, dataset_id, full_cite, short_cite,
         trial_num, rep_num, stage_num, room_num, age, gender,
         group_size, structure, language, game_id, option_set,
         target, exclude, exclusion_reason, action_type,player_id,
         role, time_stamp, text, message_number, message_irrelevant, choice_id
  )


validate_dataset(d_full, write=T)
