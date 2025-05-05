library(here)
library(tidyverse)
library(jsonlite)

ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T)
}


raw_data_dir <- here("import/hawkins_respectthecode/raw_data")

messages_1 <- read_csv(here(raw_data_dir, "rounds.csv")) |> 
  filter(createdAt >= lubridate::ymd('2021-01-21')) %>%
  mutate(data.target = map(data.target, .f = ParseJSONColumn)) %>%
  unnest(data.target) |> 
  rename(room0_target = room0, room1_target = room1) %>%
  rename_with(~ gsub("data.", "", .x, fixed = TRUE)) %>%
  rename(room1_response = room1response, room0_response = room0response,
         room1_correct= room1correct, room0_correct=room0correct) |> 
  mutate(chat = map (chat, .f=ParseJSONColumn))
#TODO figure out if there are raw-er messages


rounds_1 <- read_csv(here(raw_data_dir, "rounds.csv")) |> 
  filter(createdAt >= lubridate::ymd('2021-01-21')) %>%
  mutate(data.target = map(data.target, .f = ParseJSONColumn)) %>%
  unnest(data.target) |> 
  rename(room0_target = room0, room1_target = room1) %>%
  filter(!is.na(text)) %>%
  rename_with(~ gsub("data.", "", .x, fixed = TRUE)) %>%
  rename(room1_response = room1response, room0_response = room0response,
         room1_correct= room1correct, room0_correct=room0correct) %>%
  select(-chat, -room0data, -room1data) |> 
  gather(key, value, starts_with('room')) |> 
  separate(key, into = c('roomId', 'info')) %>%
  spread(info, value) %>%
  mutate(response = ifelse(response == 'false', 'timed_out', response)) |> 
  left_join(listener_id) |> 
  rename(player_id=listener) |> 
  mutate(role="matcher") |> 
  mutate(target= gsub("/experiment/tangram_", "", target, fixed = TRUE),
         target= gsub(".png", "", target, fixed = TRUE),
         response= gsub("/experiment/tangram_", "", response, fixed = TRUE),
         response= gsub(".png", "", response, fixed = TRUE),
         choice_id=ifelse(response %in% c("false", "FALSE"), NA, response),
         action_type="selection") |> 
  select(gameId, trialNum, repNum, partnerNum, choice_id, target, role, roomId, )
  
#Note we are just dropping room ID /partnerNum info here!!!

options=c("A", "B","C","D")

#unclear what message clean up occurred will want to double check!
all <- messages_1 |> 
  mutate(role="describer",
         player_id=speaker,
         target_id=tangram) |> 
  group_by(gameId, trialNum, repNum, partnerNum, roomId) |> 
  mutate(message_num=row_number()) |> 
  ungroup() |> 
  full_join(rounds_1) |> 
    mutate(paper_id="hawkins2021_respectthecode",
           experiment_id="network",
           full_cite="Hawkins, R., Liu, I., Goldberg, A., & Griffiths, T. (2021). Respect the code: Speakers expect novel conventions to generalize within but not across social group boundaries. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 43, No. 43).",
           short_cite="Hawkins et al (2021)",
           language="English",
           
           condition_label="network"
           trial_num=1+trialNum+partnerNum*16,
           rep_num=1+repNum+4*partnerNum,
           time_to_choice=NA,
           group_size=4,
           structure="pairs-network"
           ) |> 
    rowwise() |> 
    mutate(option_set=list(options)) |> 
    ungroup() |> 
    rename(game_id=gameId, message=text) |> 
    select(paper_id, experiment_id, game_id, player_id, trial_num,rep_num,
           role, target_id, message_num, message,
           choice_id, time_to_choice, option_set,
           group_size,
           structure
    )

source(here("validate.R"))

validate_dataset(all)