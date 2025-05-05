library(here)
library(tidyverse)
library(jsonlite)

# import decisions:
# we are treating game-room as game because that's what is unique / game-trialnum

# note that for player-role, original code implies that games.csv is canonical
# the raw-chat has a few errors!
# we also have to retrieve who the listener is in each room-game to get the right player labels on the selections


ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T)
}


raw_data_dir <- here("import/hawkins_respectthecode/raw_data")

games  <- read_csv(here(raw_data_dir, "games.csv")) %>%
  filter(createdAt >= lubridate::ymd('2021-01-21')) %>%
  rename(gameId = `_id`) 

roles.tmp <- games |> 
  select(gameId, data.roleList) %>%
  mutate(data.roleList = map(data.roleList, .f = ParseJSONColumn)) |> 
  unnest(data.roleList) |> 
  gather(playerId, role, -gameId) %>%
  rowwise() %>%
  filter(!is.null(role)) %>%
  unnest(role) %>%
  group_by(playerId) %>%
  mutate(n = row_number() - 1,
         trialNum = n %% 16,
         partnerNum = floor(n / 16),
         repNum = floor(trialNum/4) %% 4) %>%
  select(-n)

#roles.tmp |> group_by(playerId) |> summarize(n=n()) |> filter(n!=48)

rooms.tmp <- games |>   filter(createdAt >= lubridate::ymd('2021-01-21')) %>%
  select(gameId, data.schedule) %>%
  mutate(data.schedule = map(data.schedule, .f = ParseJSONColumn)) |> 
  unnest(data.schedule) |> 
  gather(playerId, room, -gameId) %>%
  rowwise() %>%
  filter(!is.null(room)) %>%
  unnest(room) %>%
  group_by(playerId) %>%
  mutate(n = row_number() - 1,
         trialNum = n %% 16,
         partnerNum = floor(n / 16),
         repNum = floor(trialNum/4) %% 4) %>%
  select(-n)

#rooms.tmp |> group_by(playerId) |> summarize(n=n()) |> filter(n!=48)

rooms_roles <- roles.tmp |> left_join(rooms.tmp)

messages_1 <- read_csv(here(raw_data_dir, "rounds.csv")) |> 
  filter(createdAt >= lubridate::ymd('2021-01-21')) %>%
  mutate(data.target = map(data.target, .f = ParseJSONColumn)) %>%
  unnest(data.target) |> 
  rename(room0_target = room0, room1_target = room1) %>%
  rename_with(~ gsub("data.", "", .x, fixed = TRUE)) %>%
  rename(room1_response = room1response, room0_response = room0response,
         room1_correct= room1correct, room0_correct=room0correct) |> 
  mutate(chat = ifelse(is.na(chat), "{}", chat)) |> 
  mutate(chat = map (chat, .f=ParseJSONColumn)) |> 
  unnest(chat)|> 
  filter(!is.na(text)) |> 
  select(gameId, trialNum, partnerNum, repNum, text, playerId, roomId, target, role) |> 
  group_by(gameId, trialNum, repNum, partnerNum, roomId) |> 
  mutate(message_number=row_number()) |> 
  ungroup() |>   mutate(action_type="message", 
                        message_number=as.numeric(message_number),
                        message_irrelevant=F) #TODO figure out message relevancy

# some player Ids seem to have both roles in the same trial? 
messages_1 |> select(gameId, trialNum, partnerNum, repNum, playerId, roomId, target, role) |> unique() |> 
  group_by(gameId, playerId, trialNum, partnerNum, repNum) |> 
  mutate(n=n()) |> 
  filter(n>1) |> View()

rounds_1 <- read_csv(here(raw_data_dir, "rounds.csv")) |> 
  filter(createdAt >= lubridate::ymd('2021-01-21'))
  mutate(data.target = map(data.target, .f = ParseJSONColumn)) %>%
  unnest(data.target) |> 
  rename(room0_target = room0, room1_target = room1) %>%
  rename_with(~ gsub("data.", "", .x, fixed = TRUE)) %>%
  rename(room1_response = room1response, room0_response = room0response,
         room1_correct= room1correct, room0_correct=room0correct) %>%
  select(-chat, -room0data, -room1data) |> 
  gather(key, value, starts_with('room')) |> 
  separate(key, into = c('roomId', 'info')) %>%
  spread(info, value) |> 
  mutate(response = ifelse(response == 'false', 'timed_out', response)) |> 
  mutate(role="listener") |> 
  mutate(target= gsub("/experiment/tangram_", "", target, fixed = TRUE),
         target= gsub(".png", "", target, fixed = TRUE),
         response= gsub("/experiment/tangram_", "", response, fixed = TRUE),
         choice_id= gsub(".png", "", response, fixed = TRUE),
         action_type="selection") |> 
  select(gameId, trialNum, repNum, partnerNum, choice_id, target, role, playerId, roomId) |> 
  mutate(action_type="selection")
  
options=c("A", "B","C","D")

all <- messages_1 |> 
  bind_rows(rounds_1)  |> 
    mutate(game_id=str_c(gameId,"_",roomId),
           role=case_when(
             role=="speaker" ~ "describer",
             role=="listener" ~ "matcher",
             T ~ NA
           ),
           paper_id="hawkins2021_respect",
           full_cite="Hawkins, R., Liu, I., Goldberg, A., & Griffiths, T. (2021). Respect the code: Speakers expect novel conventions to generalize within but not across social group boundaries. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 43, No. 43).",
           short_cite="Hawkins et al (2021)",
           language="English",
           condition_label="network",
           trial_num=1+trialNum+partnerNum*16,
           rep_num=1+repNum+4*partnerNum,
           time_stamp=as.numeric(NA),
           group_size=4,
           structure="pairs-network",
           condition_label="pairs-network",
           exclude=NA, #TODO,
           exclusion_reason=as.character(NA) #TODO
           ) |> 
    rowwise() |> 
    mutate(option_set=str_c(options, collapse=";")) |> 
    ungroup() |> 
    select(paper_id, full_cite, short_cite, language,
           condition_label, time_stamp,
           game_id, player_id=playerId,
           trial_num,rep_num,
           role, target, 
           action_type, exclude, exclusion_reason, 
           message_number, text,
           choice_id, option_set,
           group_size, message_irrelevant,
           structure
    )


dupes <- all |> select(game_id, player_id, trial_num, role) |> filter(role=="describer") |> 
  unique() |> 
  group_by(game_id, trial_num) |> 
  summarize(n=n()) |> 
  filter(n>1)




source(here("validate.R"))

validate_dataset(all)
