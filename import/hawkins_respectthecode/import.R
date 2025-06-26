library(here)
library(tidyverse)
library(jsonlite)

# import decisions:

# note that there is also post-test data associated with this dataset, but not imported

# we are treating game-room as game because that's what is unique / game-trialnum

# note that for player-role, original code implies that games.csv is canonical
# the raw-chat has a few errors! 
# we assume errors are limited to chat metadata -- it appears to be at the ends of things,
# so perhaps is getting meta-data from the next trial? 
# we assume chat messages are correctly assigned to trial and that playerid is correct which seems to be supported by spot checks
# we also have to retrieve who the listener is in each room-game to get the right player labels on the selections


ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T)
}


raw_data_dir <- here("import/hawkins_respectthecode/raw_data")

#### get canonical role and room assignments for games/players
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

rooms.tmp <- games |>   filter(createdAt >= lubridate::ymd('2021-01-21')) %>%
  select(gameId, data.schedule, data.targetSet) %>%
  mutate(data.schedule = map(data.schedule, .f = ParseJSONColumn)) |> 
  unnest(data.schedule) |> 
  gather(playerId, room, -gameId, -data.targetSet) %>%
  rowwise() %>%
  filter(!is.null(room)) %>%
  unnest(room) %>%
  #mutate(option_set=ifelse(data.targetSet=="setA", "A;B;C;D", "E;F;G;H")) |> 
  group_by(playerId) %>%
  mutate(n = row_number() - 1,
         trialNum = n %% 16,
         partnerNum = floor(n / 16),
         repNum = floor(trialNum/4) %% 4) %>%
  select(-n)

rooms_roles <- roles.tmp |> left_join(rooms.tmp) |> rename(gamerole=role, gameroom=room)



#### get message info 
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
  mutate(target= gsub("/experiment/tangram_", "", target, fixed = TRUE),
         target= gsub(".png", "", target, fixed = TRUE),) |> 
  select(gameId, trialNum, partnerNum, repNum, text, playerId, roomId, message_target=target, message_role=role) |> 
  group_by(gameId, trialNum, repNum, partnerNum, roomId) |> 
  mutate(message_number=row_number()) |> 
  ungroup() |>   mutate(action_type="message", 
                        message_number=as.numeric(message_number),
                        message_irrelevant=F) #I don't see any message relevancy cleaning in the source

# fix cases where someone seems to be in the wrong room
fixing_rooms  <- messages_1 |> left_join(rooms_roles) |> select(playerId, roomId, gameroom, partnerNum, gameId, trialNum) |> 
  unique() |> 
  group_by(roomId, gameroom, partnerNum, gameId) |> summarize(n=n()) |> 
  pivot_wider(names_from=roomId, values_from=n, values_fill = 0) |> 
  # there are 7 cases with an issue, and it's always only 1 v 12+
  # so we are going to assume that the majority ones are correct!
  mutate(roomId=ifelse(room1>room0, "room1", "room0")) |> 
  ungroup() |> 
  select(-room0, -room1) |> left_join(rooms_roles)


# read in rounds and use room0/1 to determine playerId
rounds <- read_csv(here(raw_data_dir, "rounds.csv")) |> 
  filter(createdAt >= lubridate::ymd('2021-01-21')) |> 
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
  mutate(gamerole="listener") |> 
    left_join(fixing_rooms) |> 
  mutate(target= gsub("/experiment/tangram_", "", target, fixed = TRUE),
         target= gsub(".png", "", target, fixed = TRUE),
         response= gsub("/experiment/tangram_", "", response, fixed = TRUE),
         choice_id= gsub(".png", "", response, fixed = TRUE),
         action_type="selection") |> 
  select(gameId, trialNum, repNum, partnerNum, choice_id, target, gamerole, playerId, roomId) |> 
  filter(!is.na(choice_id)) |> 
  mutate(action_type="selection")


# get the target for each 
targets <- rounds |> select(gameId, trialNum, repNum, partnerNum, roomId, target) |> unique()

messages_2 <- messages_1 |> rename(message_roomId=roomId) |> left_join(fixing_rooms) |> 
  left_join(targets)

messages_2 |> filter(roomId!=message_roomId) # 8 instances all trialNum15
messages_2 |> filter(target!=message_target) # 6 instances all trialNum15
messages_2 |> filter(gamerole!=message_role) # |> select(trialNum) |> unique() # 42 instances, all trial num 7/11/3/15

messages <- messages_2 |> 
  select(gameId, trialNum, partnerNum, repNum, text, playerId, 
                                 message_number, action_type, message_irrelevant, roomId, gamerole, target)


# according to paper, after excluding incomplete, 33 groups, 132 participants
# this appears to be the exclusion used, but this keeps 37 (which does line up with the number of games in cleaned_messages)
# I can't identify other exclusion used
talked <- messages %>%
  filter(!is.na(text)) %>%
  group_by(gameId, trialNum, roomId, partnerNum) %>%
  tally() %>%
  group_by(gameId) %>%
  tally() |> 
  filter(n >= 90) %>%
  select(gameId) |> 
  mutate(exclude=F,
         exclusion_reason=NA)


all <- messages |> 
  bind_rows(rounds)  |> 
  left_join(talked) |> 
    mutate(game_id=gameId,
           room_num=1+ (str_sub(roomId, -1, -1) |> as.numeric()) ,
           role=case_when(
             gamerole=="speaker" ~ "describer",
             gamerole=="listener" ~ "matcher",
             T ~ NA
           ),
           age=as.numeric(NA), #TODO demographics
           gender=as.character(NA),
           dataset_id="hawkins2021_respect",
           full_cite="Hawkins, R., Liu, I., Goldberg, A., & Griffiths, T. (2021). Respect the code: Speakers expect novel conventions to generalize within but not across social group boundaries. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 43, No. 43).",
           short_cite="Hawkins et al (2021)",
           language="English",
           condition_label="network",
           stage_num=partnerNum+1,
           trial_num=1+trialNum+partnerNum*16,
           rep_num=1+repNum+4*partnerNum,
           time_stamp=ifelse(choice_id=="timed_out", 45, NA), # didn't find timestamps in source, paper reports time out at 45 seconds/trial 
           group_size=4,
           structure="network-swap",
           condition_label="pairs-network",
           exclude=ifelse(is.na(exclude), T, exclude),
           exclusion_reason=ifelse(exclude, "incomplete game", NA), 
           option_set = case_when(
             target %in% c("A", "B", "C", "D") ~ "A;B;C;D",
             target %in% c("E", "F", "G", "H") ~ "E;F;G;H",
             T ~ NA
           )
           ) |> 
  filter(!is.na(target)) |> 
    select(dataset_id, full_cite, short_cite, language, stage_num, 
           condition_label, time_stamp,
           game_id, room_num, player_id=playerId,
           age, gender,
           trial_num,rep_num,
           role, target, 
           action_type, exclude, exclusion_reason, 
           message_number, text,
           choice_id, option_set,
           group_size, message_irrelevant,
           structure
    )


message <- all |> filter(action_type=="message") |> filter(role=="describer") |> select(game_id, room_num, trial_num) |> unique()
choice <- all |> filter(action_type=="selection") |> select(game_id, room_num, trial_num) |> unique()

choice |> anti_join(message)
source(here("validate.R"))

validate_dataset(all, write=T)

