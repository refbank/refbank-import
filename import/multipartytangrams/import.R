library(here)
library(tidyverse)
library(jsonlite)

###### read in data from github
url <- "https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/"
one_chat <- read_csv(str_c(url, "data/study1/filtered_chat.csv")) |> mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_chat <- read_csv(str_c(url, "data/study2a/filtered_chat.csv")) |> mutate(rotate = "no_rotate")
two_b_chat <- read_csv(str_c(url, "data/study2b/filtered_chat.csv")) |>
  mutate(rotate = "full_feedback") |>
  select(-`row num`)
two_c_chat <- read_csv(str_c(url, "data/study2c/filtered_chat.csv")) |>
  mutate(rotate = "emoji") |>
  select(-type)
three_chat <- read_csv(str_c(url, "data/study3/filtered_chat.csv")) |>
  inner_join(read_rds(str_c(url, "data/study3/round_results.rds")) |> select(gameId, trialNum, condition = name) |> unique()) |>
  select(-rowid, -type)


one_round_results <- read_rds(str_c(url, "data/study1/round_results.rds")) %>% mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_round_results <- read_rds(str_c(url, "data/study2a/round_results.rds")) %>% mutate(rotate = "no_rotate")
two_b_round_results <- read_rds(str_c(url, "data/study2b/round_results.rds")) %>% mutate(rotate = "full_feedback")
two_c_round_results <- read_rds(str_c(url, "data/study2c/round_results.rds")) |> mutate(rotate = "emoji")
three_round_results <- read_rds(str_c(url, "data/study3/round_results.rds")) |> rename(`_id` = "X_id", condition = name)


one_round_include <- read_rds(str_c(url, "data/study1/rounds_include.rds")) %>% mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_round_include <- read_rds(str_c(url, "data/study2a/rounds_include.rds")) %>% mutate(rotate = "no_rotate")
two_b_round_include <- read_rds(str_c(url, "data/study2b/rounds_include.rds")) %>% mutate(rotate = "full_feedback")
two_c_round_include <- read_rds(str_c(url, "data/study2c/rounds_include.rds")) |> mutate(rotate = "emoji")
three_round_include <- read_rds(str_c(url, "data/study3/rounds_include.rds")) |> rename(condition = name)

all_include <- one_round_include |> 
  rbind(two_a_round_include) |> 
  rbind(two_b_round_include) |> 
  rbind(two_c_round_include) |> 
  rbind(three_round_include) |> 
  mutate(include=T)


options=c("A", "B","C","D","E","F","G","H","I", "J", "K", "L") 
##### do message processing

combined_chat <- one_chat |>
  rbind(two_a_chat) |>
  rbind(two_b_chat) |>
  rbind(two_c_chat) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  rbind(three_chat) |> 
  mutate(
    text = ifelse(is.na(spellchecked), text, spellchecked),
    text = gsub("\\n", "", fixed = T, text), # note that this is using spellcorrected version!!!!
    text = str_squish(text),
    tangram = gsub("/experiment/tangram_", "", target, fixed = TRUE),
    tangram = gsub(".png", "", tangram, fixed = TRUE)
  ) %>%
  select(gameId, trialNum, repNum, tangram, playerId, role, numPlayers, text, condition, is.chitchat) |> 
  group_by(gameId, trialNum, repNum) |> 
  mutate(message_number=row_number()) |> 
  ungroup() |> 
  mutate(action_type="message") |> 
  mutate(role=case_when(role=="speaker" ~ "describer",
                        role=="listener" ~ "matcher")) |> 
  filter(!is.na(text))

##### do result processing

combined_results <- one_round_results |>
  rbind(two_a_round_results) |>
  rbind(two_b_round_results) |>
  rbind(two_c_round_results) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  rbind(three_round_results) |> 
  mutate(choice_id= gsub("/experiment/tangram_", "", response, fixed = TRUE),
         choice_id= gsub(".png", "", choice_id, fixed = TRUE))

##### determine when people weren't actually there

last_present <- combined_results |> select(gameId, playerId, trialNum, choice_id) |> 
  filter(choice_id %in% options) |> 
  group_by(gameId, playerId) |> 
  summarize(lasttrialNum=max(trialNum))

choices <- combined_results |>
  left_join(last_present) |> 
  mutate(choice_id = case_when(
    choice_id %in% options ~ choice_id, # if there's a choice, keep it
    is.na(lasttrialNum) ~ NA, # if there's never a choice, NA
    trialNum<lasttrialNum+1 ~ NA, # if it's more than one after the last choice, NA
    T ~ "timed_out"), # otherwise, it's a timed_out
    time_stamp=case_when(
      choice_id %in% options ~ time,
      choice_id=="timed_out" ~ 180, # known max time for trial
      T ~ NA
    ),
    action_type="selection", 
    role="matcher")

#### exclusions


all <- choices |> bind_rows(combined_chat) |> 
  left_join(all_include) |> 
  mutate(paper_id="boyce2024_interaction",
         trial_num=trialNum+1,
         rep_num=repNum+1,
         full_cite="Boyce, V., Hawkins, R. D., Goodman, N. D., & Frank, M. C. (2024). Interaction structure constrains the emergence of conventions in group communication. Proceedings of the National Academy of Sciences, 121(28), e2403888121.",
         short_cite="Boyce et al. (2024)",
         group_size=case_when(
           condition %in% c( "emoji", "full_feedback", "no_rotate") ~ 6,
           T ~ str_sub(condition, 1,1) |> as.numeric()
         ), # note this matches condition, not actual player count necessarily
         structure=case_when(
           str_detect(condition, "thin") ~ "thin",
           str_detect(condition, "thick") ~ "thick",
           condition=="emoji" ~ "thin",
           str_detect(condition, "rotate") ~ "medium",
           condition =="full_feedback" ~ "med_thick",
           condition =="no_rotate" ~ "med_thick"
         ),
         language="English",
         exclude=ifelse(is.na(include),T, NA),
         exclusion_reason=ifelse(exclude, "incomplete block", NA),
         message_irrelevant=(is.chitchat==1),
  ) |> 
  rowwise() |> 
  mutate(option_set= options |> str_c(collapse=";") ) |> 
  ungroup() |> 
  select(condition_label=condition,
         paper_id,
         full_cite, 
         short_cite,
         group_size,
         structure,
         language,
         game_id=gameId,
         option_set,
         target=tangram,
         trial_num,
         rep_num,
         exclude,
         exclusion_reason,
         action_type,
         player_id=playerId,
         role,
         time_stamp,
         text,
         message_number,
         message_irrelevant,
         choice_id
) |> arrange(game_id) |>  write_csv("test_boyce.csv", quote="needed")

#read_csv("test_boyce.csv") |> View()

