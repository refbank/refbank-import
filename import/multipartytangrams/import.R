library(here)
library(tidyverse)


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

combined_chat <- one_chat |>
  rbind(two_a_chat) |>
  rbind(two_b_chat) |>
  rbind(two_c_chat) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  rbind(three_chat) |>
  filter(!(is.chitchat)) |>
  mutate(
    text = gsub("\\n", "", fixed = T, spellchecked), # note that this is using spellcorrected version!!!!
    text = gsub("[/?/.]", " ", text),
    text = str_squish(text),
    tangram = gsub("/experiment/tangram_", "", target, fixed = TRUE),
    tangram = gsub(".png", "", tangram, fixed = TRUE)
  ) %>%
  select(gameId, trialNum, repNum, tangram, playerId, role, numPlayers, text, condition)
# so here we instead count non-white space chunks for words

one_round_results <- read_rds(str_c(url, "data/study1/round_results.rds")) %>% mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_round_results <- read_rds(str_c(url, "data/study2a/round_results.rds")) %>% mutate(rotate = "no_rotate")
two_b_round_results <- read_rds(str_c(url, "data/study2b/round_results.rds")) %>% mutate(rotate = "full_feedback")
two_c_round_results <- read_rds(str_c(url, "data/study2c/round_results.rds")) |> mutate(rotate = "emoji")
three_round_results <- read_rds(str_c(url, "data/study3/round_results.rds")) |> rename(`_id` = "X_id", condition = name)

combined_results <- one_round_results |>
  rbind(two_a_round_results) |>
  rbind(two_b_round_results) |>
  rbind(two_c_round_results) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  rbind(three_round_results) |>
  mutate(choice_id= gsub("/experiment/tangram_", "", response, fixed = TRUE),
         choice_id= gsub(".png", "", choice_id, fixed = TRUE),
         choice_id=ifelse(choice_id %in% c("false", "FALSE"), NA, choice_id)) |> 
  select(realCorrect, gameId,tangram, targetNum, repNum, trialNum, condition, numPlayers, activePlayerCount, choice_id, playerId, time_to_choice=time) |>
  unique() |> mutate(role="matcher")

options=c("A", "B","C","D","E","F","G","H","I", "J", "K", "L")

all <- combined_chat |>
  group_by(gameId, trialNum, repNum) |> 
  mutate(message_num=row_number()) |> 
  ungroup() |> 
  full_join(combined_results) |> 
  mutate(paper_id="boyce2024_interaction",
         experiment_id=condition,
         trial_num=trialNum+1,
         rep_num=repNum+1,
         role=case_when(
           role=="speaker" ~ "describer",
           role=="listener" ~ "matcher",
           T ~ role
         ),
         target_id=tangram,
         structure=case_when(
           str_detect(condition, "thin") ~ "thin",
           str_detect(condition, "thick") ~ "thick",
           condition=="emoji" ~ "thin",
           str_detect(condition, "rotate") ~ "medium",
           condition =="full_feedback" ~ "med_thick",
           condition =="no_rotate" ~ "med_thick"
      ), group_size=ifelse(!is.na(activePlayerCount), activePlayerCount, numPlayers)) |> 
  rowwise() |> 
  mutate(option_set=list(options))|> 
  ungroup() |> 
  rename(game_id=gameId, player_id=playerId, message=text) |> 
  filter(!is.na(game_id)) |> 
  select(paper_id, experiment_id, game_id, player_id, trial_num, rep_num,
         role, target_id, message_num, message, 
         choice_id, time_to_choice,          option_set,
         group_size,
         structure
         )

all |> write_csv(here("harmonized_data/mpt.csv"))
