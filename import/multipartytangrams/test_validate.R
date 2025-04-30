library(here)
library(tidyverse)

file <- read_csv("test_boyce.csv")

# check that action_type is only message and selection (no NA)
# check that role is only matcher and describer (no NA)

# check types of columns ? 

condition <- file |> 
  select(condition_label, paper_id, full_cite, short_cite, group_size, structure, language) |>
  unique() |> 
  mutate(condition_id=row_number())
# TODO should require that condition_label is unique per row
# TODO should require that paper_id, full_cite, short_cite are consistent

describer <- file |> select(game_id, trial_num, player_id, role) |> 
  filter(role=="describer") |> 
  unique() |> 
  select(-role) |> 
  rename(describer=player_id)
# TODO require that there is one row / game_id*trial_num


matchers <- file |> select(game_id, trial_num, player_id, role) |> 
  filter(role=="matcher") |> 
  unique() |> 
  group_by(game_id, trial_num) |> 
  summarize(matchers=list(player_id))

trials <- file |> 
  select(condition_label, game_id, option_set, target, trial_num, rep_num, exclude,
         exclusion_reason) |> 
  unique() |> 
  left_join(condition |> select(condition_label, condition_id)) |> 
  left_join(describer) |> 
  left_join(matchers) |> 
  mutate(trial_id = row_number()) |> 
  select(-condition_label)
# confirm that there's one row per game_id - trial_num 

messages <- file |> filter(action_type=="message") |> 
  select(game_id, trial_num, text, player_id, role, message_number, message_irrelevant, time_stamp) |> 
  left_join(trials |> select(trial_id, game_id, trial_num)) |> 
  select(-trial_num, -game_id) 


choices <- file |> filter(action_type=="selection") |> 
  select(game_id, trial_num, choice_id, player_id, role, time_stamp) |> 
  left_join(trials |> select(trial_id, game_id, trial_num)) |> 
  select(-trial_num, -game_id)
# check that choices in option_set, or "timed-out" or NA
