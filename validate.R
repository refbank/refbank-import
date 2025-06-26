library(here)
library(tidyverse)
library(testthat)
library(assertthat)

# Table conditions{
#   condition_id integer 
#   dataset_id varchar
#   full_cite varchar
#   short_cite varchar
#   condition_label varchar
#   group_size integer
#   structure varchar
#   language varchar
#   placeholder_features varchar
# }

# Table choices {
#   id integer [primary key]
#   trial_id integer
#   choice integer
#   player_id integer
#   time_stamp time 
# }
# 
# Table messages {
#   id integer [primary key]
#   trial_id integer
#   player_id integer
#   role varchar
#   text string
#   message_number integer
#   message_irrelevant bool
#   time_stamp time
# }
# 
# Table players {
#   id integer [primary key]
#   age integer
#   gender varchar
#   placeholder_demo varchar
# }
# 
# Table trials {
#   id integer [primary key]
#   condition_id integer
#   game_id varchar
#   room_num varchar
#   option_set list
#   target integer
#   stage_num integer
#   trial_num integer
#   rep_num integer
#   exclude bool
#   exclusion_reason varchar
#   describer varchar
#   matchers list
# }

check_cols <- function(required_cols, df){
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    print("Some columns are missing from the input table:")
    print(missing_cols)
    stop()
  }
  
  excess_cols <- setdiff(colnames(df), required_cols)
  if (length(excess_cols) > 0) {
    print("Some columns are not expected:")
    print(excess_cols)
    stop()
  }
}


validate_dataset <- function(df, write=F) {
  
  # check column presence
  required_cols <- c(
    "condition_label", "dataset_id", "full_cite", "short_cite",
    "group_size", "structure", "language",
    
    "game_id", "room_num", "option_set", "target", 
    "trial_num", "rep_num", "stage_num",
    "exclude", "exclusion_reason",
    
    "action_type", "player_id", "role", "time_stamp",
    "age", "gender",
    
    "text", "message_number", "message_irrelevant",
    
    "choice_id")

  
  # check column types
  types <- map(df, class) |> enframe() |> unnest(value)
  
  should_be_char <- c("dataset_id", "full_cite", "short_cite", 
                      "structure", "language", "option_set",
                      "exclusion_reason", "action_type", "role", "text", "gender")
  should_be_numeric <- c("group_size", "room_num", "trial_num", "rep_num", "stage_num",
                         "time_stamp", "message_number", "age")
  should_be_bool <- c("exclude", "message_irrelevant")
  
  valid_types <- bind_rows(
    tibble(name = should_be_char, correct_value = "character"),
    tibble(name = should_be_numeric, correct_value = "numeric"),
    tibble(name = should_be_bool, correct_value = "logical")
  )
  
  check_cols(required_cols, df)
  
  types_check <- types |> 
    left_join(valid_types, by = join_by(name)) |> 
    filter(!is.na(correct_value),
           value != correct_value)
  
  if (nrow(types_check) > 0) {
    print("Some columns have incorrect types:")
    print(types_check)
    stop()
  }
  
  assert_that(all(df$action_type %in% c("message", "selection")), 
              msg = "action_type must be message or selection")
  assert_that(all(df$role %in% c("describer", "matcher")), 
              msg = "role must be matcher or describer")
  
  # check conditions
  try_condition <- df |>
    select(condition_label, dataset_id, full_cite, short_cite, 
           group_size, structure, language) |>
    unique()
  
  assert_that(length(unique(try_condition$condition_label)) == 
                length(try_condition$condition_label),
              msg = "condition labels must be unique")
  
  assert_that(length(unique(try_condition$dataset_id)) == 1, 
              msg = "dataset_id should be consistent")
  assert_that(length(unique(try_condition$full_cite)) == 1, 
              msg = "full_cite should be consistent")
  assert_that(length(unique(try_condition$short_cite)) == 1, 
              msg = "short_cite should be consistent")
  
  conditions <- try_condition |>
    mutate(condition_id = row_number())
  
  # create player_id -- numeric 
  try_players <- df |> select(player_id, age, gender) |> unique()
  
  assert_that(length(unique(try_players$player_id)) ==
                length(try_players$player_id),
              msg="player_id should be unique")
  
  players <- try_players |> 
    mutate(player_id_numeric = row_number())
  
  # check players
  try_describer <- df |> select(game_id, room_num, trial_num, player_id, role) |>
    filter(role == "describer") |>
    unique() |>
    select(-role)
  
  assert_that(try_describer |> select(game_id, room_num, trial_num) |> unique() |> nrow() == 
                try_describer |> nrow(),
              msg = "describer should be unique per game-room-trial")
  
  describer <- try_describer |> left_join(players) |> 
    rename(describer = player_id_numeric) |> 
    select(game_id, room_num, trial_num, describer)
  
  matchers <- df|> select(game_id, room_num, trial_num, player_id, role) |>
    filter(role == "matcher") |>
    unique() |>
    left_join(players) |> 
    group_by(game_id, room_num, trial_num) |>
    summarize(matchers = str_c(player_id_numeric, collapse=";"), .groups = "drop")
  
  # check trials
  try_trials <- df |>
    select(condition_label, game_id, room_num, option_set, target, stage_num, trial_num, rep_num, exclude,
           exclusion_reason) |>
    unique() |>
    left_join(conditions |> select(condition_label, condition_id),
              by = join_by(condition_label)) |>
    left_join(describer, by = join_by(game_id, room_num, trial_num)) |>
    left_join(matchers, by = join_by(game_id, room_num, trial_num))
  
  assert_that(try_trials |> select(game_id, room_num, trial_num) |> unique() |> nrow() == 
                try_trials |> nrow(),
              msg = "trials should be uniquely determined by game_id, room_num and trial_num")
 
  # create game_id -- numeric
  
  games <-try_trials |> select(game_id) |> unique() |> 
    mutate(game_id_numeric = row_number())
  
  
  trials <- try_trials |>
    mutate(trial_id = row_number()) |> 
    left_join(games) |> 
    select(-condition_label)
  

  # check messages
  messages <- df|> filter(action_type == "message") |>
    left_join(players) |> 
    select(game_id, room_num, trial_num, text, player_id=player_id_numeric, role, 
           message_number, message_irrelevant, time_stamp) |>
    left_join(trials |> select(trial_id, game_id, room_num, trial_num), 
              by = join_by(game_id, room_num, trial_num)) |>
    mutate(text=str_trim(text)) |>
    filter(!is.na(text)) |> 
    filter(text!="") |> 
    select(-trial_num, -game_id, -room_num)
  
  # check selections
  try_choices <- df |> filter(action_type == "selection") |>
    left_join(players) |> 
    select(game_id, room_num, trial_num, choice_id, player_id=player_id_numeric, time_stamp) |>
    left_join(trials |> select(trial_id, game_id, room_num, trial_num, option_set),
              by = join_by(game_id, room_num, trial_num)) |>
    mutate(option_set_list = str_split(option_set, ";")) |>
    select(-trial_num, -room_num, -game_id) |>
    mutate(check_choices = map2_lgl(choice_id, option_set_list, 
                                    \(c, o) {c %in% o | c == "timed_out" }))
  
  assert_that(all(try_choices$check_choices), 
              msg = "choice_id must be in option_set or timed_out")
  
  choices <- try_choices |> select(-option_set_list, -check_choices, -option_set)
  
  # remove excess columns
  
  players <- players |> select(-player_id) |> rename(player_id=player_id_numeric)
  conditions <- conditions |> select(-condition_label)
  trials <- trials |> select(-game_id) |> rename(game_id=game_id_numeric)
  
  print("Checking players")
  check_cols(c("player_id", "age", "gender"), players)
  na_players <- players |> filter(if_any(c("player_id"), is.na))
  assert_that(nrow(na_players)==0)
  
  print("Checking conditions")
  check_cols(c("condition_id", "dataset_id", "full_cite", "short_cite", "group_size", "structure", "language"), conditions)
  na_conditions <- conditions |> filter(if_any(c("dataset_id","short_cite", "full_cite", "condition_id", "group_size", "structure", "language"), is.na))
  assert_that(nrow(na_conditions)==0)
  
  print("Checking choices")
  check_cols(c("trial_id", "choice_id", "player_id", "time_stamp"), choices)
  na_choices <- choices |> filter(if_any(c("trial_id", "choice_id", "player_id"), is.na))
  assert_that(nrow(na_choices)==0)
  
  print("Checking messages")
  check_cols(c("trial_id", "player_id", "role", "text", "message_number", "message_irrelevant", "time_stamp"), messages)
  na_messages <- messages |> filter(if_any(c("player_id", "trial_id", "role", "message_number", "text"), is.na))
  assert_that(nrow(na_messages)==0)
  empty_message <- messages |> filter(text=="")
  assert_that(nrow(empty_message)==0)
  
  print("Checking trials")
  check_cols(c("trial_id", "condition_id", "game_id", "room_num", "option_set", "target", "stage_num",
               "trial_num", "rep_num", "exclude", "exclusion_reason", "describer", "matchers"), trials)
  na_trials <- trials |> filter(if_any(c("condition_id", "game_id", "room_num", "option_set", "target", "stage_num", "trial_num", "rep_num", "describer"), is.na))
#View(na_trials)
    assert_that(nrow(na_trials)==0)
  
  print("All checks pass!")
  
  
  if (write) {
    dataset <- df$dataset_id[1]
    dir <- str_c("harmonized_data/",dataset)
    dir.create(here(dir), showWarnings = FALSE)
    print("Writing conditions")
    write_csv(conditions, here(dir,"conditions.csv"))
    print("Writing trials")
    write_csv(trials, here(dir, "trials.csv"))
    print("Writing messages")
    write_csv(messages, here(dir, "messages.csv"))
    print("Writing choices")
    write_csv(choices, here(dir, "choices.csv"))
    print("Writing players")
    write_csv(players, here(dir, "players.csv"))
         
  }
}

