library(here)
library(tidyverse)
library(testthat)
library(assertthat)

validate_dataset <- function(df, write=F) {
  
  # check column presence
  required_cols <- c(
    "condition_label", "paper_id", "full_cite", "short_cite",
    "group_size", "structure", "language", "game_id",
    "option_set", "target", "trial_num", "rep_num",
    "exclude", "exclusion_reason",
    "action_type", "player_id", "role", "time_stamp",
    "text", "message_number", "message_irrelevant", "choice_id")
  
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
  
  # check column types
  types <- map(df, class) |> enframe() |> unnest(value)
  
  should_be_char <- c("paper_id", "full_cite", "short_cite", 
                      "structure", "language", "option_set",
                      "exclusion_reason", "action_type", "role", "text")
  should_be_numeric <- c("group_size", "trial_num", "rep_num", 
                         "time_stamp", "message_number")
  should_be_bool <- c("exclude", "message_irrelevant")
  
  valid_types <- bind_rows(
    tibble(name = should_be_char, correct_value = "character"),
    tibble(name = should_be_numeric, correct_value = "numeric"),
    tibble(name = should_be_bool, correct_value = "logical")
  )
  
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
    select(condition_label, paper_id, full_cite, short_cite, 
           group_size, structure, language) |>
    unique()
  
  assert_that(length(unique(try_condition$condition_label)) == 
                length(try_condition$condition_label),
              msg = "condition labels must be unique")
  
  assert_that(length(unique(try_condition$paper_id)) == 1, 
              msg = "paper_id should be consistent")
  assert_that(length(unique(try_condition$full_cite)) == 1, 
              msg = "full_cite should be consistent")
  assert_that(length(unique(try_condition$short_cite)) == 1, 
              msg = "short_cite should be consistent")
  
  condition <- try_condition |>
    mutate(condition_id = row_number())
  
  # create player_id -- numeric 
  players <- df |> select(player_id) |> unique() |> 
    mutate(player_id_numeric = row_number())
  
  # check players
  try_describer <- df |> select(game_id, trial_num, player_id, role) |>
    filter(role == "describer") |>
    unique() |>
    select(-role)
  
  assert_that(try_describer |> select(game_id, trial_num) |> unique() |> nrow() == 
                try_describer |> nrow(),
              msg = "describer should be unique per game-trial")
  
  describer <- try_describer |> left_join(players) |> 
    rename(describer = player_id_numeric)
  
  matchers <- df|> select(game_id, trial_num, player_id, role) |>
    filter(role == "matcher") |>
    unique() |>
    left_join(players) |> 
    group_by(game_id, trial_num) |>
    summarize(matchers = list(player_id_numeric), .groups = "drop")
  
  # check trials
  try_trials <- df |>
    select(condition_label, game_id, option_set, target, trial_num, rep_num, exclude,
           exclusion_reason) |>
    unique() |>
    left_join(condition |> select(condition_label, condition_id),
              by = join_by(condition_label)) |>
    left_join(describer, by = join_by(game_id, trial_num)) |>
    left_join(matchers, by = join_by(game_id, trial_num))
  
  assert_that(try_trials |> select(game_id, trial_num) |> unique() |> nrow() == 
                try_trials |> nrow(),
              msg = "trials should be uniquely determined by game_id and trial_num")
 
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
    select(game_id, trial_num, text, player_id=player_id_numeric, role, 
           message_number, message_irrelevant, time_stamp) |>
    left_join(trials |> select(trial_id, game_id, trial_num), 
              by = join_by(game_id, trial_num)) |>
    select(-trial_num, -game_id)
  
  # check selections
  try_choices <- df |> filter(action_type == "selection") |>
    left_join(players) |> 
    select(game_id, trial_num, choice_id, player_id=player_id_numeric, role, time_stamp) |>
    left_join(trials |> select(trial_id, game_id, trial_num, option_set),
              by = join_by(game_id, trial_num)) |>
    mutate(option_set_list = str_split(option_set, ";")) |>
    select(-trial_num, -game_id) |>
    mutate(check_choices = map2_lgl(choice_id, option_set_list, 
                                    \(c, o) {c %in% o | c == "timed_out" | is.na(c)}))
  
  assert_that(all(try_choices$check_choices), 
              msg = "choice_id must be in option_set or timed_out or NA")
  
  choices <- try_choices |> select(-option_set_list)
  
  print("All checks pass!")
  
  if (write) {
    dataset <- df$paper_id[1]
    dir <- str_c("harmonized_data/",dataset)
    dir.create(here(dir), showWarnings = FALSE)
    print("Writing conditions")
    write_csv(condition, here(dir,"conditions.csv"))
    print("Writing trials")
    write_csv(trials, here(dir, "trials.csv"))
    print("Writing messages")
    write_csv(messages, here(dir, "messages.csv"))
    print("Writing choices")
    write_csv(choices, here(dir, "choices.csv"))
         
  }
}

