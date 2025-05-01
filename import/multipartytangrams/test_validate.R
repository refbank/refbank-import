library(here)
library(tidyverse)
library(testthat)
library(assertthat)

validate_dataset <- function(file){

required_cols <- c("condition_label", "paper_id", "full_cite", "short_cite",
                   "group_size", "structure", "language", "game_id",
                   "option_set", "target", "trial_num", "rep_num",
                   "exclude", "exclusion_reason", 
                   "action_type", "player_id", "role", "time_stamp",
                   "text", "message_number", "message_irrelevant", "choice_id")

missing_cols <- setdiff(required_cols, colnames(file))
if (length(missing_cols) > 0) {
  print("Some columns are missing from the input table:")
  print(missing_cols)
  stop()
}

excess_cols <- setdiff(colnames(file), required_cols)
if (length(excess_cols) > 0) {
  print("Some columns are not expected:")
  print(excess_cols)
  stop()
}

types <- map(file, class) |> enframe() |> unnest(value)

should_be_char <- c("paper_id", "full_cite", "short_cite", "structure", "language", "option_set",
                    "exclusion_reason", "action_type", "role", "text")
should_be_numeric <- c("group_size", "trial_num", "rep_num", "time_stamp", "message_number")
should_be_bool <- c("exclude", "message_irrelevant")

characters <- types |> filter(value=="character") |> pull(name)
missing_char <- setdiff(should_be_char, characters)
if (length(missing_char)>0){
  print("Some columns should be class character:")
  print(missing_char)
  stop()
}

numerics <- types |> filter(value=="numeric") |> pull(name)
missing_num <- setdiff(should_be_numeric, numerics)
if (length(missing_num)>0){
  print("Some columns should be class numeric:")
  print(missing_num)
  stop()
}

bools <- types |> filter(value=="logical") |> pull(name)
missing_bools <- setdiff(should_be_bool, bools)
if (length(missing_bools)>0){
  print("Some columns should be class logical:")
  print(missing_bools)
  stop()
}

assert_that(all(file$action_type %in% c("message", "selection")), msg="action_type must be message or selection")

assert_that(all(file$role %in% c("describer", "matcher")), msg="role must be matcher or describer")

  
try_condition <- file |> 
  select(condition_label, paper_id, full_cite, short_cite, group_size, structure, language) |>
  unique()

assert_that(length(unique(try_condition$condition_label))==length(try_condition$condition_label),
msg="condition labels must be unique")

assert_that(length(unique(try_condition$paper_id))==1, msg="paper_id should be consistent")
assert_that(length(unique(try_condition$full_cite))==1, msg="full_cite should be consistent")
assert_that(length(unique(try_condition$short_cite))==1, msg="short_cite should be consistent")

                        
condition <- try_condition |> 
  mutate(condition_id=row_number())

try_describer <- file |> select(game_id, trial_num, player_id, role) |> 
  filter(role=="describer") |> 
  unique() |> 
  select(-role)

assert_that(try_describer |> select(game_id, trial_num) |> unique() |> nrow() == try_describer |> nrow(),
            msg="describer should be unique per game-trial")
  
describer <- try_describer |> 
  rename(describer=player_id)


matchers <- file |> select(game_id, trial_num, player_id, role) |> 
  filter(role=="matcher") |> 
  unique() |> 
  group_by(game_id, trial_num) |> 
  summarize(matchers=list(player_id))

try_trials <- file |> 
  select(condition_label, game_id, option_set, target, trial_num, rep_num, exclude,
         exclusion_reason) |> 
  unique() |> 
  left_join(condition |> select(condition_label, condition_id)) |> 
  left_join(describer) |> 
  left_join(matchers)


assert_that(try_trials |> select(game_id, trial_num) |> unique() |> nrow() == try_trials |> nrow(),
            msg="trials should be uniquely determined by game_id and trial_num")

trials <- try_trials |> 
  mutate(trial_id = row_number()) |> 
  select(-condition_label)


messages <- file |> filter(action_type=="message") |> 
  select(game_id, trial_num, text, player_id, role, message_number, message_irrelevant, time_stamp) |> 
  left_join(trials |> select(trial_id, game_id, trial_num)) |> 
  select(-trial_num, -game_id) 


try_choices <- file |> filter(action_type=="selection") |> 
  select(game_id, trial_num, choice_id, player_id, role, time_stamp) |> 
  left_join(trials |> select(trial_id, game_id, trial_num, option_set)) |> 
  mutate(option_set_list=str_split(option_set,";")) |> 
  select(-trial_num, -game_id) |> 
  rowwise() |>  #I'm sure there's a faster way to do this, but this is what I could find that works...
  mutate(check_choices = case_when(
    choice_id %in% option_set_list ~ T,
    choice_id == "timed_out" ~ T,
    is.na(choice_id) ~ T,
    T ~ F
  )) |> ungroup()

assert_that(all(try_choices$check_choices), msg="choice_id must be in option_set or timed_out or NA")

choices <- try_choices |> select(-option_set_list)
print("All checks pass!")

}

validate_dataset(read_csv("test_boyce.csv"))


