library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

# Load data
# some sort of data error on trial 21 of game 107 (adult adult, where both players are listed as matchers)
# guessing that right is the describer so that it's alternating across 20 and 22 ? 
combined_df <- read_csv(here("import/leung2024_scaffolding/raw_data/combined_clean.csv")) |> 
  mutate(role=case_when(
    subid==107 & trial==21 & person=="right" ~ "director",
    T ~ role
  ))

selections <- combined_df |> 
  select(subid, trial, person, role, target, rep_num, age, experiment, correct) |> 
  filter(role=="matcher") |> unique() |> 
  mutate(choice_id=ifelse(correct, target, "unk1"),
         action_type="message")

messages <- combined_df |> select(-correct, -director) |> 
  filter(!is.na(utterance)) |> 
  group_by(subid, trial) |> 
  mutate(message_number=row_number() |> as.numeric(),
         text=utterance,
         message_irrelevant=NA,
         action_type="message") |> ungroup()

# some sort of data error on trial 21 of game 107 (adult adult, where both players are listed as matchers)
# guessing that right is the describer so that it's alternating across 20 and 22 ? 

missing_messages <- combined_df |> select(subid, trial, person, role, director, age, experiment, target, rep_num) |>   # these are all matchers!
  anti_join(messages |> filter(role=="director") |> select(subid, trial, person) |> unique()) |> 
  mutate(person=director, 
         role="director",
         message_number=as.numeric(NA),
         text=NA,
         message_irrelevant=NA,
         action_type="message") |> select(-director)
  

  
all <- messages |> bind_rows(selections) |> bind_rows(missing_messages) |> 
  mutate(game_id=as.character(subid) |> str_trim(),
         role=ifelse(role=="matcher", "matcher", "describer"),
         player_id=str_c(game_id, "_", person),
         option_set=str_c(target, "unk1", sep = ";"), # we don't know what the distractor is per trial!
         age=ifelse(person=="child", age, NA) |> as.numeric(),
         gender=as.character(NA),
         stage_num=1,
         room_num=1,
         group_size=2, 
         structure="nofeedback", #need to systematize in future!
         language="English",
         full_cite= "Leung, A., Yurovsky, D., & Hawkins, R. D. (2025). Parents spontaneously scaffold the formation of conversational pacts with their children. Child Development, 96(2), 546-561.",
         short_cite="Leung et al. (2024)",
         dataset_id="leung2024_scaffolding",
         exclude=F,
         exclusion_reason=as.character(NA),
         time_stamp=as.numeric(NA),
         )|> 
  rename(trial_num=trial, 
         condition_label=experiment,
         ) |> 
  select(-subid, -person, -utterance, -correct)

source(here("validate.R"))

validate_dataset(all, write = T)

