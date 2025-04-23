library(here)
library(tidyverse)


raw_data_dir <- here("import/hawkins_dynamics/raw_data")

#TODO parse out trials for the uncued ones!

games <- read_csv(here(raw_data_dir, "tangramsSequential.csv")) 
  
choices <- games |> 
  select(gameid, trialNum, repetitionNum, role, intendedName, clickedObj) |> 
  unique() |> 
  mutate(time_to_choice=NA,
         player_id=str_c(gameid, "matcher")) # timing info is probably possible but difficult? 


options=c("A", "B","C","D","E","F","G","H","I", "J", "K", "L")


all <- games |> filter(!is.na(contents)) |> 
  group_by(gameid, trialNum, repetitionNum) |> 
  mutate(message_num=row_number()) |> 
  select(gameid, trialNum, repetitionNum, role, intendedName, timeElapsed, contents, message_num) |> 
  ungroup() |> 
  full_join(choices) |> 
  mutate(paper_id="hawkins2020_characterizing",
         experiment_id="cued",
         structure="thick",
         player_id=str_c(gameid, role), # we know there aren't role switches in this experiment 
         trial_num=trialNum, 
         rep_num=repetitionNum,
         role=case_when(
           role=="director" ~ "describer",
           role=="matcher" ~ "matcher"
         ),
         group_size=2,
         group_structure="thick") |>
  rowwise() |> 
  mutate(option_set=list(options)) |> 
  ungroup() |> 
  rename(game_id=gameid,
         target_id=intendedName,
         message=contents,
         choice_id=clickedObj,
        ) |> 
  select(paper_id, experiment_id, game_id, player_id, trial_num,rep_num,
         role, target_id, message_num, message,
         choice_id, time_to_choice, option_set,
         group_size,
         structure
  )

all |> write_csv(here("harmonized_data/hawkins_dynamics.csv"))


