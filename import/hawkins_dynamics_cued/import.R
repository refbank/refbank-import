library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))

# Note this is only for the cued ones!
# uncued will have to wait for parsing out trial-level stuff


#TODOs
# exclusions -- expect 131 pairs --> -33 for disconnect ; -3 for native lang; 66/66 rule  excluded 8 for low accuracy 
# this has 83 --> seems like post exclusions then? 
# message relevancy
# choice timing
# message timing

url <- "https://raw.githubusercontent.com/hawkrobe/tangrams/refs/heads/master/data/tangrams_sequential/"

# read-in using Robert's code
sequentialMsgs = read_csv(str_c(url,"message/sequential_message_raw.csv")) %>%
  rename(msgTime = time, role = sender, repetitionNum = occurrenceNum, trialNum = roundNum)

sequentialClicks = read_csv(str_c(url,"clickedObj/sequential_clicks.csv")) %>% 
  rename(repetitionNum = occurrenceNum, trialNum = roundNum)

sequentialSubjInfo = read.csv(str_c(url, "tangrams_sequential-subject_information.csv"))

sequentialCombined.raw <- sequentialMsgs %>% 
  left_join(sequentialSubjInfo, by = c('gameid', 'role')) %>% 
  left_join(sequentialClicks, by = c('gameid', 'trialNum', 'repetitionNum'))

singleUtts = sequentialCombined.raw %>%
  group_by(gameid,trialNum) %>%
  mutate(contents = paste0(contents, collapse = '. ')) %>%
  filter(row_number() == 1)

### exclusions

# We detect a handful of workerIDs that occurred across multiple games
# from pre-processing --these are the only full games where a participant was a 'repeat'
duplicate_gameids <- c('0210-5d5ad8b6-7c94-4e2a-a3cb-760d2f613953', 
                       '9087-3c0f9d65-0427-406f-9251-94da5d2dee54', 
                       '7261-07035df8-84ee-4381-b72c-a04234c8f5ed')

# This one somehow got through wihtout director saying anything
garbage_gameids <- c('7840-4bd35c77-f10c-4055-a122-f591efae2826') 

incompleteIDs <- sequentialCombined.raw %>% 
  group_by(gameid) %>% 
  filter(length(unique(trialNum)) != 72) %>%
  pull(gameid) %>%
  unique()

nonNativeSpeakerIDs <- sequentialSubjInfo %>% 
  filter(nativeEnglish != "yes") %>%
  pull(gameid) %>%
  unique()

lowAccuracyIDs <- singleUtts %>% 
  filter(!(gameid %in% incompleteIDs)) %>%
  group_by(repetitionNum, gameid) %>%
  summarize(totalCorrect = sum(correct)) %>%
  mutate(lessThan66Pct = totalCorrect <= 2/3 * 12) %>%
  group_by(gameid) %>%
  filter(sum(lessThan66Pct) >= 2/3 * 6) %>%
  pull(gameid) %>%
  unique()

exclusion_duplicates <- tibble(gameid=duplicate_gameids, exclude=T, exclusion_reason="repeat participant")
exclusion_garbage <- tibble(gameid=garbage_gameids, exclude=T, exclusion_reason="describer didn't talk")
exclusion_incomplete <- tibble(gameid=incompleteIDs, exclude=T, exclusion_reason="incomplete game")
exclusion_nonnative <- tibble(gameid=nonNativeSpeakerIDs, exclude=T, exclusion_reason="non-native English-speaking participant")
exclusion_accuracy <- tibble(gameid=lowAccuracyIDs, exclude=T, exclusion_reason="low accuracy game") |> anti_join(exclusion_garbage |> select(gameid)) # prevent duplicate

exclude <- exclusion_duplicates |> bind_rows(exclusion_garbage) |> bind_rows(exclusion_incomplete) |> 
  bind_rows(exclusion_nonnative) |> bind_rows(exclusion_accuracy)

##### message corrections

spellcorrector = read_json(str_c(url, 'message/spell_correction.json'))
additional_spellcorrector = read_json(str_c(url,'message/additional_spell_correction.json'))
metacorrector = read_json(str_c(url,'message/meta-cleaning.json')) |> as_tibble() |> 
  pivot_longer(everything()) |> 
  separate(name, c("gameid", "trialNum", "contents"), sep="~~~") |> 
  mutate(trialNum=as.numeric(trialNum)) |> 
  filter(value=="") |> select(-value) |> 
  mutate(contents=str_replace_all(contents, fixed("\\"), "")) |> 
  mutate(message_irrelevant=T) 

# only match full words
names(spellcorrector) <- paste0('\\b', names(spellcorrector), '\\b')
names(additional_spellcorrector) <- paste0('\\b', names(additional_spellcorrector), '\\b')

messages <- sequentialCombined.raw %>%
  mutate(contents = tolower(contents)) %>%
  mutate(contents = str_replace_all(contents, 
                                    unlist(spellcorrector, 
                                           use.names = T))) |> 
  mutate(contents = str_replace_all(contents,
                                    unlist(additional_spellcorrector,
                                           use.names = T))) |> 
  left_join(metacorrector, 
            by = c('trialNum', 'gameid', "contents")) |> 
  mutate(contents = str_trim(contents)) |> 
  group_by(gameid, trialNum, repetitionNum) |> 
  mutate(message_number=row_number() |> as.numeric()) |> 
  mutate(time_stamp=as.numeric(NA)) |> 
  #we are unsure of timestamps -- there are both msgTime and timeElapsed, 
  #but they don't obviously line up, so not sure we have reliable time to message
  select(gameid, trialNum, repetitionNum, role, intendedName, time_stamp, contents, 
         message_number, message_irrelevant) |> 
  ungroup() |> 
  mutate(action_type="message")
  

### choices
choices <- sequentialClicks |> 
  mutate(role="matcher") |> 
  select(gameid, trialNum, repetitionNum, role, intendedName=intendedObj, clickedObj, role) |> 
  mutate(time_stamp=as.numeric(NA), #there is a time column, but I don't think we have a start of trial indicator to baseline with
         action_type="selection"
         ) 

####
options=c("A", "B","C","D","E","F","G","H","I", "J", "K", "L")



all <- choices |> bind_rows(messages) |> 
  left_join(exclude) |> 
  mutate(dataset_id="hawkins2020_characterizing_cued",
         full_cite="Hawkins, R. D., Frank, M. C., & Goodman, N. D. (2020). Characterizing the dynamics of learning in repeated reference games. Cognitive science, 44(6), e12845.",
         short_cite="Hawkins et al. (2020)",
         condition_label="cued",
         structure="thick",
         player_id=str_c(gameid, role), # we know there aren't role switches in this experiment 
         stage_num=1,
         room_num=1,
         age=as.numeric(NA), #we may have this?
         gender=as.character(NA),
         trial_num=trialNum, 
         rep_num=repetitionNum,
         role=case_when(
           role=="director" ~ "describer",
           role=="matcher" ~ "matcher"
         ),
         group_size=2,
         group_structure="thick",
         language="English",
         exclude=ifelse(exclude, exclude, F), # fill in F for ones not excluded
         ) |>
  mutate(option_set=str_c(options, collapse=";")) |> 
  rename(game_id=gameid,
         target=intendedName,
         text=contents,
         choice_id=clickedObj,
        ) |> 
  select(dataset_id, condition_label, full_cite, short_cite,
         stage_num, room_num, 
         language,
         game_id, player_id, age, gender, trial_num,rep_num,
         role, target, message_number, text,
         choice_id, time_stamp, option_set,
         group_size,
         structure,
         exclude, exclusion_reason, action_type,
         message_irrelevant
  )


validate_dataset(all, write=T)

