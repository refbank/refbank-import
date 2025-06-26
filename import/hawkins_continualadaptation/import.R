library(here)
library(tidyverse)
library(jsonlite)



url <- "https://raw.githubusercontent.com/hawkrobe/continual-adaptation/refs/heads/main/data/"


# read in human-human
clickedObj.raw.humanhuman <- read_csv(str_c(url, "human-human/clickedObjFromMongo.csv")) %>%
  filter(iterationName != "testing") %>%
  mutate(trialNum = as.numeric(trialNum), repNum = as.numeric(repNum)) %>%
  select(-timeFromMessage, -targetImg)

chatMessages.raw.humanhuman <- read_csv(str_c(url, "human-human/messageFromMongo.csv")) %>%
  filter(iterationName != "testing") %>%
  mutate(uttLength = str_count(msg, "\\S+")) %>%
  group_by(gameid, trialNum, repNum, targetImg, context_id) 

incompleteIDs.humanhuman <- clickedObj.raw.humanhuman %>%
  group_by(gameid) %>%
  tally() %>%
  filter(n != 24) %>%
  select(gameid) |>
  mutate(exclude = T, exclusion_reason = "incomplete game")

d.humanhuman <- left_join(chatMessages.raw.humanhuman, clickedObj.raw.humanhuman,
  by = c("gameid", "trialNum", "repNum", "context_id")
) %>%
  ungroup() |> 
  mutate(targetImg = gsub('{"url":"', "", targetImg, fixed = TRUE)) %>%
  mutate(targetImg = gsub('","targetStatus":"target"}', "", targetImg, fixed = TRUE)) %>%
  left_join(incompleteIDs.humanhuman) |>
  mutate(condition = "human-human")


# Read in model-as-listener data
clickedObj.raw.modellistener <- read_csv(str_c(url, "model-as-listener/clickedObjFromMongo.csv")) %>%
  mutate(trialNum = as.numeric(trialNum), repNum = as.numeric(repNum)) %>%
  select(-timeFromMessage, -targetImg)

chatMessages.raw.modellistener <- read_csv(str_c(url, "model-as-listener/messageFromMongo.csv")) %>%
  mutate(uttLength = str_count(msg, "\\S+")) %>%
  select(-timeFromRoundStart, -iterationName, -time)

incompleteIDs.modellistener <- clickedObj.raw.modellistener %>%
  group_by(gameid) %>%
  tally() %>%
  filter(n != 24) %>%
  select(gameid) |>
  mutate(exclude = T, exclusion_reason = "incomplete game") |>
  bind_rows(tibble(
    gameid = "9297-a1aff98a-9088-4347-a738-87ae5acba53c",
    exclude = T,
    exclusion_reason = "speaker typed nonsense"
  ))


d.modellistener <- left_join(chatMessages.raw.modellistener, clickedObj.raw.modellistener,
  by = c("gameid", "trialNum", "repNum", "context_id")
) %>%
  filter(iterationName == "final-for-paper") %>%
  ungroup() %>%
  mutate(targetImg = gsub('{"url":"', "", targetImg, fixed = TRUE)) %>%
  mutate(targetImg = gsub('","targetStatus":"target"}', "", targetImg, fixed = TRUE)) %>%
  left_join(incompleteIDs.modellistener) |>
  mutate(condition = "human-speaker-model-listener")


# Read in 'easy' human-human data
clickedObj.raw.humanseasy <- read_csv(str_c(url, "human-human-easy/clickedObjFromMongo.csv")) %>%
  mutate(trialNum = as.numeric(trialNum), repNum = as.numeric(repNum)) %>%
  select(-timeFromMessage, -targetImg)
chatMessages.raw.humanseasy <- read_csv(str_c(url, "human-human-easy/messageFromMongo.csv")) %>%
  select(-timeFromRoundStart, -iterationName, -time)
incompleteIDs.humanseasy <- clickedObj.raw.humanseasy %>%
  group_by(gameid) %>%
  tally() %>%
  filter(n != 24) %>%
  select(gameid) |>
  mutate(exclude = T, exclusion_reason = "incomplete game")


d.humanhumaneasy <- left_join(chatMessages.raw.humanseasy, clickedObj.raw.humanseasy,
  by = c("gameid", "trialNum", "repNum", "context_id")
) %>%
  ungroup() %>%
  mutate(targetImg = gsub('{"url":"', "", targetImg, fixed = TRUE)) %>%
  mutate(targetImg = gsub('","targetStatus":"target"}', "", targetImg, fixed = TRUE)) %>%
  left_join(incompleteIDs.humanseasy) |>
  mutate(condition = "human-human-easy")




#### combine
combined <- d.humanhuman |>
  bind_rows(d.humanhumaneasy, d.modellistener) |>
  mutate(
    exclusion_reason = case_when(
      !is.na(exclusion_reason) ~ exclusion_reason,
      iterationName == "pilot0" ~ "pilot",
      is.na(iterationName) ~ "pilot?"
    ),
    exclude = ifelse(!is.na(exclusion_reason), T, F)
  )

combined |>
  select(gameid, iterationName, condition, exclude) |>
  unique() |>
  group_by(iterationName, condition) |>
  tally()

### trying to figure out image contexts

# each group sees 1 context 

ParseJSONColumn <- function(x) {
  if(is.na(x)){x="{}"}
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T)
}

contexts <- combined |> select(context_id, targetImg, fullContext, condition) |>
  filter(!is.na(fullContext)) |> 
  mutate(fullContext=map(fullContext, .f=ParseJSONColumn)) |> 
  unnest(fullContext) |> 
  unnest(fullContext)

concat_contexts <- contexts |> group_by(context_id, targetImg, condition) |> summarize(option_set=str_c(url, collapse=";"))

clicked <- contexts |> select(clickedObj=targetStatus, context_id, targetImg, condition, choice_id=url)


### split 
chat <- combined |> select(
  gameid, trialNum, repNum, targetImg, context_id, msg,
  exclude, exclusion_reason, condition
) |> 
  mutate(role="describer",
         player_id=str_c(gameid, "_describer")) |> 
  group_by(gameid, trialNum, repNum) |> 
  mutate(message_number=row_number() |> as.numeric()) |> 
  ungroup() |> 
  left_join(concat_contexts) |> 
  mutate(action_type="message",
         message_irrelevant=F) # we don't have annotations for msg relevancy

choices <- combined |> select(
  gameid, trialNum, repNum, targetImg, context_id, exclude,
  exclusion_reason, condition,
  clickedObj
) |> unique() |> mutate(role="matcher", player_id=str_c(gameid, "_matcher"),
                        player_id=ifelse(condition=="human-speaker-model-listener", "model", player_id)) |> 
  left_join(concat_contexts) |> 
  left_join(clicked) |> 
  mutate(action_type="selection") |>
  mutate(choice_id=ifelse(is.na(choice_id), "timed_out", choice_id))



### combine everything 
all <- chat |>
  bind_rows(choices) |>
  mutate(
    dataset_id = "hawkins2019_continual",
    full_cite = "Hawkins, R. D., Kwon, M., Sadigh, D., & Goodman, N. D. (2019). Continual adaptation for efficient machine communication. Proceedings of the 24th Conference on Computational Natural Language Learning.",
    short_cite = "Hawkins et al (2019)",
    language = "English",
    trial_num = trialNum + 1,
    stage_num=1,
    room_num=1,
    rep_num = 1 + repNum,
    age=as.numeric(NA),
    gender=as.character(NA),
    time_stamp = as.numeric(NA), # there is some time info in original data but unclear how to map it
    group_size = ifelse(condition=="human-speaker-model-listener", 1, 2), # counting number of actual people? idk
    structure = ifelse(condition=="human-speaker-model-listener", "thin", "medium"),
    ) |>
  filter(!is.na(option_set)) |> # this is two games in pilots where there wasn't context info in source 
  select(dataset_id, full_cite, short_cite, language, stage_num, 
    condition_label=condition, time_stamp,
    game_id=gameid, room_num,
    player_id, age, gender,
    trial_num, rep_num,
    role, target=targetImg,
    action_type, exclude, exclusion_reason,
    message_number, text=msg,
    choice_id, option_set,
    group_size, message_irrelevant,
    structure
  )

source(here("validate.R"))

validate_dataset(all, write = T)
