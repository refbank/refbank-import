library(here)
library(tidyverse)
library(jsonlite)

# from paper Experiment 2 
# 92 participants in networks or 4 (23 networks)
# networks got 1 of 3 contexts
# each pair does 4 blocks of 4 each 
# do all 3 pairs  
# not time-synced so wait if one pair is faster 


ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T)
}


url <- "https://raw.githubusercontent.com/hawkrobe/conventions_model/refs/heads/master/data/experiment3/"
# note that in github this is expt 3 even if in paper it's expt 2


messages <- read_csv(str_c(url,'messages.csv')) |> 
  mutate(role=case_when(
    role=="speaker" ~ "describer",
    role=="listener" ~ "matcher")) |> 
  select(role, networkid, roomid, trialnum, partnernum, stimsetid, target, participantid, text=content) |> 
  mutate(target=str_replace(target, "tangram_", ""),
         target=str_replace(target, ".png", "")) |> 
  group_by(networkid, roomid, trialnum, partnernum) |> 
  mutate(message_number=row_number() |> as.numeric()) |> ungroup() |> 
  mutate(action_type="message",
         message_irrelevant=F) # we don't have data on this


contexts <- messages |> select(networkid, roomid, trialnum, target) |> unique() |> 
  mutate(
  option_set=case_when(
    target %in% c("A", "B", "C", "D") ~ "A;B;C;D",
    target %in% c("E", "F", "G", "L") ~ "E;F;G;L",
    target %in% c("I", "J", "K", "H") ~ "I;J;K;H"
  ),
  stim_set_id=case_when(
    target %in% c("A", "B", "C", "D") ~ 0,
    target %in% c("E", "F", "G", "L") ~ 1,
    target %in% c("I", "J", "K", "H") ~ 2
  ),
  target_name=target,
  dist0=case_when( # there are all based on the idea that the distractors are labeled in order from what's left of the set
    # see https://github.com/hawkrobe/conventions_model/blob/master/reference_game/experiment.py#L102 for why we think that
    target=="A" ~ "B",
    target %in% c("B", "C","D") ~ "A",
    target=="E" ~ "F",
    target %in% c("F", "G", "L") ~ "E",
    target=="I" ~ "J",
    target %in% c("J", "K", "H") ~ "I",
  ),
  dist1=case_when(
    target %in% c("A","B") ~ "C",
    target %in% c("C","D") ~ "B",
    target %in% c("E", "F") ~ "G",
    target %in% c("G", "L") ~ "F",
    target %in% c("I", "J") ~ "K",
    target %in% c("K", "H") ~ "J",
  ),
  dist2=case_when(
    target=="D" ~ "C",
    target %in% c("A", "B", "C") ~ "D",
    target=="L" ~ "G",
    target %in% c("E", "F", "G") ~ "L",
    target=="H" ~ "K",
    target %in% c("I","J", "K") ~ "H",
  )) |> 
  pivot_longer(c("target", "dist0", "dist1", "dist2"), names_to="object_id", values_to="choice_id") |> 
  rename(target=target_name)

roles <- messages |> mutate(rep_num=trialnum%/%4) |> 
select(networkid, roomid, participantid, role, partnernum, rep_num) |> filter(role=="describer") |> unique() |>  
  group_by(networkid, roomid, partnernum) |> 
  mutate(matcher=lag(participantid),
         matcher2=lead(participantid)) |> 
  mutate(matcher=ifelse(is.na(matcher), matcher2, matcher)) |> 
# I fully recognize this is an incredibly hacky way to identify matchers but it works
  select(networkid, roomid, partnernum, rep_num, participantid=matcher)

clicks <- read_csv(str_c(url,'clicks.csv')) |> 
  mutate(role="matcher") |> 
  select(networkid, roomid, trialnum, partnernum, stim_set_id, object_id, role) |> 
  mutate(rep_num=trialnum%/%4) |> 
  left_join(roles) |> 
  mutate(action_type="selection") |> left_join(contexts |> select(-option_set, -target)) |> 
  left_join(contexts |> select(networkid,roomid, option_set, trialnum, target) |> unique())
  
messages_with_context <- messages |> 
  left_join(contexts |> ungroup() |> select(networkid, option_set) |> unique())


completeNetworks <- clicks %>% 
  distinct() %>% 
  group_by(networkid) %>% 
  tally() %>%
  filter(n == 96) %>% 
  select(networkid) |> 
  mutate(exclude=F, exclusion_reason=NA)

all <- messages_with_context |> 
  bind_rows(clicks) |> 
  left_join(completeNetworks) |> 
    mutate(game_id=networkid,
           room_num=roomid%%2+1, #for consistency with others, start with 1 and reset each
           player_id=str_c(networkid,"_", participantid),
           dataset_id="hawkins2023_frompartners",
           full_cite="Hawkins, R. D., Franke, M., Frank, M. C., Goldberg, A. E., Smith, K., Griffiths, T. L., & Goodman, N. D. (2023). From partners to populations: A hierarchical Bayesian account of coordination and convention. Psychological Review, 130(4), 977.",
           short_cite="Hawkins et al (2023)",
           language="English",
           stage_num=partnernum+1,
           rep_num=trialnum%/%4,
           trial_num=1+trialnum+partnernum*16,
           rep_num=1+rep_num+4*partnernum,
           time_stamp=as.numeric(NA), # didn't find timestamps in source
           age=as.numeric(NA), #TODO demographics
           gender=as.character(NA),
           group_size=4,
           structure="network-swap",
           condition_label="pairs-network",
           exclude=ifelse(is.na(exclude), T, exclude),
           exclusion_reason=ifelse(exclude, "incomplete game", NA), 
           ) |> 
    select(dataset_id, full_cite, short_cite, language,
           stage_num, 
           condition_label, time_stamp,
           game_id, player_id, room_num,
           trial_num,rep_num,
           role, target, 
           age, gender,
           action_type, exclude, exclusion_reason, 
           message_number, text,
           choice_id, option_set,
           group_size, message_irrelevant,
           structure
    )

source(here("validate.R"))

validate_dataset(all, write=T)

