library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))
###### read in data from github
url <- "https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/"

source(here("import/boyce2024_interaction/process_pilots.R"))

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


one_round_results <- read_rds(str_c(url, "data/study1/round_results.rds")) %>% mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_round_results <- read_rds(str_c(url, "data/study2a/round_results.rds")) %>% mutate(rotate = "no_rotate")
two_b_round_results <- read_rds(str_c(url, "data/study2b/round_results.rds")) %>% mutate(rotate = "full_feedback")
two_c_round_results <- read_rds(str_c(url, "data/study2c/round_results.rds")) |> mutate(rotate = "emoji")
three_round_results <- read_rds(str_c(url, "data/study3/round_results.rds")) |> rename(`_id` = "X_id", condition = name)


one_round_include <- read_rds(str_c(url, "data/study1/rounds_include.rds")) %>% mutate(rotate = str_c(as.character(numPlayers), "_rotate"))
two_a_round_include <- read_rds(str_c(url, "data/study2a/rounds_include.rds")) %>% mutate(rotate = "no_rotate")
two_b_round_include <- read_rds(str_c(url, "data/study2b/rounds_include.rds")) %>% mutate(rotate = "full_feedback")
two_c_round_include <- read_rds(str_c(url, "data/study2c/rounds_include.rds")) |> mutate(rotate = "emoji")
three_round_include <- read_rds(str_c(url, "data/study3/rounds_include.rds")) |> rename(condition = name)

all_include <- one_round_include |>
  rbind(two_a_round_include) |>
  rbind(two_b_round_include) |>
  rbind(two_c_round_include) |>
  rbind(three_round_include) |>
  mutate(include = T) |>
  bind_rows(pilot_include) |>
  # keep only the real join keys: pilot_include has no numPlayers column, and the
  # rotate/condition/name columns are redundant with (and inconsistently named across)
  # what good_choices/good_chat already carry -- see note at the join below.
  # distinct() because pilot_include itself has repeated (gameId, repNum) rows (one per
  # player/trial in the source pilot_round_results, never deduped) -- previously masked
  # because the broken join never matched pilot rows at all
  select(gameId, repNum, include) |>
  distinct()


options <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
##### do message processing

combined_chat <- one_chat |>
  rbind(two_a_chat) |>
  rbind(two_b_chat) |>
  rbind(two_c_chat) |>
  rbind(pilot_chat) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  rbind(three_chat) |>
  mutate(
    text = ifelse(is.na(spellchecked), text, spellchecked),
    text = gsub("\\n", "", fixed = T, text), # note that this is using spellcorrected version!!!!
    text = str_squish(text),
    tangram = gsub("/experiment/tangram_", "", target, fixed = TRUE),
    tangram = gsub(".png", "", tangram, fixed = TRUE)
  ) %>%
  select(gameId, trialNum, repNum, tangram, playerId, role, numPlayers, text, condition, is.chitchat) |>
  group_by(gameId, trialNum, repNum) |>
  mutate(message_num = row_number()) |>
  ungroup() |>
  mutate(action_type = "message") |>
  mutate(role = case_when(
    role == "speaker" ~ "describer",
    role == "listener" ~ "matcher"
  )) |>
  filter(!is.na(tangram))



##### do result processing

combined_results <- one_round_results |>
  rbind(two_a_round_results) |>
  rbind(two_b_round_results) |>
  rbind(two_c_round_results) |>
  bind_rows(pilot_round_results) |>
  mutate(activePlayerCount = NA) |>
  rename(condition = rotate) |>
  bind_rows(three_round_results) |>
  mutate(
    selected_image = gsub("/experiment/tangram_", "", response, fixed = TRUE),
    selected_image = gsub(".png", "", selected_image, fixed = TRUE)
  )

##### determine when people weren't actually there

last_present <- combined_results |>
  select(gameId, playerId, trialNum, selected_image) |>
  filter(selected_image %in% options) |>
  group_by(gameId, playerId) |>
  summarize(lasttrialNum = max(trialNum))

selections <- combined_results |>
  left_join(last_present) |>
  mutate(
    selected_image = case_when(
      selected_image %in% options ~ selected_image, # if there's a choice, keep it
      is.na(lasttrialNum) ~ NA, # if there's never a choice, NA
      trialNum > lasttrialNum + 1 ~ NA, # if it's more than one after the last choice, NA
      T ~ "timed_out"
    ), # otherwise, it's a timed_out
    time = case_when(
      time < 0 ~ NA, # something went wrong here and we don't know how to fix,
      time > 180 ~ NA, # again if > 180 there's a problem with recording,
      T ~ time
    ),
    time_stamp = case_when(
      selected_image %in% options ~ time,
      selected_image == "timed_out" ~ 180, # known max time for trial
      T ~ NA
    ),
    action_type = "selection",
    role = "matcher"
  ) |>
  filter(!is.na(selected_image))

#### exclusions

# missing describers: there are trials where a matcher talks but nothing else happens, we're going to exclude
good_chat <- combined_chat |> inner_join(combined_chat |> filter(role == "describer") |> select(gameId, trialNum) |> unique())

# exclude trials where there is no describer talking
good_choices <- selections |> inner_join(combined_chat |> filter(role == "describer") |> select(gameId, trialNum) |> unique())




all_data <- good_choices |>
  bind_rows(good_chat) |>
  # explicit join key: all_include originally carried a "rotate" column (study1/2) and a
  # "condition" column (study3 only, renamed from "name"), each NA on the rows that don't
  # have it, plus pilot_include has no "numPlayers" at all. Since good_choices/good_chat
  # already has a real "condition" value on every row, an unqualified left_join() used to
  # pick "condition" up as an accidental extra join key -- NA on every study1/2 row of
  # all_include, so the join silently failed for 100% of study1/2 data (include ended up
  # NA for all of it). (gameId, repNum) is globally unique and sufficient on its own.
  left_join(all_include, by = c("gameId", "repNum")) |>
  mutate(
    dataset_id = "boyce2024_interaction",
    trial_num = trialNum + 1,
    round_num = repNum + 1,
    full_cite = "Boyce, V., Hawkins, R. D., Goodman, N. D., & Frank, M. C. (2024). Interaction structure constrains the emergence of conventions in group communication. Proceedings of the National Academy of Sciences, 121(28), e2403888121.",
    short_cite = "Boyce et al. (2024)",
    group_size = case_when(
      condition %in% c("emoji", "full_feedback", "no_rotate") ~ 6,
      T ~ str_sub(condition, 1, 1) |> as.numeric()
    ), # note this matches condition, not actual player count necessarily
    prior_relationship = "no",
    partner_constancy = "yes",
    role_constancy = case_when(
      str_detect(condition, "thin") ~ "no",
      str_detect(condition, "thick") ~ "yes",
      condition == "emoji" ~ "no",
      condition == "full_feedback" ~ "no",
      condition == "no_rotate" ~ "yes",
      str_detect(condition, "rotate") ~ "no",
      
    ),
    confederates = "no",
    modality = "written",
    feedback = case_when(
      str_detect(condition, "thin") ~ "limited",
      str_detect(condition, "thick") ~ "full",
      condition == "emoji" ~ "limited",
      condition == "full_feedback" ~ "full",
      condition == "no_rotate" ~ "limited",
      str_detect(condition, "rotate") ~ "limited",
      
    ),
    backchannel = case_when(
      str_detect(condition, "thin") ~ "limited",
      str_detect(condition, "thick") ~ "full",
      condition == "emoji" ~ "limited",
      condition == "full_feedback" ~ "full",
      condition == "no_rotate" ~ "full",
      str_detect(condition, "rotate") ~ "full"
    ),
    condition=case_when(
      str_detect(condition, "thin")~ str_c("expt3_", condition),
      str_detect(condition, "thick")~ str_c("expt3_", condition),
      condition%in%c("emoji", "full_feedback","no_rotate") ~ str_c("expt2_", condition),
      str_detect(condition, "rotate")~str_c("expt1_", condition)
    ),
    order_match = "match",
    room_num = 1,
    stage_num = 1,
    language = "English",
    exclude = case_when(
      is.na(include) ~ T,
      include == F ~ T,
      T ~ F
    ),
    exclusion_reason = case_when(
      include == F ~ "pilot", # this is only used for pilot data
      exclude ~ "incomplete block",
      T ~ NA
    ),
    message_irrelevant = (is.chitchat == 1),
    message_num = as.numeric(message_num),
    population = "adult"
  ) |>
  rowwise() |>
  mutate(image_options = options |> str_c(collapse = ";")) |>
  ungroup() |>
  select(
    condition_label = condition,
    dataset_id,
    full_cite,
    short_cite,
    group_size,
    language,
    prior_relationship, partner_constancy, role_constancy, population,
    confederates, modality, feedback, backchannel, order_match,
    game_id = gameId,
    room_num,
    image_options,
    target_image = tangram,
    stage_num,
    trial_num,
    round_num,
    exclude,
    exclusion_reason,
    action_type,
    player_id = playerId,
    role,
    time_stamp,
    text,
    message_num,
    message_irrelevant,
    selected_image
  ) |>
  arrange(game_id) |>
  left_join(read_csv(here("import/boyce2024_interaction/demogs.csv")))




validate_dataset(all_data, write = T)
