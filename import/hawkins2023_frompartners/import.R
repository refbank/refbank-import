library(here)
library(tidyverse)
library(jsonlite)


ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep = " "), " ]") %>%
    fromJSON(flatten = T)
}


url <- "https://raw.githubusercontent.com/hawkrobe/conventions_model/refs/heads/master/data/experiment3/"
# note that in github this is expt 3 even if in paper it's expt 2


messages <- read_csv(str_c(url, "messages.csv")) |>
  mutate(role = case_when(
    role == "speaker" ~ "describer",
    role == "listener" ~ "matcher"
  )) |>
  select(role, networkid, roomid, trialnum, partnernum, stimsetid, target_image = target, participantid, text = content) |>
  mutate(
    target_image = str_replace(target_image, "tangram_", ""),
    target_image = str_replace(target_image, ".png", "")
  ) |>
  group_by(networkid, roomid, trialnum, partnernum) |>
  mutate(message_num = row_number() |> as.numeric()) |>
  ungroup() |>
  mutate(
    action_type = "message",
    message_irrelevant = F
  ) # we don't have data on this


contexts <- messages |>
  select(networkid, roomid, trialnum, target_image) |>
  unique() |>
  mutate(
    image_options = case_when(
      target_image %in% c("A", "B", "C", "D") ~ "A;B;C;D",
      target_image %in% c("E", "F", "G", "L") ~ "E;F;G;L",
      target_image %in% c("I", "J", "K", "H") ~ "I;J;K;H"
    ),
    stim_set_id = case_when(
      target_image %in% c("A", "B", "C", "D") ~ 0,
      target_image %in% c("E", "F", "G", "L") ~ 1,
      target_image %in% c("I", "J", "K", "H") ~ 2
    ),
    target_name = target_image,
    distr0 = case_when(
      target_image == "A" ~ "B",
      target_image %in% c("B", "C", "D") ~ "A",
      target_image == "E" ~ "F",
      target_image %in% c("F", "G", "L") ~ "E",
      target_image == "I" ~ "J",
      target_image %in% c("J", "K", "H") ~ "I",
    ),
    distr1 = case_when(
      target_image %in% c("A", "B") ~ "C",
      target_image %in% c("C", "D") ~ "B",
      target_image %in% c("E", "F") ~ "G",
      target_image %in% c("G", "L") ~ "F",
      target_image %in% c("I", "J") ~ "K",
      target_image %in% c("K", "H") ~ "J",
    ),
    distr2 = case_when(
      target_image == "D" ~ "C",
      target_image %in% c("A", "B", "C") ~ "D",
      target_image == "L" ~ "G",
      target_image %in% c("E", "F", "G") ~ "L",
      target_image == "H" ~ "K",
      target_image %in% c("I", "J", "K") ~ "H",
    )
  ) |>
  pivot_longer(c("target_image", "distr0", "distr1", "distr2"), names_to = "object_id", values_to = "selected_image") |>
  mutate(object_id = ifelse(object_id == "target_image", "target", object_id)) |> # clicks.csv's raw object_id values use "target", not our schema's target_image
  rename(target_image = target_name)

roles <- messages |>
  mutate(round_num = trialnum %/% 4) |>
  select(networkid, roomid, participantid, role, partnernum, round_num) |>
  filter(role == "describer") |>
  unique() |>
  group_by(networkid, roomid, partnernum) |>
  mutate(
    matcher = lag(participantid),
    matcher2 = lead(participantid)
  ) |>
  mutate(matcher = ifelse(is.na(matcher), matcher2, matcher)) |>
  # I fully recognize this is an incredibly hacky way to identify matchers but it works
  select(networkid, roomid, partnernum, round_num, participantid = matcher)

clicks <- read_csv(str_c(url, "clicks.csv")) |>
  mutate(role = "matcher") |>
  select(networkid, roomid, trialnum, partnernum, stim_set_id, object_id, role) |>
  mutate(round_num = trialnum %/% 4) |>
  left_join(roles) |>
  mutate(action_type = "selection") |>
  left_join(contexts |> select(-image_options, -target_image)) |>
  left_join(contexts |> select(networkid, roomid, image_options, trialnum, target_image) |> unique())


# exclude messages from matchers on trials where the describer didn't talk
# only applies to two trials
describer_talked <- messages |>
  filter(role == "describer") |>
  select(networkid, roomid, trialnum) |>
  unique()

messages_with_context <- messages |>
  inner_join(describer_talked) |>
  left_join(contexts |> ungroup() |> select(networkid, image_options) |> unique())


completeNetworks <- clicks %>%
  distinct() %>%
  group_by(networkid) %>%
  tally() %>%
  filter(n == 96) %>%
  select(networkid) |>
  mutate(exclude = F, exclusion_reason = NA)

all <- messages_with_context |>
  bind_rows(clicks) |>
  left_join(completeNetworks) |>
  mutate(
    game_id = networkid,
    room_num = roomid %% 2 + 1, # for consistency with others, start with 1 and reset each
    player_id = str_c(networkid, "_", participantid),
    dataset_id = "hawkins2023_frompartners",
    full_cite = "Hawkins, R. D., Franke, M., Frank, M. C., Goldberg, A. E., Smith, K., Griffiths, T. L., & Goodman, N. D. (2023). From partners to populations: A hierarchical Bayesian account of coordination and convention. Psychological Review, 130(4), 977.",
    short_cite = "Hawkins et al. (2023)",
    language = "English",
    stage_num = partnernum + 1,
    round_num = trialnum %/% 4,
    trial_num = 1 + trialnum + partnernum * 16,
    round_num = 1 + round_num + 4 * partnernum,
    time_stamp = as.numeric(NA), # didn't find timestamps in source
    age = as.numeric(NA), # TODO demographics
    gender = as.character(NA),
    race = as.character(NA),
    education = as.character(NA),
    native_language = as.character(NA),
    group_size = 4,
    prior_relationship = "no",
    partner_constancy = "no",
    role_constancy = "no",
    confederates = "no",
    modality = "written",
    feedback = "full",
    backchannel = "full",
    order_match = "match",
    condition_label = "pairs-network",
    population = "adult",
    exclude = ifelse(is.na(exclude), T, exclude),
    exclusion_reason = ifelse(exclude, "incomplete game", NA),
  ) |>
  select(
    dataset_id, full_cite, short_cite, language,
    stage_num,
    condition_label, time_stamp,
    game_id, player_id, room_num,
    trial_num, round_num,
    role, target_image,
    age, gender, race, education, native_language, population,
    action_type, exclude, exclusion_reason,
    message_num, text,
    selected_image, image_options,
    group_size, message_irrelevant,
    prior_relationship, partner_constancy, role_constancy, confederates, modality,
    feedback, backchannel, order_match
  )


source(here("validate.R"))

validate_dataset(all, write = T)
