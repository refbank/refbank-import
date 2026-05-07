library(tidyverse)
library(here)

transcript <- read_csv(here("import/hawkins2020_characterizing/raw_data/segmented_transcript.csv"))
raw_messages <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/message/rawUnconstrainedMessages.csv"
raw_choices <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/finalBoard/tangramsFinalBoards.csv"
raw_demog <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/turk/tangrams-subject_information.csv"

# Load and prepare messages data
messages_df <- read_csv(raw_messages) %>%
  rename(role = sender, msgTime = time)

# Load tangram boards
choices <- read_csv(raw_choices) %>%
  rename(
    rep_num = roundNum,
  ) |>
  select(-score, -time) |>
  group_by(gameid, rep_num) |>
  pivot_longer(c(starts_with("true"), starts_with("sub")), names_to = "type", values_to = "trial_num") |>
  mutate(
    source = str_sub(type, 1, -2),
    tangram = str_sub(type, -1, -1)
  ) |>
  select(-type) |>
  pivot_wider(names_from = source, values_from = tangram) |>
  rename(target = true, choice_id = sub) |>
  mutate(
    trial_num = (rep_num - 1) * 12 + trial_num,
    player_id = str_c(gameid, "_", "matcher"),
    action_type = "selection",
    role = "matcher"
  ) |>
  select(gameid, rep_num, trial_num, choice_id, target, player_id, action_type, role)


target_info <- choices |> select(gameid, rep_num, trial_num, target)

pre_transcript <- read_csv(raw_messages) |>
  mutate(
    game = as.factor(gameid) |> as.numeric(),
    grid = roundNum,
    targetPosition = "",
    role = ifelse(sender == "director", "describer", "matcher"),
    message = contents,
    message_id_num = row_number()
  ) |>
  distinct(game, gameid)

messages <- transcript |>
  left_join(pre_transcript) |>
  mutate(
    message_irrelevant = ifelse(target_position == "chitchat", T, NA),
    target_position = ifelse(target_position == "chitchat", NA, target_position)
  ) |>
  group_by(grid, game) |>
  fill(target_position, .direction = "downup") |>
  ungroup() |>
  mutate(
    rep_num = as.numeric(grid),
    trial_num = (rep_num - 1) * 12 + as.numeric(target_position)
  ) |>
  group_by(trial_num, gameid) |>
  mutate(message_number = row_number() |> as.numeric()) |>
  ungroup() |>
  mutate(
    action_type = "message",
    player_id = str_c(gameid, "_", role)
  ) |>
  left_join(target_info) |>
  filter(is.na(gameid)) |>
  select(rep_num, trial_num, gameid, role, text = message, message_number, message_irrelevant, action_type, player_id, target) |>
  filter(!is.na(trial_num)) # confirmed these are all not relevant

missing_messages <- target_info |> # sometimes they don't describe the last one b/c process of elimination
  anti_join(messages |> filter(role == "describer")) |>
  mutate(
    role = "describer",
    player_id = str_c(gameid, "_", role),
    text = NA,
    message_number = NA,
    action_type = "message"
  )

# Load subject information
subj_df <- read_csv(raw_demog) %>%
  rename(gameid = gameID) %>%
  mutate(role = ifelse(role == "director", "describer", role)) %>%
  select(gameid, role, nativeEnglish)


# Apply exclusions
# Find incomplete games
incomplete_games <- messages_df %>%
  count(gameid, roundNum) %>%
  count(gameid, name = "rounds") %>%
  filter(rounds != 6) %>%
  pull(gameid)

# Find non-native English games
non_native_games <- subj_df %>%
  filter(nativeEnglish != "yes") %>%
  pull(gameid)

# Find low accuracy games
low_accuracy_games <- read_csv(raw_choices) %>%
  select(gameid, roundNum, score) |>
  unique() |>
  group_by(gameid) |>
  add_tally() |>
  ungroup() |>
  filter(n == 6) |>
  filter(score < 8) |>
  group_by(gameid) |>
  tally() |>
  filter(n > 3) |>
  pull(gameid)

# Create exclusion tibble
exclusion_df <- tribble(
  ~gameid, ~exclusion_reason,
  incomplete_games, "incomplete game",
  non_native_games, "non-native English-speaking participant",
  low_accuracy_games, "low accuracy game",
  "0574-6", "incomplete game (manual)"
) %>%
  unnest(cols = c(gameid, exclusion_reason)) %>%
  group_by(gameid) %>%
  summarise(
    exclude = TRUE,
    exclusion_reason = str_c(exclusion_reason, collapse = "; "),
    .groups = "drop"
  ) %>%
  # Add games not excluded
  bind_rows(
    tibble(
      gameid = setdiff(unique(messages_df$gameid), .$gameid),
      exclude = FALSE,
      exclusion_reason = "NA"
    )
  )


all_uncued <- messages |>
  bind_rows(choices) |>
  bind_rows(missing_messages) |>
  left_join(select(subj_df, "gameid", "role", "nativeEnglish")) %>%
  left_join(exclusion_df) |>
  mutate(
    dataset_id = "hawkins2020_characterizing",
    condition_label = "unconstrained",
    full_cite = "Hawkins, R. D., Frank, M. C., & Goodman, N. D. (2020). Characterizing the dynamics of learning in repeated reference games. Cognitive science, 44(6), e12845.",
    short_cite = "Hawkins et al. (2020)",
    language = "English",
    player_id = str_c(gameid, "_", role),
    room_num = 1,
    stage_num = 1,
    time_stamp = NA_real_,
    age = as.numeric(NA),
    gender = as.character(NA),
    native_language = ifelse(nativeEnglish == tolower("yes"), "English", NA),
    race = as.character(NA),
    education = as.character(NA),
    option_set = "A;B;C;D;E;F;G;H;I;J;K;L;",
    group_size = 2,
    prior_relationship = "no",
    population = "adult",
    partner_constancy = "yes",
    role_constancy = "yes",
    confederates = "no",
    modality = "written",
    feedback = "full",
    backchannel = "full",
    order_match = "order",
  ) |>
  rename(game_id = gameid) |>
  select(-nativeEnglish)
