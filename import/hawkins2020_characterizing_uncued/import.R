# nolint start
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(logger)
library(tibble)
library(here)
# Logging setup
log_appender(appender_file("segmentation.log"))
log_layout(layout_glue_colors)

# Define segment patterns as a tibble for easier manipulation
segment_patterns <- tribble(
  ~pattern, ~type,
  "\\b(\\d{1,2})(?:st|nd|rd|th)\\s*(?:one|image|picture)\\b", "ordinal",
  "\\b(\\d{1,2})\\.\\s*", "numbered",
  "\\b(\\d{1,2})\\s*:\\s*", "colon",
  "\\b(\\d{1,2})\\s*-", "dash",
  "\\bimage\\s*(\\d{1,2})\\b", "image_num",
  "\\bpicture\\s*(\\d{1,2})\\b", "picture_num",
  "\\bnumber\\s*(\\d{1,2})\\b", "number_num",
  "\\bnext\\s*(?:one|image|picture)\\b", "next_one",
  "\\bfirst\\s*(?:one|image|picture)\\b", "first_one",
  "\\bsecond\\s*(?:one|image|picture)\\b", "second_one",
  "\\bthird\\s*(?:one|image|picture)\\b", "third_one",
  "\\bfourth\\s*(?:one|image|picture)\\b", "fourth_one",
  "\\bfifth\\s*(?:one|image|picture)\\b", "fifth_one",
  "\\bsixth\\s*(?:one|image|picture)\\b", "sixth_one",
  "\\bseventh\\s*(?:one|image|picture)\\b", "seventh_one",
  "\\beighth\\s*(?:one|image|picture)\\b", "eighth_one",
  "\\bninth\\s*(?:one|image|picture)\\b", "ninth_one",
  "\\btenth\\s*(?:one|image|picture)\\b", "tenth_one",
  "\\beleventh\\s*(?:one|image|picture)\\b", "eleventh_one",
  "\\btwelfth\\s*(?:one|image|picture)\\b", "twelfth_one",
  "\\b(\\d{1,2})\\b(?!\\d)", "standalone"
)

# Create ordinal mapping as a named vector
ordinal_map <- c(
  first = 1, second = 2, third = 3, fourth = 4, fifth = 5,
  sixth = 6, seventh = 7, eighth = 8, ninth = 9, tenth = 10,
  eleventh = 11, twelfth = 12
)

extract_image_number <- function(text, type) {
  case_when(
    type %in% c(
      "numbered", "colon", "dash", "standalone", "ordinal",
      "image_num", "picture_num", "number_num"
    ) ~
      as.integer(str_extract(text, "\\d+")),
    type == "next_one" ~ -1L,
    str_ends(type, "_one") ~ {
      word <- str_remove(type, "_one")
      ordinal_map[word] %||% 0L
    },
    TRUE ~ 0L
  ) %>%
    list(num = ., text = text)
}

segment_description <- function(text, expected_segments = 12) {
  # Initialize segments as a named list
  segments <- set_names(vector("list", expected_segments), seq_len(expected_segments))
  segments <- map(segments, ~ character(0))

  current_num <- 1
  last_pos <- 1

  # Find all matches using tidyverse approach
  matches <- segment_patterns %>%
    pmap(function(pattern, type) {
      str_locate_all(text, regex(pattern, ignore_case = TRUE))[[1]] %>%
        as_tibble() %>%
        filter(start > 0) %>%
        mutate(
          match_text = str_sub(text, start, end),
          type = type,
          num = map2_int(match_text, type, ~ extract_image_number(.x, .y)$num)
        )
    }) %>%
    bind_rows() %>%
    arrange(start)

  # Process matches
  for (i in seq_len(nrow(matches))) {
    match_row <- matches[i, ]

    # Add text before match to current segment
    if (match_row$start > last_pos) {
      segments[[current_num]] <- c(
        segments[[current_num]],
        str_sub(text, last_pos, match_row$start - 1)
      )
    }

    # Handle "next" pattern
    if (match_row$num == -1) {
      match_row$num <- current_num + 1
    }

    # Add match to appropriate segment
    if (match_row$num >= 1 && match_row$num <= expected_segments) {
      segments[[match_row$num]] <- c(segments[[match_row$num]], match_row$match_text)
      current_num <- match_row$num
      last_pos <- match_row$end + 1
    }
  }

  # Add remaining text
  if (last_pos <= nchar(text)) {
    segments[[current_num]] <- c(
      segments[[current_num]],
      str_sub(text, last_pos, nchar(text))
    )
  }

  # Collapse and clean segments
  segments %>%
    map_chr(~ str_trim(str_c(.x, collapse = " ")))
}

# Helper function using case_when for cleaner logic
num_to_letter <- function(num) {
  case_when(
    is.na(num) | num == "" ~ "NA",
    !str_detect(as.character(num), "^[0-9]+$") ~ "NA",
    as.integer(num) < 1 | as.integer(num) > 12 ~ "NA",
    TRUE ~ LETTERS[as.integer(num)]
  )
}

# Data processing workflow

INPUT_FILE <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/message/rawUnconstrainedMessages.csv"

BOARD_FILE <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/finalBoard/tangramsFinalBoards.csv"

SUBJ_FILE <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/turk/tangrams-subject_information.csv"

# Load and prepare messages data
messages_df <- read_csv(INPUT_FILE) %>%
  rename(role = sender, msgTime = time)

# Load tangram boards
boards_dict <- read_csv(BOARD_FILE) %>%
  rename(
    repetitionNum = roundNum,
    repetitionScore = score
  ) %>%
  group_by(gameid, repetitionNum) |>
  pivot_longer(c(starts_with("true"), starts_with("sub")), names_to = "type", values_to = "trial_number") |>
  mutate(
    source = str_sub(type, 1, -2),
    tangram = str_sub(type, -1, -1)
  ) |>
  select(-type) |>
  pivot_wider(names_from = source, values_from = tangram) |>
  rename(targets = true, selections = sub, score = repetitionScore)

# Load subject information
subj_df <- read_csv(SUBJ_FILE) %>%
  rename(gameid = gameID) %>%
  mutate(role = ifelse(role == "director", "describer", role)) %>%
  select(gameid, nativeEnglish, role) %>%
  mutate()

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
low_accuracy_games <- boards_dict |>
  select(gameid, repetitionNum, score) |>
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

# Process game messages
id_cols <- c("gameid", "roundNum")
options <- LETTERS[1:12]
option_set <- str_c(options, collapse = ";")

# Group and collapse messages
grouped_messages <- messages_df %>%
  group_by(across(all_of(id_cols))) %>%
  summarise(
    contents = str_c(na.omit(contents), collapse = " "),
    msgTime = first(msgTime),
    .groups = "drop"
  ) %>%
  mutate(
    segments = map(contents, segment_description),
    key = str_c(gameid, roundNum, sep = "_")
  )


clean_messages <- grouped_messages |>
  # Filter out keys not in boards_dict
  # filter(key %in% names(boards_dict)) %>%
  # Add trial numbering
  arrange(gameid, roundNum) %>%
  # Expand segments
  mutate(segment_id = row_number()) %>%
  unnest_longer(segments, indices_to = "trial_number") %>%
  mutate(trial_number = as.numeric(trial_number)) |>
  rename(repetitionNum = roundNum) |>
  left_join(boards_dict, by = c("gameid", "trial_number", "repetitionNum")) |>
  left_join(exclusion_df, by = "gameid") %>%
  replace_na(list(exclude = FALSE, exclusion_reason = "NA"))

# Create message and choice rows
message_rows <- clean_messages |>
  select(
    gameid, repetitionNum, segments, msgTime,
    trial_number, targets, exclude, exclusion_reason
  ) |>
  filter(!is.na(targets)) |>
  mutate(
    role = "describer",
    action_type = "message",
    time_stamp = as.numeric(msgTime),
    message_irrelevant = F,
    message_number = 1
  )

choice_rows <- clean_messages %>%
  select(
    gameid, repetitionNum, segments, msgTime,
    trial_number, targets, selections, exclude, exclusion_reason
  ) |>
  filter(!is.na(selections)) |>
  filter(!is.na(targets)) |>
  mutate(role = "matcher", action_type = "selection")


result <- bind_rows(message_rows, choice_rows) |>
  left_join(select(subj_df, "gameid", "role", "nativeEnglish")) %>%
  mutate(
    dataset_id = "hawkins2020_characterizing_uncued",
    condition_label = "unconstrained",
    full_cite = "Hawkins, R. D., Frank, M. C., & Goodman, N. D. (2020). Characterizing the dynamics of learning in repeated reference games. Cognitive science, 44(6), e12845.",
    short_cite = "Hawkins et al. (2020)",
    language = "English",
    game_id = gameid,
    player_id = str_c(gameid, "_", role),
    trial_num = trial_number + 12 * (as.numeric(repetitionNum) - 1),
    rep_num = repetitionNum,
    room_num = 1,
    stage_num = 1,
    age = as.numeric(NA),
    gender = as.character(NA),
    native_language = ifelse(nativeEnglish == tolower("yes"), "English", NA),
    race = as.character(NA),
    education = as.character(NA),
    target = targets,
    text = "NA",
    choice_id = selections,
    option_set = option_set,
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
  arrange(game_id, trial_num, desc(action_type)) |>
  mutate(trial_num = as.numeric(trial_num)) |>
  select(-gameid, -msgTime, -repetitionNum, -segments, -trial_number, -targets, -selections, -nativeEnglish)

source(here("validate.R"))
validate_dataset(result, write = T)
# nolint end
