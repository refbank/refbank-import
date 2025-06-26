# nolint start
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

process_leung_dataset <- function(combined_file, exchanges_file, output_file) {
  # Load data
  combined_df <- read_csv(combined_file, show_col_types = FALSE)
  exchanges_df <- read_csv(exchanges_file, locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)

  # Ensure subid is character and trim whitespace in both frames
  combined_df$subid <- str_trim(as.character(combined_df$subid))
  exchanges_df$subid <- str_trim(as.character(exchanges_df$subid))

  # Prepare age lookup (unique and numeric)
  age_lookup <- exchanges_df %>%
    select(subid, age) %>%
    distinct()
  age_lookup$age <- as.numeric(age_lookup$age)

  # Vectorized match: lookup age for each combined_df row
  combined_df$age_lookup <- age_lookup$age[match(combined_df$subid, age_lookup$subid)]
  # Fill any missing ages with -1
  combined_df$age_lookup[is.na(combined_df$age_lookup)] <- -1

  combined_df <- combined_df %>%
    group_by(subid) %>%
    arrange(subid, rep_num, trial) %>%
    mutate(trial_num_overall = row_number()) %>%
    ungroup()

  # Get global option set
  option_set <- combined_df$target %>%
    unique() %>%
    sort() %>%
    str_c(collapse = ";")

  records <- list()
  global_trial_num <- 0

  grouped <- combined_df %>%
    group_by(subid, trial, rep_num) %>%
    arrange(subid, trial, rep_num) %>%
    group_split()

  for (group in grouped) {
    game_id <- as.character(group$subid[1])
    trial_num <- group$trial_num_overall[1]
    rep_num <- group$rep_num[1]
    target <- group$target[1]
    condition_label <- group$experiment[1]
    age <- group$age_lookup[1]
    gender <- "unknown"

    global_trial_num <- global_trial_num + 1
    stage_num <- 1
    room_num <- 1
    group_size <- 2
    structure <- "thick"
    language <- "English"

    full_cite <- "Leung, A., Yurovsky, D., & Hawkins, R. D. (2024)..."
    short_cite <- "Leung et al. (2024)"
    dataset_id <- "leung2024_scaffolding"

    # Describer message
    dir_msg <- group %>%
      filter(role == "director", !is.na(utterance)) %>%
      tail(1)

    message_text <- if (nrow(dir_msg) > 0) dir_msg$utterance else NA_character_
    message_number <- if (nrow(dir_msg) > 0) 1 else NA_real_
    message_irrelevant <- FALSE
    describer_pid <- paste0(game_id, "_describer")

    # Matcher selection
    valid_options <- str_split(option_set, ";")[[1]]
    matcher_row <- exchanges_df %>%
      filter(subid == group$subid[1],
             trial == group$trial[1],
             rep_num == group$rep_num[1],
             role == "matcher") %>%
      filter(!is.na(selection)) %>%
      tail(1)
    if (nrow(matcher_row) > 0) {
      raw_choice <- as.character(matcher_row$selection)
      if (!is.na(raw_choice) && raw_choice %in% valid_options) {
        choice_id <- raw_choice
      } else {
        choice_id <- "timed_out"
      }
    } else {
      choice_id <- "timed_out"
    }

    matcher_pid <- paste0(game_id, "_matcher")

    # Append describer message
    records[[length(records) + 1]] <- tibble(
      condition_label = condition_label,
      dataset_id = dataset_id,
      full_cite = full_cite,
      short_cite = short_cite,
      group_size = group_size,
      structure = structure,
      language = language,

      game_id = game_id,
      room_num = room_num,
      stage_num = stage_num,
      option_set = option_set,
      target = target,
      trial_num = trial_num,
      rep_num = rep_num,
      exclude = FALSE,
      exclusion_reason = "",

      action_type = "message",
      player_id = describer_pid,
      role = "describer",
      time_stamp = NA_real_,
      age = age,
      gender = gender,

      text = message_text,
      message_number = message_number,
      message_irrelevant = FALSE,

      choice_id = NA_character_
    )

    # Append matcher selection
    records[[length(records) + 1]] <- tibble(
      condition_label = condition_label,
      dataset_id = dataset_id,
      full_cite = full_cite,
      short_cite = short_cite,
      group_size = group_size,
      structure = structure,
      language = language,

      game_id = game_id,
      room_num = room_num,
      stage_num = stage_num,
      option_set = option_set,
      target = target,
      trial_num = trial_num,
      rep_num = rep_num,
      exclude = FALSE,
      exclusion_reason = "",

      action_type = "selection",
      player_id = matcher_pid,
      role = "matcher",
      time_stamp = NA_real_,
      age = age,
      gender = gender,

      text = NA_character_,
      message_number = NA_real_,
      message_irrelevant = NA,

      choice_id = choice_id
    )
  }

  final_df <- bind_rows(records)
  write_csv(final_df, output_file, na = "")
  return(final_df)
}

# File paths
COMBINED_FILE <- "import/leung2024_scaffolding/raw_data/combined_clean.csv"
EXCHANGES_FILE <- "import/leung2024_scaffolding/raw_data/exchanges.csv"
OUTPUT_FILE <- "import/leung2024_scaffolding/segmented_data.csv"

cat("Segmenting data for Leung et al. (2024)...\n")
segmented_df <- process_leung_dataset(COMBINED_FILE, EXCHANGES_FILE, OUTPUT_FILE)
cat("Segmented data written to:", OUTPUT_FILE, "\n")


source(here("validate.R"))
test <- read_csv(OUTPUT_FILE, show_col_types = FALSE) %>%
   mutate(
    gender = if_else(is.na(gender) | gender == "", "unknown", gender),
    exclusion_reason = as.character(exclusion_reason),
    time_stamp = as.numeric(time_stamp),
    text = as.character(text),
    message_number = as.numeric(message_number),
    choice_id = as.character(choice_id),
    message_irrelevant = as.logical(message_irrelevant),
    age = as.numeric(age)
  )
validate_dataset(test, write = FALSE)

# nolint end
