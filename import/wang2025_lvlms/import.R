# Load Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(here)

# Basic dataset info
dataset_id <- "wang2025_lvlms"
full_cite <- "Wang, Z., Li, W., Kaliosis, P., Rambow, O., & Brennan, S. E. (2025, November). LVLMs are Bad at Overhearing Human Referential Communication. In Proceedings of the 2025 Conference on Empirical Methods in Natural Language Processing (pp. 16769-16793)."
short_cite <- "Wang et al. (2025)"
group_size <- 2
structure <- "thick"
language <- "English"

# Harmonization constants based on the paper
prior_relationship <- "no"
partner_constancy <- "yes"
population <- "adult"
role_constancy <- "yes"
confederates <- "no"
modality <- "oral-remote"
feedback <- "none"
backchannel <- "full"

# File paths
baskets_file <- here("import/wang2025_lvlms/raw_data/baskets-matching-data-plus-formal.xlsx")
dogs_file <- here("import/wang2025_lvlms/raw_data/dogs-matching-data-plus-formal.xlsx")

# Helper Function to Harmonize
harmonize_matching_dataset <- function(df, condition_label) {
  # Identify player columns
  player_cols <- grep("^P\\d+", names(df), value = TRUE)

  # Add a unique trial identifier that combines Round and the item number
  df <- df %>%
    mutate(
      item_num = as.numeric(str_extract(Order, "\\d+")),
      trial_id_temp = (Round - 1) * 10 + item_num
    )

  # Pivot players long
  df_long <- df %>%
    pivot_longer(
      cols = all_of(player_cols),
      names_to = "player",
      values_to = "text"
    )

  # Split conversations into individual messages
  # Split by D: and M:
  messages_list <- list()

  for (i in 1:nrow(df_long)) {
    row <- df_long[i, ]
    conv_text <- row$text

    if (is.na(conv_text) || conv_text == "") next

    # Split by newlines and identify speaker
    lines <- strsplit(as.character(conv_text), "\n")[[1]]

    current_speaker <- NULL
    current_text <- c()
    message_num <- 1

    for (line in lines) {
      line <- str_trim(line)
      if (line == "") next

      # Check if line starts with D: or M:
      if (str_detect(line, "^D:")) {
        # Save previous message if exists
        if (!is.null(current_speaker) && length(current_text) > 0) {
          messages_list[[length(messages_list) + 1]] <- list(
            condition_label = condition_label,
            game_id = paste0(condition_label, "_", row$player),
            player_id = paste0(condition_label, "_", row$player),
            role = current_speaker,
            trial_id_temp = row$trial_id_temp,
            trial_num = row$trial_id_temp,
            Round = row$Round,
            Answer = row$Answer,
            text = paste(current_text, collapse = "\n"),
            message_num = message_num
          )
          message_num <- message_num + 1
          current_text <- c()
        }
        current_speaker <- "describer"
        current_text <- c(str_trim(str_remove(line, "^D:")))
      } else if (str_detect(line, "^M:")) {
        # Save previous message if exists
        if (!is.null(current_speaker) && length(current_text) > 0) {
          messages_list[[length(messages_list) + 1]] <- list(
            condition_label = condition_label,
            game_id = paste0(condition_label, "_", row$player),
            player_id = paste0(condition_label, "_", row$player),
            role = current_speaker,
            trial_id_temp = row$trial_id_temp,
            trial_num = row$trial_id_temp,
            Round = row$Round,
            Answer = row$Answer,
            text = paste(current_text, collapse = "\n"),
            message_num = message_num
          )
          message_num <- message_num + 1
          current_text <- c()
        }
        current_speaker <- "matcher"
        current_text <- c(str_trim(str_remove(line, "^M:")))
      } else {
        # Continuation of previous speaker OR unprefixed line
        if (!is.null(current_speaker)) {
          # Continuation of current speaker
          current_text <- c(current_text, line)
        } else {
          # First line without D: or M: prefix - treat as describer
          current_speaker <- "describer"
          current_text <- c(line)
        }
      }
    }

    # Save last message
    if (!is.null(current_speaker) && length(current_text) > 0) {
      messages_list[[length(messages_list) + 1]] <- list(
        condition_label = condition_label,
        game_id = paste0(condition_label, "_", row$player),
        player_id = paste0(condition_label, "_", row$player),
        role = current_speaker,
        trial_id_temp = row$trial_id_temp,
        trial_num = row$trial_id_temp,
        Round = row$Round,
        Answer = row$Answer,
        text = paste(current_text, collapse = "\n"),
        message_num = message_num
      )
    }
  }

  # Convert list to dataframe
  messages_df <- bind_rows(messages_list)

  # Ensure every trial has at least one describer message
  trials_with_describer <- messages_df %>%
    filter(role == "describer") %>%
    distinct(game_id, trial_num)

  all_trials <- df_long %>%
    distinct(game_id = paste0(condition_label, "_", player), trial_id_temp) %>%
    rename(trial_num = trial_id_temp)

  missing_describer_trials <- all_trials %>%
    anti_join(trials_with_describer, by = c("game_id", "trial_num"))

  if (nrow(missing_describer_trials) > 0) {
    cat(sprintf(
      "Warning: %d trials missing describer, adding placeholder messages\n",
      nrow(missing_describer_trials)
    ))

    # For each missing trial, add a describer message with the original text
    for (i in 1:nrow(missing_describer_trials)) {
      trial_info <- missing_describer_trials[i, ]

      # Find the original row
      orig_row <- df_long %>%
        filter(
          paste0(condition_label, "_", player) == trial_info$game_id,
          trial_id_temp == trial_info$trial_num
        ) %>%
        slice(1)

      if (nrow(orig_row) > 0) {
        messages_df <- bind_rows(messages_df, tibble(
          condition_label = condition_label,
          game_id = trial_info$game_id,
          player_id = trial_info$game_id,
          role = "describer",
          trial_id_temp = trial_info$trial_num,
          trial_num = trial_info$trial_num,
          Round = orig_row$Round,
          Answer = orig_row$Answer,
          text = str_trim(as.character(orig_row$text)),
          message_num = 0
        ))
      }
    }
  }

  # Add all the required columns
  harmonized <- messages_df %>%
    rowwise() |>
    mutate(
      dataset_id = dataset_id,
      full_cite = full_cite,
      short_cite = short_cite,
      group_size = group_size,
      structure = structure,
      language = language,
      prior_relationship = prior_relationship,
      partner_constancy = partner_constancy,
      population = population,
      role_constancy = role_constancy,
      confederates = confederates,
      modality = modality,
      feedback = feedback,
      backchannel = backchannel,
      room_num = 1,
      image_options = str_c(condition_label, "_", 1:10, sep = "", collapse = ";"),
      target_image = str_c(condition_label, "_", as.character(Answer)),
      round_num = Round,
      stage_num = 1,
      exclude = FALSE,
      exclusion_reason = NA_character_,
      order_match = "match",
      action_type = "message",
      time_stamp = NA_real_,
      age = NA_real_,
      gender = "unknown",
      native_language = NA,
      race = NA,
      education = NA,
      message_irrelevant = FALSE,
      selected_image = NA_character_,
      condition_label = "experiment_1",
      text = str_replace_all(text, "\n", " ")
    ) %>%
    select(
      condition_label, dataset_id, full_cite, short_cite, group_size, language, prior_relationship, partner_constancy,
      population, role_constancy, confederates, modality, feedback, backchannel,
      game_id, room_num, image_options, target_image, trial_num, round_num, stage_num,
      exclude, exclusion_reason, order_match,
      action_type, player_id, role, time_stamp, age, gender, native_language, race, education,
      text, message_num, message_irrelevant, selected_image
    )

  return(harmonized)
}

# Read & Harmonize Data
baskets_raw <- read_excel(baskets_file)
dogs_raw <- read_excel(dogs_file)

baskets_harmonized <- harmonize_matching_dataset(baskets_raw, "basket")
dogs_harmonized <- harmonize_matching_dataset(dogs_raw, "dog")

# Combine and Save
all_messages <- bind_rows(baskets_harmonized, dogs_harmonized)

all_choices <- all_messages |>
  select(-action_type, -text, -message_num, -message_irrelevant) |>
  distinct() |>
  filter(role == "matcher") |>
  mutate(text = NA, message_num = NA, message_irrelevant = NA, action_type = "selection", selected_image = target_image)

all_harmonized <- all_choices |> bind_rows(all_messages)
write_csv(all_harmonized, here("import/wang2025_lvlms/LVLMs2025_overhearing_harmonized.csv"), na = "")

cat("Wrote harmonized data to LVLMs2025_overhearing_harmonized.csv\n")

# Validate
source("validate.R")

df <- read_csv(here("import/wang2025_lvlms/LVLMs2025_overhearing_harmonized.csv"), show_col_types = FALSE)

df <- df %>%
  mutate(
    group_size = as.numeric(group_size),
    room_num = as.numeric(room_num),
    trial_num = as.numeric(trial_num),
    round_num = as.numeric(round_num),
    stage_num = as.numeric(stage_num),
    time_stamp = as.numeric(time_stamp),
    message_num = as.numeric(message_num),
    age = as.numeric(age),
    prior_relationship = as.character(prior_relationship),
    partner_constancy = as.character(partner_constancy),
    population = as.character(population),
    role_constancy = as.character(role_constancy),
    confederates = as.character(confederates),
    modality = as.character(modality),
    feedback = as.character(feedback),
    backchannel = as.character(backchannel),
    order_match = as.character(order_match),
    dataset_id = as.character(dataset_id),
    full_cite = as.character(full_cite),
    short_cite = as.character(short_cite),
    language = as.character(language),
    image_options = as.character(image_options),
    exclusion_reason = as.character(exclusion_reason),
    action_type = as.character(action_type),
    player_id = as.character(player_id),
    role = as.character(role),
    text = as.character(text),
    gender = as.character(gender),
    native_language = as.character(native_language),
    race = as.character(race),
    education = as.character(education),
    selected_image = as.character(selected_image)
  )

# Check if all trials have both describer and matcher messages
trial_roles <- df %>%
  group_by(game_id, room_num, trial_num) %>%
  summarise(
    has_describer = any(role == "describer"),
    has_matcher = any(role == "matcher"),
    n_messages = n(),
    .groups = "drop"
  )

cat("\n=== Now running validation ===\n")
validate_dataset(df, write = TRUE)
