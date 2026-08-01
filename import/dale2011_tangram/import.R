# Load Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(here)
library(fs)


# Set Metadata
dataset_id <- "dale2011_tangram"
full_cite <- "Dale, R., Kirkham, N. Z., & Richardson, D. C. (2011). How two people become a tangram recognition system. In Proceedings of the European Conference on Computer-Supported Cooperative Work."
short_cite <- "Dale et al. (2011)"
group_size <- 2
language <- "English"
prior_relationship <- "no"
partner_constancy <- "yes"
population <- "adult"
role_constancy <- "yes"
confederates <- "no"
modality <- "oral-remote"
feedback <- "none"
backchannel <- "full"

# Helper to Read a .dat File
read_tangram_dat <- function(path) {
  tryCatch(
    {
      df <- read_table(path,
        col_names = FALSE,
        show_col_types = FALSE,
        col_types = "ddddd",
        progress = FALSE
      )

      if (ncol(df) != 5) {
        warning("Expected 5 columns in ", basename(path), ", got ", ncol(df))
      }

      colnames(df) <- c("tangram_mouse", "correct_tangram", "turn_time", "director_eye", "matcher_eye")
      return(df)
    },
    error = function(e) {
      warning("Error reading ", basename(path), ": ", e$message)
      return(tibble(
        tangram_mouse = numeric(0),
        correct_tangram = numeric(0),
        turn_time = numeric(0),
        director_eye = numeric(0),
        matcher_eye = numeric(0)
      ))
    }
  )
}

# Find All .dat Files
data_dir <- here("import/dale2011_tangram/raw_data")
dat_files <- dir_ls(data_dir, regexp = "\\.dat$")

# Extract Info and Harmonize
cat("Found", length(dat_files), ".dat files to process...\n")

# Read all files and add metadata
all_data <- map_dfr(dat_files, function(file) {
  m <- str_match(basename(file), "^tan(\\d+)\\.(\\d+)\\.dat$")
  round_num <- as.numeric(m[2])
  dyad_id <- as.numeric(m[3])

  cat(".")

  df <- read_tangram_dat(file)

  if (nrow(df) == 0) {
    return(NULL)
  }

  df %>%
    mutate(
      round_num = round_num,
      dyad_id = dyad_id
    )
})

cat("\n")

# Create matcher selections (only valid tangram selections 1-6)
matcher_df <- all_data %>%
  mutate(matcher_best_guess = case_when( # if missing try with matcher eye instead
    tangram_mouse %in% c(1, 2, 3, 4, 5, 6) ~ tangram_mouse,
    matcher_eye %in% c(1, 2, 3, 4, 5, 6) ~ matcher_eye,
    T ~ NA
  )) |>
  filter(matcher_best_guess %in% c(1, 2, 3, 4, 5, 6)) %>%
  filter(correct_tangram %in% c(1, 2, 3, 4, 5, 6)) |>
  group_by(correct_tangram, round_num, dyad_id) |>
  filter(turn_time == max(turn_time)) |>
  group_by(dyad_id) |>
  mutate(
    trial_num = row_number(),
    condition_label = "experiment 1", # only one condition
    dataset_id = dataset_id,
    full_cite = full_cite,
    short_cite = short_cite,
    group_size = group_size,
    language = language,
    prior_relationship = prior_relationship,
    partner_constancy = partner_constancy,
    population = population,
    role_constancy = role_constancy,
    confederates = confederates,
    modality = modality,
    feedback = feedback,
    backchannel = backchannel,
    game_id = paste0("dyad", dyad_id),
    room_num = 1,
    image_options = "1;2;3;4;5;6",
    target_image = as.character(correct_tangram),
    stage_num = 1,
    exclude = FALSE,
    exclusion_reason = NA_character_,
    order_match = "order",
    action_type = "selection",
    player_id = paste0("dyad", dyad_id, "_matcher"),
    role = "matcher",
    time_stamp = as.numeric(turn_time) / 1000,
    age = NA_real_,
    gender = NA_character_,
    native_language = NA_character_,
    race = NA_character_,
    education = NA_character_,
    text = NA_character_,
    message_num = NA_real_,
    message_irrelevant = FALSE,
    selected_image = as.character(matcher_best_guess)
  ) %>%
  ungroup()

# Add describer entries (one per trial) for validation
describer_df <- matcher_df %>%
  select(
    condition_label, dataset_id, full_cite, short_cite, group_size, language,
    prior_relationship, partner_constancy, population, role_constancy,
    confederates, modality, feedback, backchannel,
    game_id, room_num, image_options, target_image, trial_num, round_num, stage_num,
    exclude, exclusion_reason, order_match, dyad_id
  ) %>%
  distinct() %>%
  mutate(
    action_type = "message",
    player_id = paste0("dyad", dyad_id, "_describer"),
    role = "describer",
    time_stamp = NA_real_,
    age = NA_real_,
    gender = "unknown",
    native_language = NA_character_,
    race = NA_character_,
    education = NA_character_,
    text = NA_character_,
    message_num = NA_real_,
    message_irrelevant = FALSE,
    selected_image = NA_character_
  ) %>%
  select(-dyad_id)

# Combine matcher selections and describer entries
harmonized_df <- bind_rows(
  matcher_df %>% select(
    -dyad_id, -tangram_mouse,
    -correct_tangram, -turn_time, -director_eye, -matcher_eye
  ),
  describer_df
) %>%
  select(
    condition_label, dataset_id, full_cite, short_cite, group_size, language,
    prior_relationship, partner_constancy, population, role_constancy,
    confederates, modality, feedback, backchannel,
    game_id, room_num, image_options, target_image, trial_num, round_num, stage_num,
    exclude, exclusion_reason, order_match,
    action_type, player_id, role, time_stamp, age, gender, native_language,
    race, education, text, message_num, message_irrelevant, selected_image
  )

# Save Harmonized Data
write_csv(harmonized_df, "import/dale2011_tangram/dale2011_tangram_harmonized.csv", na = "")

# Validation |>
source("validate.R")
df <- read_csv("import/dale2011_tangram/dale2011_tangram_harmonized.csv", show_col_types = FALSE)

# Fix column types
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

validate_dataset(df, write = TRUE)
