# nolint start
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(logger)

# Logging setup
log_appender(appender_file("segmentation.log"))
log_layout(layout_glue_colors)

# Define segment patterns
segment_patterns <- list(
  list(pattern = "\\b(\\d{1,2})(?:st|nd|rd|th)\\s*(?:one|image|picture)\\b", type = "ordinal"), 
  list(pattern = "\\b(\\d{1,2})\\.\\s*", type = "numbered"),
  list(pattern = "\\b(\\d{1,2})\\s*:\\s*", type = "colon"),
  list(pattern = "\\b(\\d{1,2})\\s*-", type = "dash"),
  list(pattern = "\\bimage\\s*(\\d{1,2})\\b", type = "image_num"),
  list(pattern = "\\bpicture\\s*(\\d{1,2})\\b", type = "picture_num"),
  list(pattern = "\\bnumber\\s*(\\d{1,2})\\b", type = "number_num"),
  list(pattern = "\\bnext\\s*(?:one|image|picture)\\b", type = "next_one"),
  list(pattern = "\\bfirst\\s*(?:one|image|picture)\\b", type = "first_one"),
  list(pattern = "\\bsecond\\s*(?:one|image|picture)\\b", type = "second_one"),
  list(pattern = "\\bthird\\s*(?:one|image|picture)\\b", type = "third_one"),
  list(pattern = "\\bfourth\\s*(?:one|image|picture)\\b", type = "fourth_one"),
  list(pattern = "\\bfifth\\s*(?:one|image|picture)\\b", type = "fifth_one"),
  list(pattern = "\\bsixth\\s*(?:one|image|picture)\\b", type = "sixth_one"),
  list(pattern = "\\bseventh\\s*(?:one|image|picture)\\b", type = "seventh_one"),
  list(pattern = "\\beighth\\s*(?:one|image|picture)\\b", type = "eighth_one"),
  list(pattern = "\\bninth\\s*(?:one|image|picture)\\b", type = "ninth_one"),
  list(pattern = "\\btenth\\s*(?:one|image|picture)\\b", type = "tenth_one"),
  list(pattern = "\\beleventh\\s*(?:one|image|picture)\\b", type = "eleventh_one"),
  list(pattern = "\\btwelfth\\s*(?:one|image|picture)\\b", type = "twelfth_one"),
  list(pattern = "\\b(\\d{1,2})\\b(?!\\d)", type = "standalone")
)

extract_image_number <- function(text, type) {
  ordinal_map <- c(
    first = 1, second = 2, third = 3, fourth = 4, fifth = 5,
    sixth = 6, seventh = 7, eighth = 8, ninth = 9, tenth = 10,
    eleventh = 11, twelfth = 12
  )
  if (type %in% c("numbered", "colon", "dash", "standalone", "ordinal",
                  "image_num", "picture_num", "number_num")) {
    num <- as.integer(str_extract(text, "\\d+"))
  } else if (type == "next_one") {
    num <- -1
  } else if (str_ends(type, "_one")) {
    word <- str_remove(type, "_one")
    num <- ordinal_map[[word]]
    if (is.null(num)) num <- 0
  } else {
    num <- 0
  }
  list(num = num, text = text)
}

segment_description <- function(text, expected_segments = 12) {
  segments <- vector("list", expected_segments)
  for (i in seq_len(expected_segments)) {
    segments[[i]] <- character(0)
  }

  current_num <- 1
  last_pos <- 1
  matches <- list()

  for (pat in segment_patterns) {
    m <- str_match_all(text, regex(pat$pattern, ignore_case = TRUE))[[1]]
    if (nrow(m) > 0) {
      for (i in seq_len(nrow(m))) {
        start <- str_locate_all(text, regex(pat$pattern, ignore_case = TRUE))[[1]][i, 1]
        end <- str_locate_all(text, regex(pat$pattern, ignore_case = TRUE))[[1]][i, 2]
        match_text <- m[i, 1]
        info <- extract_image_number(match_text, pat$type)
        matches <- append(matches, list(list(start = start, end = end, num = info$num, text = info$text)))
      }
    }
  }

  matches <- matches[order(sapply(matches, function(x) x$start))]

  for (m in matches) {
    if (m$start > last_pos) {
      segments[[current_num]] <- c(segments[[current_num]], substr(text, last_pos, m$start - 1))
    }
    if (m$num == -1) {
      m$num <- current_num + 1
    }
    if (m$num >= 1 && m$num <= expected_segments) {
      segments[[m$num]] <- c(segments[[m$num]], m$text)
      current_num <- m$num
      last_pos <- m$end + 1
    }
  }

  if (last_pos <= nchar(text)) {
    segments[[current_num]] <- c(segments[[current_num]], substr(text, last_pos, nchar(text)))
  }

  return(sapply(segments, function(x) str_trim(paste(x, collapse = " "))))
}

load_tangram_boards <- function(board_file) {
  df <- read_csv(board_file)
  names(df)[names(df) == "roundNum"] <- "repetitionNum"
  names(df)[names(df) == "score"] <- "repetitionScore"
  
  # Create a named list manually instead of using deframe
  boards_dict <- list()
  
  df_processed <- df %>%
    group_by(gameid, repetitionNum) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      targets = pmap_chr(select(., starts_with("true")), ~paste(..., sep = ",")),
      selections = pmap_chr(select(., starts_with("sub")), ~paste(..., sep = ",")),
      key = paste(gameid, repetitionNum, sep = "_")
    ) %>%
    select(key, targets, selections, repetitionScore)
  
  # Iterate over rows to build the named list
  for (i in seq_len(nrow(df_processed))) {
    row <- df_processed[i, ]
    boards_dict[[row$key]] <- list(
      targets = str_split(row$targets, ",")[[1]],
      selections = str_split(row$selections, ",")[[1]],
      score = row$repetitionScore
    )
  }
  
  return(boards_dict)
}

load_subject_info <- function(subj_file) {
  read_csv(subj_file) %>%
    rename(gameid = gameID) %>%
    select(gameid, nativeEnglish)
}

apply_exclusions <- function(messages_df, boards_dict, subj_df) {
  incomplete_games <- messages_df %>%
    group_by(gameid) %>%
    summarise(rounds = n_distinct(roundNum)) %>%
    filter(rounds != 6) %>%
    pull(gameid)

  non_native_games <- subj_df %>%
    filter(nativeEnglish != "yes") %>%
    pull(gameid)

  low_accuracy_games <- unique(messages_df$gameid)
  low_acc <- vector("character", 0)

  for (gid in low_accuracy_games) {
    round_keys <- grep(paste0("^", gid, "_"), names(boards_dict), value = TRUE)
    scores <- sapply(round_keys, function(k) boards_dict[[k]]$score)
    if (length(scores) == 6 && sum(scores <= 8) >= 4) {
      low_acc <- c(low_acc, gid)
    }
  }

  # Create exclusion data frame and aggregate reasons
  exclusion_df <- bind_rows(
    tibble(gameid = incomplete_games, exclusion_reason = "incomplete game"),
    tibble(gameid = non_native_games, exclusion_reason = "non-native English-speaking participant"),
    tibble(gameid = low_acc, exclusion_reason = "low accuracy game"),
    tibble(gameid = "0574-6", exclusion_reason = "incomplete game (manual)")
  ) %>%
    group_by(gameid) %>%
    summarise(
      exclude = TRUE,
      exclusion_reason = paste(exclusion_reason, collapse = "; "),  # Combine reasons
      .groups = "drop"
    )

  # Add games not in exclusion_df with exclude = FALSE
  all_games <- unique(messages_df$gameid)
  missing_games <- setdiff(all_games, exclusion_df$gameid)
  if (length(missing_games) > 0) {
    exclusion_df <- bind_rows(
      exclusion_df,
      tibble(
        gameid = missing_games,
        exclude = FALSE,
        exclusion_reason = "NA"
      )
    )
  }

  return(exclusion_df)
}

# Helper function to map numeric values to letters (1=A, 2=B, ..., 12=L)
num_to_letter <- function(num) {
  letters <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")
  if (is.na(num) || num == "" || !is.character(num) || !grepl("^[0-9]+$", num) || as.integer(num) < 1 || as.integer(num) > 12) {
    return("NA")
  }
  letters[as.integer(num)]
}

process_game_messages <- function(messages_df, id_cols, boards_dict, exclusion_df) {
  options <- LETTERS[1:12]
  option_set <- paste(options, collapse = ";")
  result <- list()
  global_trial_num <- 0
  last_game_id <- ""

  grouped <- messages_df %>%
    group_by(across(all_of(id_cols))) %>%
    summarise(contents = paste(na.omit(contents), collapse = " "),
              msgTime = first(msgTime), .groups = "drop")

  for (i in seq_len(nrow(grouped))) {
    row <- grouped[i, ]
    game_id <- row[[id_cols[1]]]
    rep_num <- if ("roundNum" %in% id_cols) row$roundNum else 1
    time_stamp <- if (!is.na(row$msgTime)) as.numeric(row$msgTime) else NA

    if (game_id != last_game_id) {
      global_trial_num <- 0
      last_game_id <- game_id
    }

    segments <- segment_description(row$contents)
    key <- paste(game_id, rep_num, sep = "_")
    if (!key %in% names(boards_dict)) {
      log_info("Skipping key {key}: not found in boards_dict")
      next
    }

    # Access targets and selections without [[1]] and log their content
    targets <- boards_dict[[key]]$targets
    selections <- boards_dict[[key]]$selections
    log_info("Processing key {key}: targets = {paste(targets, collapse=', ')}, selections = {paste(selections, collapse=', ')}")

    # Check lengths
    if (length(targets) < length(segments) || length(selections) < length(segments)) {
      log_warn("Length mismatch for key {key}: segments={length(segments)}, targets={length(targets)}, selections={length(selections)}")
    }

    exclude_info <- exclusion_df %>% filter(gameid == game_id)
    exclude <- if (nrow(exclude_info) > 0) exclude_info$exclude else FALSE
    exclusion_reason <- if (nrow(exclude_info) > 0) exclude_info$exclusion_reason else "NA"

    for (j in seq_along(segments)) {
      global_trial_num <- global_trial_num + 1

      # Use j to index targets and selections, with fallback to "NA" if index exceeds length
      target_val <- if (j <= length(targets)) targets[j] else NA
      selection_val <- if (j <= length(selections)) selections[j] else NA

      #messages
      result[[length(result) + 1]] <- tibble(
        dataset_id = "hawkins2020_characterizing_uncued",
        condition_label = "unconstrained",
        full_cite = "Hawkins, R. D., Frank, M. C., & Goodman, N. D. (2020)...",
        short_cite = "Hawkins et al. (2020)",
        language = "English",
        game_id = game_id,
        player_id = paste0(game_id, "_describer"),
        trial_num = global_trial_num,
        rep_num = rep_num,
        room_num=1,
        stage_num=1,
        age=as.numeric(NA), # do age later
        gender=as.character(NA), #do gender later
        role = "describer",
        target = num_to_letter(target_val),  # Map target to letter
        message_number = j,
        text = ifelse(segments[j] == "", "NA", segments[j]),
        choice_id = "NA",
        time_stamp = time_stamp,
        option_set = option_set,
        group_size = 2,
        structure = "thick",
        exclude = exclude,
        exclusion_reason = exclusion_reason,
        action_type = "message",
        message_irrelevant = FALSE
      )

      #choices
      result[[length(result) + 1]] <- tibble(
        dataset_id = "hawkins2020_characterizing_uncued",
        condition_label = "unconstrained",
        full_cite = "Hawkins, R. D., Frank, M. C., & Goodman, N. D. (2020)...",
        short_cite = "Hawkins et al. (2020)",
        language = "English",
        game_id = game_id,
        player_id = paste0(game_id, "_matcher"),
        trial_num = global_trial_num,
        rep_num = rep_num,
        room_num=1,
        stage_num=1,
        age=as.numeric(NA), # do age later,
        gender=as.character(NA),
        role = "matcher",
        target = num_to_letter(target_val),  # Map target to letter
        message_number = NA,
        text = "NA",
        choice_id = num_to_letter(selection_val),  # Map selection to letter
        time_stamp = NA,
        option_set = option_set,
        group_size = 2,
        structure = "thick",
        exclude = exclude,
        exclusion_reason = exclusion_reason,
        action_type = "selection",
        message_irrelevant = NA
      )
    }
  }

  bind_rows(result)
}

load_and_process <- function(input_file, board_file, subj_file, output_file) {
  messages_df <- read_csv(input_file) %>%
    rename(role = sender, msgTime = time)

  boards_dict <- load_tangram_boards(board_file)
  subj_df <- load_subject_info(subj_file)
  exclusion_df <- apply_exclusions(messages_df, boards_dict, subj_df)

  if (!"roundNum" %in% colnames(messages_df)) {
    messages_df$roundNum <- 1
  }

  id_cols <- c("gameid", "roundNum")
  result_df <- process_game_messages(messages_df, id_cols, boards_dict, exclusion_df)

  write_csv(result_df, output_file)
  result_df
}

# Example execution
INPUT_FILE <- "import/hawkins_dynamics_free/raw_data/rawUnconstrainedMessages.csv" 
BOARD_FILE <- "import/hawkins_dynamics_free/raw_data/tangramsFinalBoards.csv"
SUBJ_FILE <- "import/hawkins_dynamics_free/raw_data/tangrams-subject_information.csv"
OUTPUT_FILE <- "import/hawkins_dynamics_free/segmented_data.csv"

cat("Starting tangrams description segmentation for refbank-import...\n")
result <- load_and_process(INPUT_FILE, BOARD_FILE, SUBJ_FILE, OUTPUT_FILE)
cat("Segmentation completed successfully!\n")
print(head(result, 24))

# nolint end

source(here("validate.R"))

test <- read_csv(here(OUTPUT_FILE)) |> mutate(age=as.numeric(age), gender=as.character(gender))
validate_dataset(test, write=T)
