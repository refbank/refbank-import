library(refbankr)
library(here)
library(tidyverse)
# dataset check of a given dataset

# should work either with data from refbankr or with harmonized data


get_data_local <- function(dataset, data_path) {
  # return a named list of trials, selections, messages, conditions
  list(
    trials = read_csv(file.path(data_path, dataset, "trials.csv"), show_col_types = FALSE, guess_max = Inf),
    selections = read_csv(file.path(data_path,dataset, "selections.csv"), show_col_types = FALSE, guess_max = Inf),
    messages = read_csv(file.path(data_path,dataset, "messages.csv"), show_col_types = FALSE, guess_max = Inf),
    conditions = read_csv(file.path(data_path,dataset, "conditions.csv"), show_col_types = FALSE, guess_max = Inf)
  )
}

get_data_redivis <- function(dataset, version) {
  # return a named list of trials, selections, messages, conditions
  list(
    trials = get_trials(version = version, datasets = dataset),
    selections = get_selections(version = version, datasets = dataset),
    messages = get_messages(version = version, datasets = dataset),
    conditions = get_conditions(version = version, datasets = dataset)
  )
}

integer_breaks <- function(x) {
  # pretty() breaks can land on non-integers (e.g. round_num 1-96 -> 12.5);
  # round_num is always a whole number, so round to the nearest integers instead
  unique(round(pretty(x)))
}

plot_data <- function(data) {
  # should do a stat_summary( mean_ci_boot) with geom_point range and geom_line
  # expects columns: round_num, condition_label, stage_num, value
  # lines are grouped by stage so they don't connect across stage boundaries
  # (a new stage means a new partner, so continuity across stages is misleading)
  data |>
    filter(!is.na(value)) |>
    ggplot(aes(x = round_num, y = value, color = condition_label, group = interaction(condition_label, stage_num))) +
    theme_bw() +
    stat_summary(fun.data = mean_cl_boot, geom = "pointrange") +
    stat_summary(fun = mean, geom = "line") +
    scale_x_continuous(breaks = integer_breaks)
}

check_dataset <- function(dataset, version = NULL, local = F, datapath = NULL) {
  # get data from relevant source
  if (local) {
    data <- get_data_local(dataset, datapath)
  } else {
    data <- get_data_redivis(dataset, if (is.null(version)) "current" else version)
  }

  # normalize join-key types: some datasets have empty selections/messages
  # tables, which makes readr guess a different type than the populated tables
  data$trials <- data$trials |> mutate(trial_id = as.character(trial_id), condition_id = as.character(condition_id))
  data$selections <- data$selections |> mutate(trial_id = as.character(trial_id))
  data$messages <- data$messages |> mutate(trial_id = as.character(trial_id))
  data$conditions <- data$conditions |> mutate(condition_id = as.character(condition_id))

  # apply all originally occurring exclusions
  trials <- data$trials |>
    filter(is.na(exclude) | !exclude) |>
    left_join(data$conditions |> select(condition_id, condition_label), by = "condition_id")

  messages <- data$messages |>
    filter(is.na(message_irrelevant) | !message_irrelevant)

  # summarize and plot accuracy per round (in each condition)
  selections_by_trial <- trials |>
    left_join(data$selections |> select(trial_id, selected_image), by = "trial_id")

  # some conditions have multiple matchers per trial (one selection row each),
  # so count distinct trials with a selection rather than selection rows
  n_trials_with_selections <- selections_by_trial |>
    group_by(condition_label) |>
    summarize(n_trials_with_selections = n_distinct(trial_id[!is.na(selected_image)]), .groups = "drop")

  accuracy_plot_data <- selections_by_trial |>
    mutate(value = as.numeric(as.character(selected_image) == as.character(target_image))) |>
    select(round_num, condition_label, stage_num, value)

  if (sum(n_trials_with_selections$n_trials_with_selections) > 0) {
    print(plot_data(accuracy_plot_data) +
      labs(title = paste(dataset, "- accuracy by round"), x = "round", y = "proportion correct") +
      ylim(0, 1))
  }

  # summarize and plot mean # words from describer per round (in each condition)
  words_per_trial <- messages |>
    filter(role == "describer") |>
    mutate(n_words = str_count(text, "\\S+")) |>
    group_by(trial_id) |>
    summarize(n_words = sum(n_words), .groups = "drop")

  words_by_trial <- trials |>
    left_join(words_per_trial, by = "trial_id")

  n_trials_with_messages <- words_by_trial |>
    group_by(condition_label) |>
    summarize(n_trials_with_messages = sum(!is.na(n_words)), .groups = "drop")

  words_plot_data <- words_by_trial |>
    mutate(value = replace_na(n_words, 0)) |>
    select(round_num, condition_label, stage_num, value)

  if (sum(n_trials_with_messages$n_trials_with_messages) > 0) {
    print(plot_data(words_plot_data) +
      labs(title = paste(dataset, "- describer word count by round"), x = "round", y = "# words") +
      expand_limits(y = 0) +
      scale_y_continuous(breaks = integer_breaks))
  }

  # summary stats to print for each condition

  # number of images (total), option set size (range)
  images_total <- trials |>
    mutate(image = str_split(image_options, ";")) |>
    unnest(image) |>
    distinct(condition_label, image) |>
    count(condition_label, name = "n_images_total")

  option_set_size <- trials |>
    mutate(option_set_size = image_options |> str_split(";") |> lengths()) |>
    group_by(condition_label) |>
    summarize(
      min_option_set_size = min(option_set_size),
      max_option_set_size = max(option_set_size),
      .groups = "drop"
    )

  trials_per_round <- trials |>
    count(condition_label, game_id, room_num, round_num, name = "n_trials_in_round") |>
    group_by(condition_label) |>
    summarize(
      mean_trials_per_round = mean(n_trials_in_round),
      min_trials_per_round = min(n_trials_in_round),
      max_trials_per_round = max(n_trials_in_round),
      .groups = "drop"
    )

  # number of games
  # mean, min - max players / game
  # mean, min - max rounds / game
  # mean, min - max trials / game
  # total trials in condition
  players_per_game <- bind_rows(
    trials |> distinct(condition_label, game_id, player_id = as.character(describer)),
    trials |>
      mutate(matcher = str_split(matchers, ";")) |>
      unnest(matcher) |>
      distinct(condition_label, game_id, player_id = as.character(matcher))
  ) |>
    # a trial with no resolved matcher (e.g. an unmatched role-inference join)
    # leaves matchers NA -- that's a missing player, not an extra one
    filter(!is.na(player_id)) |>
    distinct(condition_label, game_id, player_id) |>
    count(condition_label, game_id, name = "n_players")

  rounds_per_game <- trials |>
    distinct(condition_label, game_id, round_num) |>
    count(condition_label, game_id, name = "n_rounds")

  trials_per_game <- trials |>
    count(condition_label, game_id, name = "n_trials")

  per_game <- trials_per_game |>
    left_join(rounds_per_game, by = c("condition_label", "game_id")) |>
    left_join(players_per_game, by = c("condition_label", "game_id"))

  summary_stats <- per_game |>
    group_by(condition_label) |>
    summarize(
      n_games = n(),
      total_trials = sum(n_trials),
      mean_players = mean(n_players), min_players = min(n_players), max_players = max(n_players),
      mean_rounds = mean(n_rounds), min_rounds = min(n_rounds), max_rounds = max(n_rounds),
      mean_trials = mean(n_trials), min_trials = min(n_trials), max_trials = max(n_trials),
      .groups = "drop"
    ) |>
    left_join(images_total, by = "condition_label") |>
    left_join(option_set_size, by = "condition_label") |>
    left_join(trials_per_round, by = "condition_label") |>
    left_join(n_trials_with_selections, by = "condition_label") |>
    left_join(n_trials_with_messages, by = "condition_label") |>
    left_join(
      data$conditions |> select(
        condition_label, group_size, population, prior_relationship, partner_constancy,
        role_constancy, confederates, modality, feedback, backchannel, language
      ),
      by = "condition_label"
    )

  cat("\n\n------", dataset, "summary ------\n")

  for (i in seq_len(nrow(summary_stats))) {
    row <- summary_stats[i, ]
    cat("\n------ condition:", row$condition_label, "------\n")
    print(paste("group_size:", row$group_size))
    print(paste("population:", row$population))
    print(paste("prior_relationship:", row$prior_relationship))
    print(paste("partner_constancy:", row$partner_constancy))
    print(paste("role_constancy:", row$role_constancy))
    print(paste("confederates:", row$confederates))
    print(paste("modality:", row$modality))
    print(paste("feedback:", row$feedback))
    print(paste("backchannel:", row$backchannel))
    print(paste("language:", row$language))
    print(paste("# of games:", row$n_games))
    print(paste("total trials:", row$total_trials))
    print(paste0(
      "players / game -- mean: ", round(row$mean_players, 2),
      ", min: ", row$min_players, ", max: ", row$max_players
    ))
    print(paste0(
      "rounds / game -- mean: ", round(row$mean_rounds, 2),
      ", min: ", row$min_rounds, ", max: ", row$max_rounds
    ))
    print(paste0(
      "trials / game -- mean: ", round(row$mean_trials, 2),
      ", min: ", row$min_trials, ", max: ", row$max_trials
    ))
    print(paste0(
      "trials / round -- mean: ", round(row$mean_trials_per_round, 2),
      ", min: ", row$min_trials_per_round, ", max: ", row$max_trials_per_round
    ))
    print(paste("# of images (total):", row$n_images_total))
    print(paste0(
      "option set size -- min: ", row$min_option_set_size,
      ", max: ", row$max_option_set_size
    ))
    print(paste0(
      "trials with selections: ", row$n_trials_with_selections, " / ", row$total_trials
    ))
    print(paste0(
      "trials with messages: ", row$n_trials_with_messages, " / ", row$total_trials
    ))
  }

  invisible(NULL)
}
