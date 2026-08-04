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

  checks <- refbankr::summarize_dataset_checks(
    trials = data$trials,
    selections = data$selections,
    messages = data$messages,
    conditions = data$conditions
  )
  summary_stats <- checks$summary_stats
  accuracy_plot_data <- checks$accuracy_plot_data
  words_plot_data <- checks$words_plot_data

  if (sum(summary_stats$n_trials_with_selections) > 0) {
    print(plot_data(accuracy_plot_data) +
      labs(title = paste(dataset, "- accuracy by round"), x = "round", y = "proportion correct") +
      ylim(0, 1))
  }

  if (sum(summary_stats$n_trials_with_messages) > 0) {
    print(plot_data(words_plot_data) +
      labs(title = paste(dataset, "- describer word count by round"), x = "round", y = "# words") +
      expand_limits(y = 0) +
      scale_y_continuous(breaks = integer_breaks))
  }

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
