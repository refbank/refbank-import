library(tidyverse)
library(redivis)


# option dataset stats
con <- redivis$user("mcfrank")$dataset("refbank:2zy7")
trials <- con$table("trials:zkj2")$to_tibble()
messages <- con$table("messages:2q18")$to_tibble()
choices <- con$table("choices:s1zj")$to_tibble()
conditions <- con$table("conditions:kk1e")$to_tibble()
sims <- con$table("cosine_similarities:cp0s")$to_tibble()

# Condition metadata
# Game
# Rep
# N trials (for weighting)
# Word count
# Accuracy
# RT
# … (embedding outcomes)

words <- messages |> filter(role=="describer") |> 
  group_by(trial_id, dataset_id) |>
  summarise(total_num_words = sum(lengths(str_split(text, " ")), na.rm = TRUE))

accuracy <- choices |> left_join(trials) |> 
  group_by(trial_id, dataset_id) |>
  summarise(overall_accuracy = sum(choice_id == target, na.rm = TRUE) / n())

rt <- choices |> left_join(trials) |>
  group_by(trial_id, dataset_id) |>
  summarise(mean_rt = mean(time_stamp, na.rm = TRUE))



sim_summary <- sims |> filter(sim_type %in% c("to_next", "diff", "idiosyncrasy")) |> 
  mutate(rep_num=ifelse(is.na(rep_num), later, rep_num)) |> 
  group_by(dataset_id, game_id, rep_num, sim_type, stage_num) |> 
  summarize(mean_sim=mean(sim, na.rm=TRUE)) |> 
  pivot_wider(names_from=sim_type, values_from=mean_sim)

diverge <- sims |> filter(sim_type %in% c("diverge")) |> 
  select(game_id=game_id_1, sim, dataset_id, rep_num, stage_num) |> 
  bind_rows(sims |> filter(sim_type %in% c("diverge")) |> 
              select(game_id=game_id_2, sim, dataset_id, rep_num, stage_num)) |> 
  group_by(game_id, dataset_id, rep_num, stage_num) |> 
  summarize(diverge=mean(sim, na.rm=T)) |> 
  mutate(game_id=as.numeric(game_id))

trial_sum <- trials |> left_join(words) |> left_join(accuracy) |> left_join(rt) |> 
  group_by(game_id, rep_num, stage_num, option_size, condition_id, dataset_id) |> 
  summarize(words=sum(total_num_words, na.rm=T),
            accuracy=mean(overall_accuracy, na.rm=T),
            rt=mean(mean_rt, na.rm=T),
            trials=n()) |> 
  left_join(sim_summary) |> left_join(diverge) |> left_join(conditions)

         

theme_set(theme_bw())

x <- "option_set_size"

y <- "trials_per_game"



ggplot(all, aes(x = .data[[x]], y = .data[[y]])) +
  geom_point(aes(color=dataset_id))
