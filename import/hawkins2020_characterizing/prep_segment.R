library(tidyverse)
library(here)


INPUT_FILE <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/message/rawUnconstrainedMessages.csv"

BOARD_FILE <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/finalBoard/tangramsFinalBoards.csv"

SUBJ_FILE <- "https://raw.githubusercontent.com/hawkrobe/tangrams/master/data/tangrams_unconstrained/turk/tangrams-subject_information.csv"


raw_messages <- read_csv(INPUT_FILE)

ready_for_segment <- raw_messages |>
  mutate(
    game = as.factor(gameid) |> as.numeric(),
    grid = roundNum,
    targetPosition = "",
    role = ifelse(sender == "director", "describer", "matcher"),
    person = ifelse(role == "describer", "A", "B"), # we know that roles don't swap
    message = contents,
    message_id_num=row_number()
  ) |>
  select(game, grid, targetPosition, role, message_id_num, message)

# specs
# game (numeric from 1)
# grid (= round, numeric from 1)
# targetPosition blank
# role -- describer/matcher (if known)
# person A/B
# message

# then save out as
# sample.csv
# remainder.csv

write_csv(ready_for_segment, here("segmentation/remainder/hawkins2020.csv"))
