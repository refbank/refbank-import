library(tidyverse)
library(readxl)
library(here)

# Pull dahan

m_dahan_sample <- read_xlsx(here("segmentation/worker_segmentation_sample.xlsx"),
  col_types = "text", sheet = "dahan"
) |>
  janitor::clean_names() |>
  rename(role_w = role_d_for_describer_m_for_matcher, grid_w = grid_1_16, targetPosition_w = target_position_1_3) |>
  mutate(message_id_num = as.numeric(message_id_num))

m_dahan_remainder <- read_xlsx(here("segmentation/worker_segmentation_remainder.xlsx"),
  col_types = "text", sheet = "dahan"
) |>
  janitor::clean_names() |>
  rename(role_w = role_d_for_describer_m_for_matcher, grid_w = grid_1_16, targetPosition_w = target_position_1_3) |>
  mutate(message_id_num = as.numeric(message_id_num))

m_dahan <- m_dahan_sample |>
  bind_rows(m_dahan_remainder) |>
  write_csv(here("import/dahan2023_collaboration/raw_data/segmented_transcript.csv"))


m_hawkins <- read_xlsx(here("segmentation/worker_segmentation_remainder.xlsx"),
  col_types = "text", sheet = "hawkins"
) |>
  janitor::clean_names() |>
  rename(target_position = target_position_1_12) |>
  mutate(target_position = case_when(
    message == "swaddled baby is my #9" ~ "9",
    T ~ target_position
  )) |> # typo probably
  write_csv(here("import/hawkins2020_characterizing_uncued/raw_data/segmented_transcript.csv"))


m_french_sample <- read_xlsx(here("segmentation/worker_segmentation_sample.xlsx"),
  col_types = "text", sheet = "bangerter2020"
) |>
  janitor::clean_names() |>
  rename(targetPosition_w = target_position_1_8)


m_french_remainder <- read_xlsx(here("segmentation/worker_segmentation_remainder.xlsx"),
  col_types = "text", sheet = "bangerter2020"
) |>
  janitor::clean_names() |>
  rename(targetPosition_w = target_position_1_8)

french_original <- read_csv(here("segmentation/sample/bangerter2020.csv")) |>
  bind_rows(read_csv(here("segmentation/remainder/bangerter2020.csv"))) |>
  select(-targetPosition, -role)

m_french <- m_french_sample |>
  bind_rows(m_french_remainder) |>
  rename(message_english = message) |>
  mutate(game = as.numeric(game), grid = as.numeric(grid), message_id_num = as.numeric(message_id_num)) |>
  left_join(french_original)

# where messages got split up need to apply that to the untranslated part
need_splits <- m_french |>
  group_by(message_id_num) |>
  mutate(n = n()) |>
  filter(n > 1)

split_fr <- need_splits |>
  select(-message_english, -targetPosition_w) |>
  distinct() |>
  rowwise() |>
  mutate(phrases = str_split(message, "(?<=\\.)\\s*")) |>
  unnest(phrases) |>
  filter(phrases != "") |>
  group_by(message_id_num) |>
  mutate(message_part_no = row_number())
split_en <- need_splits |>
  select(-message) |>
  group_by(message_id_num) |>
  mutate(message_part_no = row_number())

split_fr |>
  full_join(split_en) |>
  arrange(message_id_num, message_part_no) |>
  write_csv(here("segmentation/messages_to_split_bangerter2020.csv"))

m_french_all <- m_french |>
  group_by(message_id_num) |>
  mutate(n = n()) |>
  filter(n == 1) |>
  bind_rows(read_csv(here("segmentation/bangerter2020_split_messages.csv"))) |>
  mutate(message = case_when(
    is.na(message) ~ phrases,
    T ~ message
  )) |>
  arrange(message_id_num, message_part_no) |>
  select(-message_english, -n, -phrases) |>
  write_csv(here("import/bangerter2020_lexical/raw_data/segmented_transcript.csv"))
# first assumption -- split by sentences!


### not done below here!

m_german_sample <- read_xlsx(here("segmentation/worker_segmentation_sample.xlsx"),
  col_types = "text", sheet = "bangerter2000"
) |>
  janitor::clean_names() |>
  rename(targetPosition_w = target_position_1_8) |>
  mutate(message_id_num = as.numeric(message_id_num))

m_german_remainder <- read_xlsx(here("segmentation/worker_segmentation_remainder.xlsx"),
  col_types = "text", sheet = "bangerter2000"
) |>
  janitor::clean_names() |>
  rename(targetPosition_w = target_position_1_8) |>
  mutate(message_id_num = as.numeric(message_id_num))


german_original <- read_csv(here("segmentation/sample/bangerter2000.csv")) |>
  bind_rows(read_csv(here("segmentation/remainder/bangerter2000.csv"))) |>
  select(-targetPosition, -role)

m_german <- m_german_sample |>
  bind_rows(m_german_remainder) |>
  rename(message_english = message) |>
  mutate(game = as.numeric(game), grid = as.numeric(grid), message_id_num = as.numeric(message_id_num)) |>
  left_join(german_original)

# where messages got split up need to apply that to the untranslated part
need_splits <- m_german |>
  group_by(message_id_num) |>
  mutate(n = n()) |>
  filter(n > 1)

split_de <- need_splits |>
  select(-message_english, -targetPosition_w) |>
  distinct() |>
  rowwise() |>
  mutate(phrases = str_split(message_original, "(?<=\\.)\\s*")) |>
  unnest(phrases) |>
  filter(phrases != "") |>
  group_by(message_id_num) |>
  mutate(message_part_no = row_number())

split_en <- need_splits |>
  select(-message_original) |>
  group_by(message_id_num) |>
  mutate(message_part_no = row_number())

split_de |>
  full_join(split_en) |>
  arrange(message_id_num, message_part_no) |>
  write_csv(here("segmentation/messages_to_split_bangerter2000.csv"))

split_messages <- read_csv(here("segmentation/bangerter2000_split_messages.csv")) |>
  group_by(message_id_num) |>
  mutate(message_part_no = row_number())


m_german_all <- m_german |>
  group_by(message_id_num) |>
  mutate(n = n()) |>
  filter(n == 1) |>
  bind_rows(read_csv(here("segmentation/bangerter2000_split_messages.csv"))) |>
  mutate(message = case_when(
    is.na(message_original) ~ phrases,
    T ~ message_original
  )) |>
  arrange(message_id_num, message_part_no) |>
  filter(!game %in% c(16, 17, 18)) |> # these are duplicates!!!
  select(-message_english, -message_original, -n, -phrases) |>
  write_csv(here("import/bangerter2000_swissgerman/raw_data/segmented_transcript.csv"))

# hawkins

hawkins_fmri <- read_csv(here("segmentation/Segmentation hawkins_fmri - Sheet1.csv"))
