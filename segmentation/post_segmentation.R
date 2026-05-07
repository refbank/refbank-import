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
  write_csv(here("import/dahan/segmented_transcript.csv"))


# TODO below here!!!!
vb_french <- read_xlsx(here("segmentation/segmentation_sample_veronica.xlsx"),
  col_types = "text", sheet = "bangerter2020"
) |>
  rename(targetPosition_vb = targetPosition)

m_french <- read_xlsx(here("segmentation/worker_segmentation_sample_blank.xlsx"),
  col_types = "text", sheet = "bangerter2020"
) |>
  janitor::clean_names() |>
  rename(targetPosition_w = target_position_1_8)

french <- vb_french |>
  left_join(m_french) |>
  mutate(
    target_agree = targetPosition_vb == targetPosition_w
  )

nrow(vb_french) # 228
nrow(m_french) # 240
nrow(french) # 238

french |>
  group_by(role) |>
  summarize(
    target = sum(target_agree, na.rm = T) / n()
  )

View(french)


vb_german <- read_xlsx(here("segmentation/segmentation_sample_veronica.xlsx"),
  col_types = "text", sheet = "bangerter2000"
) |>
  rename(targetPosition_vb = targetPosition) # |>
# mutate(message_id_num_orig=row_number()) |>
# mutate(message_id_num=case_when(
#   message_id_num_orig>91 ~ message_id_num_orig-9,
#   message_id_num_orig>83 ~ message_id_num_orig-4,
#   message_id_num_orig>64 ~ message_id_num_orig-3,
#   message_id_num_orig>2 ~ message_id_num_orig-2,
#   message_id_num_orig>1 ~ message_id_num_orig-1,
#   T ~ message_id_num_orig
# ))

m_german <- read_xlsx(here("segmentation/worker_segmentation_sample_blank.xlsx"),
  col_types = "text", sheet = "bangerter2000"
) |>
  janitor::clean_names() |>
  rename(targetPosition_w = target_position_1_8) |>
  mutate(message_id_num = as.numeric(message_id_num))


german <- vb_german |>
  full_join(m_german) |>
  mutate(
    target_agree = targetPosition_vb == targetPosition_w
  ) |>
  filter(message != "And", message != "and", message != "Mhm", message != "And?")


nrow(vb_german) # 214
nrow(m_german) # 240
nrow(german) # 238

german |>
  group_by(role) |>
  summarize(
    target = sum(target_agree, na.rm = T) / n()
  )
