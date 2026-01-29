library(tidyverse)
library(here)

# on the command line from the raw_data/Transcripte folder, do this to go from raw-raw to plaintext

# soffice --headless --convert-to docx *.DOC
# soffice --headless --convert-to docx *.doc
# soffice --headless --convert-to docx *.EIT
# find ./ -iname "*.docx" -type f -exec sh -c 'pandoc -f docx -w plain  "${0}" -o "${0%}.txt"' {} \;
# mv *.txt ../plain_transcript

# and then hand clean up to get rid of the things that don't look like transcripts

raw_data_loc <- "import/bangerter2000_swissgerman/raw_data/plain_transcript"


full <- here(raw_data_loc) |>
  list.files() |>
  map(\(f) {
    read_tsv(here(raw_data_loc, f), skip = 1, col_names = "col") |>
      mutate(source = f)
  }) |>
  bind_rows() |>
  mutate(col = str_replace_all(col, "\\|", "") |> str_replace_all("\\s+", " ")) |>
  mutate(rep = ifelse(str_detect(col, "Durchgang"), col, NA) |> str_replace("Durchgang", "")) |>
  fill(rep) |>
  mutate(rep = as.numeric(rep)) |>
  filter(!str_detect(col, "Durchgang")) |>
  filter(!str_detect(col, "---------------")) |>
  mutate(col = str_replace_all(col, "\\(.*\\)", "") |> trimws()) |>
  separate_wider_delim(cols = "col", too_many = "merge", too_few = "align_start", delim = " ", names = c("lineno", "speaker", "message"), cols_remove = F) |>
  mutate(
    lineno = ifelse(speaker %in% c("M", "D"), lineno, NA),
    speaker = ifelse(speaker %in% c("M", "D"), speaker, NA),
    message = ifelse(speaker %in% c("M", "D"), message, col)
  ) |>
  fill(lineno, speaker) |>
  mutate(lineno = as.integer(lineno)) |>
  group_by(source, rep, lineno, speaker) |>
  summarize(message = str_c(message, collapse = " ")) |>
  filter(speaker %in% c("M", "D")) |>
  filter(!is.na(lineno)) |>
  mutate(
    message = str_replace_all(message, "\\[", ""),
    message = str_replace_all(message, "=", ""),
    message = trimws(message)
  ) |>
  filter(message != "")



ready_for_segment <- full |>
  ungroup() |>
  mutate(
    game = as.factor(source) |> as.numeric(), grid = rep, targetPosition = "",
    role = ifelse(speaker == "D", "describer", "matcher"),
    person = case_when(
      rep %% 2 == 0 & speaker == "D" ~ "A",
      rep %% 2 == 0 & speaker == "M" ~ "B",
      rep %% 2 == 1 & speaker == "D" ~ "B",
      rep %% 2 == 1 & speaker == "M" ~ "A",
    ),
    message_original = message
  ) |>
  select(game, grid, targetPosition, role, person, message_original)
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

# NOte that roles swap after each round!

write_csv(ready_for_segment |> filter(game == 1), here("segmentation/sample/bangerter2000.csv"))
write_csv(ready_for_segment |> filter(game != 1), here("segmentation/remainder/bangerter2000.csv"))
