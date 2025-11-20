library(here)
library(tidyverse)
library(jsonlite)
###### read in data from github
url <- "https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/"

pilot_b_rounds <- read_csv(str_c(url, "data/pilotB/rounds.csv"))
pilot_c_rounds <- read_csv(str_c(url, "data/pilotC/rounds.csv"))

ParseJSONColumn <- function(x) {
  str_c("[ ", str_c(x, collapse = ",", sep = " "), " ]") %>%
    fromJSON(flatten = T)
}
pilot_chat <- pilot_b_rounds |>
  bind_rows(pilot_c_rounds) |>
  mutate(data.chat = ifelse(is.na(data.chat), "{}", data.chat)) %>%
  rename(row_id = `_id`) %>%
  mutate(data.chat = map(data.chat, .f = ParseJSONColumn)) %>%
  unnest(data.chat) %>%
  select(-data.target, -ends_with("response"), -ends_with("_correct"), -ends_with("time")) %>%
  rename_with(~ gsub("data.", "", .x, fixed = TRUE)) |>
  mutate(
    spellchecked = text,
    is.chitchat = NA,
    numPlayers = ifelse(is.na(numPlayers), 3, numPlayers), # from looking at data
    rotate = str_c(as.character(numPlayers), "_rotate")
  )


pilot_round_results <- pilot_b_rounds |>
  bind_rows(pilot_c_rounds) |>
  rename_with(~ gsub("data.", "", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("room", "player", .x, fixed = T)) %>%
  rename_with(~ gsub("player", "player_", .x, fixed = T)) %>%
  rename_with(~ gsub("correct", "_correct", .x, fixed = T)) %>%
  rename_with(~ gsub("response", "_response", .x, fixed = T)) %>%
  rename_with(~ gsub("time", "_time", .x, fixed = T)) %>%
  select(-chat) %>%
  gather(key, value, starts_with("player")) %>%
  separate(key, into = c("blah", "playerId", "info")) %>%
  spread(info, value) %>%
  select(-blah) %>%
  mutate(
    tangram = gsub("/experiment/tangram_", "", target, fixed = TRUE),
    tangram = gsub(".png", "", tangram, fixed = TRUE)
  ) %>%
  mutate(
    correct = as.logical(correct),
    time = as.numeric(time) / 1000
  ) %>%
  filter(!is.na(correct)) %>%
  filter(playerId != speaker) |>
  mutate(numPlayers = ifelse(is.na(numPlayers), 3, numPlayers)) |> # from looking at data
  mutate(rotate = str_c(as.character(numPlayers), "_rotate"))


pilot_include <- pilot_round_results |>
  select(gameId, repNum, rotate) |>
  mutate(include = F)
