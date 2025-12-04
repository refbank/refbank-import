library(tidyverse)
library(here)

pilotb <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/pilotB/player-inputs.csv"))
pilotc <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/pilotC/player-inputs.csv"))
study_3 <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/study3/player-inputs.csv"))
study_2c <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/study2c/player-inputs.csv"))
study_2b <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/study2b/player-inputs.csv")) |>
  mutate(data.age = as.character(data.age))
study_2a <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/study2a/player-inputs.csv"))
study_1b <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/study1b/player-inputs.csv"))
study_1a <- read_csv(url("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/refs/heads/main/data/study1a/player-inputs.csv")) |>
  mutate(data.age = as.character(data.age))


female_equivalent <- c("female", "Female", "F", "f", "femal", "Femal", "Woman", "woman", "FEMALE", "trans female", "demigirl")
male_equivalent <- c("Male", "male", "MALE", "Malw", "Man", "Cis man", "M", "m", "mALE", "man", "<a;e")
nb_equivalent <- c("Nonbinary", "NB", "Genderqueer", "Non-Binary", "Non-binary", "nonbinary", "Agender", "Genderfluid")

# goal for language is that a case_insensitive str_detect() would work
english_equivalent <- c(
  "english", "Englishglish", "Englsih", "Britiah", "eng", "ENGLISH", "bristish", "Ennlish", "british",
  "Engligh", "British", "Engish", "Enligh", "Engliah"
)
demogs <- bind_rows(
  study_3,
  study_2c,
  study_2b,
  study_2a,
  study_1b,
  study_1a
) |>
  mutate(
    age = as.numeric(data.age),
    gender = case_when(
      data.gender %in% female_equivalent ~ "female",
      data.gender %in% male_equivalent ~ "male",
      data.gender %in% nb_equivalent ~ "nonbinary",
    ),
    native_language = case_when(
      data.language %in% english_equivalent ~ "English",
      data.language == "MALE" ~ NA,
      T ~ data.language
    ),
    race.white = ifelse(!is.na(data.raceWhite), "White", ""),
    race.black = ifelse(!is.na(data.raceBlack), "Black", ""),
    race.asian = ifelse(!is.na(data.raceAsian), "Asian", ""),
    race.native = ifelse(!is.na(data.raceNative), "Native American", ""),
    race.islander = ifelse(!is.na(data.raceIslander), "Pacific Islander", ""),
    race.hispanic = ifelse(!is.na(data.raceHispanic), "Hispanic", ""),
    race = str_c(race.white, race.black, race.asian, race.native, race.islander, race.hispanic, sep = " "),
    race = str_squish(race),
    race = ifelse(race == "", NA, race),
    education = case_when(
      data.education=="master"~"advanced-degree",
      data.education=="bachelor"~"bachelors",
      data.education=="someCollege"~"some-college",
      data.education=="highSchool"~"high-school",
      data.education=="lessHighSchool"~"less-than-high-school",
      T~NA
  )) |>
  select(player_id = playerId, game_id = gameId, gender, age, native_language, race, education) |>
  write_csv(here("import/boyce2024_interaction/demogs.csv"))
