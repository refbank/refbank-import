library(here)
library(tidyverse)
library(jsonlite)
source(here("validate.R"))

# Pulled live from the paper authors' own repo (github.com/JMankewitz/comp-shapes-comm)
# rather than a local raw_data/ snapshot. Our earlier local copy was missing 5 of the 40
# game folders below (games "0","1","2","3","4"), which turned out to be exactly why our
# exclusion counts didn't match the published numbers -- see readme.md for the full
# before/after and the several raw-data quirks (missing `director` fields, a readr-vs-
# base-R blank-field parsing difference, and a player.csv identity-linking mismatch)
# this live pull surfaced and that this script now works around. `games`/`chats` come
# from the authors' already-cleaned processed CSVs; `round`/`player` are pulled from the
# raw export because the processed versions drop columns we need (`director`) or aren't
# parseable JSON blobs (`player`).
# As of 2026-08-25 (upstream commit 82c3d98f, "add additional image sets"), the authors
# added a second experiment and moved run_v3 from data/{raw,processed}_data/run_v3/ to
# data/{raw,processed}_data/exp_1/run_v3/ -- confirmed via the GitHub API as a pure
# rename (100% file-content match), not a data change.
raw_base_url <- "https://raw.githubusercontent.com/JMankewitz/comp-shapes-comm/master/data/raw_data/exp_1/run_v3/"
preprocessed_base_url <- "https://raw.githubusercontent.com/JMankewitz/comp-shapes-comm/master/data/processed_data/exp_1/run_v3/"

game_folders <- c(
  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
  "14-0", "14-1", "14-2", "14-3",
  "15-0", "15-1", "15-2",
  "16-0", "16-1", "16-2",
  "17-0", "17-1", "17-2",
  "18-0", "18-1",
  "19-0", "19-1",
  "20-0", "20-1", "20-2", "20-3", "20-4", "20-5",
  "21-0", "21-1", "21-2"
)

old_conditions <- c("noncomp", "comp-within", "comp-between")
new_conditions_long <- c(
  "No Competitor",
  "Within Trial Competitor",
  "Across Trial Competitor"
)

# games/chats: the authors' already-cleaned processed CSVs, one per game folder
load_processed <- function(data_name) {
  do.call(bind_rows, lapply(game_folders, function(folder) {
    read_csv(url(str_c(preprocessed_base_url, folder, "/", data_name, ".csv")), show_col_types = FALSE)
  }))
}

# round: needs the raw export -- the processed rounds.csv drops `director`, which we need
load_round_data <- function() {
  do.call(bind_rows, lapply(game_folders, function(folder) {
    # na = character(0): readr's default treats a blank CSV field as NA even in character
    # columns, unlike base read.csv (which the rest of this script's `response == ""` /
    # `director == ""` logic was written against) -- without this, missing response/
    # director silently becomes NA instead of "", and every `== ""` comparison downstream
    # (timed_out detection, exclusion counts, director fallback) silently evaluates to NA
    # instead of TRUE/FALSE.
    df <- read_csv(url(str_c(raw_base_url, folder, "/round.csv")),
      show_col_types = FALSE, na = character(0)
    ) |>
      select(
        roundID = id, correct, gameID, index, numTrials, repNum,
        response, target, targetNum, trialNum, tangramURLs, director
      )
    df$correct <- as.logical(df$correct) # Convert to logical
    df
  })) |>
    mutate(
      # a handful of rows (4-6 of ~39k) have a corrupted `director` field -- garbled
      # fragments of what look like tangram-description message text, not a player id.
      # One flagged row even has numTrials=65 instead of the expected 64, confirming
      # this is upstream CSV row/column misalignment, not a real director value. A real
      # director is always a 26-character ULID (matching the player ids actually seen
      # in gameplay); anything else gets treated as blank, same as a genuinely missing
      # director, so it flows through the existing same-parity fallback instead of
      # poisoning game_players/matcher_lookup with a bogus "player" made of message text.
      director = ifelse(str_detect(director, "^[0-9A-Z]{26}$"), director, "")
    )
}

# sort free-response gender prompt into male/female/nonbinary
bin_gender <- function(gender_val) {
  low_gender <- tolower(gender_val)
  return_val <- case_when(
    low_gender %in% c("female", "female ", "f", "woman", "femal", "femals", "femail", "famale", "females", "femaile", "femalr", "female/woman", "demigirl", "cis woman") ~ "female",
    low_gender %in% c("male", "malw", "man", " male", "male ", "man", "m", "boy", "trans-masc", "cis male") ~ "male",
    low_gender %in% c("nonbinary", "non-binary", "nb", "genderfluid") ~ "nonbinary",
    .default = as.character(low_gender)
  )
  return(return_val)
}

# I forgot to add partner information in my preprocessing script, so let's recover that...
load_participant_data <- function() {
  d_player_raw <- do.call(bind_rows, lapply(game_folders, function(folder) {
    # forced to character: some folders have all-NA columns (e.g. introDone) that get
    # inferred as logical, which then fails to bind_rows against folders where the same
    # column has real string values
    read_csv(url(str_c(raw_base_url, folder, "/player.csv")),
      show_col_types = FALSE, col_types = cols(.default = col_character())
    )
  }))
  # a 0-row data.frame here (from a missing/unparseable urlParams or exitSurvey, e.g.
  # a participant who left before finishing the exit survey) makes unnest() silently
  # DROP that player's whole row -- and since this table is also used below to recover
  # the *other* player's identity via a director->partnerID lookup, losing this row
  # breaks that lookup for every round where this player was director, producing a
  # phantom shared "unknown player" identity in place of a real one. A 1-row NA
  # placeholder keeps the player's row (and thus the partner lookup) intact; their own
  # demographics just come out NA, which is honest given the missing survey.
  d_players <- d_player_raw |>
    mutate(
      URLParams = map(urlParams, ~ possibly(function(x) {
        if (is.na(x) || x == "") {
          return(data.frame(participantKey = NA_character_))
        }
        fromJSON(x) %>% as.data.frame()
      }, otherwise = data.frame(participantKey = NA_character_))(.)),
      ExitSurvey = map(exitSurvey, ~ possibly(function(x) {
        if (is.na(x) || x == "") {
          return(data.frame(exit_survey_missing = TRUE))
        }
        fromJSON(x) %>% as.data.frame()
      }, otherwise = data.frame(exit_survey_missing = TRUE))(.))
    ) |>
    unnest(URLParams) %>%
    unnest(ExitSurvey) |>
    mutate(exit_survey_missing = coalesce(exit_survey_missing, FALSE)) |>
    select(
      playerID = id, gender, age, education, prolificID = participantKey,
      bonus, exitStepDone, gameID, partnerID = partner, exit_survey_missing
    )
  return(d_players)
}

d_game <- load_processed("games")
d_round <- load_round_data()
d_chat <- load_processed("chats")
d_players <- load_participant_data() |>
  # a handful of player.csv rows have gameID = "null" (literal string) or true NA --
  # e.g. someone who visited the lobby but never actually joined a game -- not real
  # participants in any game we have round data for
  filter(gameID %in% unique(d_round$gameID))

d_game$condition_label <- factor(d_game$contextStructure,
  levels = old_conditions,
  labels = new_conditions_long
)

d_game_final <- d_game |>
  mutate(
    dataset_id = "mankewitz2025_function",
    full_cite = "Mankewitz, J., & Hawkins, R. (2025). Function shapes form: Compositionality emerges from communicative needs, not environmental structure alone. In Proceedings of the Annual Meeting of the Cognitive Science Society (Vol. 47).",
    short_cite = "Mankewitz & Hawkins (2025)",
    group_size = 2,
    structure = "thick",
    language = "English",
    game_id = gameID
  )

# game director, per (gameID, parity of trial index) -- roles alternate by trial, so
# same-parity rounds normally share a director. A handful of rounds in the raw data have
# more than one non-missing director for the same (gameID, parity), so we take the modal
# value rather than an arbitrary one.
d_director <- d_round |>
  filter(director != "", !is.na(director)) |>
  select(gameID, director, index) |>
  mutate(parity = index %% 2) |>
  count(gameID, parity, derived_director = director) |>
  group_by(gameID, parity) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(gameID, parity, derived_director)

d_messages_final <- d_chat |>
  group_by(roundID) |>
  left_join(d_round |>
    select(roundID, gameID, index)) |>
  mutate(
    message_num = 1:n(),
    message_irrelevant = chit_chat,
    parity = index %% 2
  ) |>
  ungroup() |>
  left_join(d_director) |>
  mutate(
    action_type = "message",
    # a handful of rounds in the raw data have a genuinely missing `director` field, so
    # the precomputed director_msg here is NA rather than true/false -- fall back to the
    # modal same-parity director for that game. A residual few messages can't be
    # recovered even this way (both parity halves ambiguous/missing) and are dropped,
    # since role is genuinely unknowable for them.
    role = case_when(
      !is.na(director_msg) ~ ifelse(director_msg, "describer", "matcher"),
      !is.na(derived_director) ~ ifelse(playerID == derived_director, "describer", "matcher"),
      T ~ NA_character_
    ),
    time_stamp = NA,
    player_id = playerID,
    selected_image = NA,
    text = str_replace_all(text, "\n", " ")
  ) |>
  filter(!is.na(role)) |>
  select(
    roundID, gameID, action_type, player_id, role, time_stamp,
    message_num, message_irrelevant, selected_image, text
  )

# add dummy messages for rounds where director didn't talk

d_no_talk <- d_round |>
  as_tibble() |>
  select(roundID, gameID, director, index) |>
  unique() |>
  mutate(parity = index %% 2) |>
  filter(!is.na(gameID)) |>
  filter(!is.na(roundID)) |>
  left_join(d_director) |>
  mutate(director = ifelse(director == "" | is.na(director), derived_director, director)) |>
  anti_join(d_messages_final |> filter(role == "describer") |> select(roundID, gameID) |> unique()) |>
  filter(!is.na(director)) |>
  mutate(
    action_type = "message",
    player_id = director,
    role = "describer",
    time_stamp = NA,
    message_num = NA,
    message_irrelevant = NA,
    selected_image = NA,
    text = NA
  )


# For ~1 in 4 games, `player.csv`'s recorded playerID/partnerID pairing doesn't match
# the IDs actually used in gameplay (d_round$director / d_chat$playerID) for the same
# gameID -- an upstream identity-linking inconsistency (e.g. reconnects assigned a new
# ID mid-game). Recovering the matcher via `d_players`' partnerID lookup silently failed
# for these, producing NA player_id -- and since validate.R does a single global
# distinct() over (player_id, demographics) to assign numeric ids, every one of those NA
# rows collapses into ONE shared "ghost player" spanning all affected games. Instead, we
# derive the matcher directly from gameplay-observed identities: for a game with exactly
# 2 distinct player ids actually seen in its rounds/chat, the matcher is whichever of
# those 2 isn't the round's director -- no dependence on player.csv's identity linking.
game_players <- bind_rows(
  d_round |> filter(director != "", !is.na(director)) |> select(gameID, player_id = director),
  d_chat |>
    left_join(d_round |> select(roundID, gameID), by = "roundID") |>
    filter(playerID != "", !is.na(playerID)) |>
    select(gameID, player_id = playerID)
) |>
  distinct(gameID, player_id)

matcher_lookup <- game_players |>
  inner_join(game_players, by = "gameID", relationship = "many-to-many") |>
  filter(player_id.x != player_id.y) |>
  rename(director = player_id.x, matcher = player_id.y) |>
  # only keep games where this is unambiguous (exactly 2 gameplay-observed players)
  add_count(gameID, name = "n_pairs") |>
  filter(n_pairs == 2) |>
  distinct(gameID, director, matcher)

d_actions_final <- d_round |>
  mutate(parity = index %% 2) |>
  left_join(d_director, by = c("gameID", "parity")) |>
  mutate(
    # same blank/NA director gap as messages (see d_director above) -- fall back to the
    # same-parity modal director before looking up the matcher
    director = ifelse(director == "" | is.na(director), derived_director, director),
    image_options_list = tangramURLs |> str_remove_all('\\[|\\]|"') |> str_split(","),
    action_type = "selection",
    role = "matcher",
    time_stamp = as.numeric(NA),
    # a tiny handful of trials (3 of ~31k) have a response that isn't among the images
    # actually shown for that trial -- an upstream data glitch, not a real selection we
    # can trust -- so those get "unk" rather than passing along an invalid value.
    selected_image = case_when(
      response == "" ~ "timed_out",
      map2_lgl(response, image_options_list, \(r, o) r %in% o) ~ response,
      T ~ "unk"
    ),
    text = NA,
    message_num = as.numeric(NA),
    message_irrelevant = NA
  ) |>
  left_join(matcher_lookup |> select(gameID, director, player_id = matcher), by = c("gameID", "director")) |>
  mutate(
    # a residual ~14 games (of 617) have either only 1 distinct player id observed in
    # gameplay data, or >2 (genuinely ambiguous which 2 are the real pair) -- matcher
    # identity is unrecoverable there. A per-game placeholder (rather than leaving
    # player_id NA) keeps these from collapsing into one shared "ghost player" under
    # validate.R's global distinct() over (player_id, demographics) -- see game_players
    # comment above.
    player_id = coalesce(player_id, str_c(gameID, "_unknown_matcher"))
  ) |>
  select(
    roundID, gameID, action_type, player_id, role, time_stamp,
    message_num, message_irrelevant, selected_image, text
  )

# 10 games (of 617) have `director` missing in the raw data for every round of a whole
# parity half (32 trials) or the entire game (64 trials) -- both the real director field
# and the same-parity fallback above are unrecoverable there, so those trials have no
# usable describer identity at all. Rather than fabricate one, these games are dropped
# from the corpus entirely (not just flagged excluded -- validate.R requires a real
# describer per trial as a structural completeness check, not a quality-control one).
trials_with_describer <- bind_rows(
  d_messages_final |> filter(role == "describer") |> distinct(gameID, roundID),
  d_no_talk |> distinct(gameID, roundID)
)
incomplete_describer_games <- d_round |>
  distinct(gameID, roundID) |>
  anti_join(trials_with_describer, by = c("gameID", "roundID")) |>
  distinct(gameID) |>
  pull(gameID)

d_actions <- bind_rows(d_messages_final, d_actions_final, d_no_talk) |>
  filter(!(gameID %in% incomplete_describer_games)) |>
  left_join(d_players |> select(gameID, age, gender, education, player_id = playerID), by = c("player_id", "gameID"))

# Round Info

# Quality-control exclusions, per the paper: "participants were excluded from analysis
# (but still compensated) if they were missing more than 32 (50%) trials (n = 52) or
# had an accuracy rate below 75% (n = 41)." Two additional dyads were excluded for
# AI-generated-text chat content.
#
# This is now taken directly from the authors' own analysis code (game_stats block in
# analysis/02_cogsci2025_analyses.qmd, github.com/JMankewitz/comp-shapes-comm), not
# reconstructed by guesswork:
#   - n_responses = count of trials in the game with a non-empty response (across all
#     64 trials, not just a trailing run -- there's no special handling for scattered
#     vs. trailing timeouts, it's a flat count).
#   - accuracy = n_correct / n_responses, i.e. fraction correct among trials with an
#     actual selection (timed-out trials aren't scored either way).
#   - exclude if n_responses < 32 OR accuracy < 0.75.
#   - two specific games (game_blacklist below) are excluded for AI-generated text,
#     identified by the paper's authors by inspection, not by any derivable rule.
#
# Applying this exact formula directly to the source repo's own committed processed
# data (data/processed_data/run_v3, not re-derived through our own pipeline) exactly
# reproduces the paper's own numbers: 41 accuracy-only exclusions, 52 missing-only, 2
# AI-text blacklist, 450 kept -- out of 617 total games with round data. That confirms
# the formula above is correct, not a guess.
#
# Our own pipeline re-derives everything from the raw export (not the authors' processed
# CSVs) and needs some additional handling the paper's own code doesn't, because it's
# rebuilding describer/matcher identity from scratch rather than reusing already-clean
# columns -- see the `director`/`game_players`/`matcher_lookup` comments above. After
# those fixes, our actual output is 449 kept / 41 accuracy / 51 missing (19 missing-only
# + 32 missing-and-accuracy) / 2 blacklist / 64 "never started" -- within 1-2 games of
# the ground-truth numbers above, with the residual gap fully explained by the 10 games
# we drop for unrecoverable describer identity (some of which the ground-truth check
# would also have excluded, just for a different reason).
#
# One more thing the paper's own code does that isn't in the quoted paper text: some
# games have zero responses at all (n_responses == 0) -- i.e. the game never meaningfully
# started -- and the authors' analysis silently drops these before counting the 52/41/2
# exclusions (their own comment: "games that were able to start successfully ie have at
# least 1 response"). We keep those games but flag them with their own
# exclusion_reason rather than folding them into "missing", since "never started" and
# "started but stopped partway through" are different failure modes worth distinguishing.
game_blacklist <- c("01JGZ1J4WKTCRGQS8R8214JJH2", "01JGMPEGCD0EX4TXSSNEY3A670")

d_game_quality <- d_round |>
  as_tibble() |>
  mutate(correct_trial = ifelse(response == "", NA, response == target)) |>
  group_by(gameID) |>
  summarize(
    n_responses = sum(response != ""),
    n_correct = sum(correct_trial, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    accuracy = n_correct / n_responses,
    exclude_never_started = n_responses == 0,
    exclude_missing = !exclude_never_started & n_responses < 32,
    exclude_accuracy = !exclude_never_started & accuracy < 0.75,
    exclude_blacklist = gameID %in% game_blacklist,
    exclude = exclude_missing | exclude_accuracy | exclude_blacklist | exclude_never_started,
    exclusion_reason = case_when(
      exclude_blacklist ~ "AI-generated chat content",
      exclude_never_started ~ "no responses recorded (game never meaningfully started)",
      exclude_missing & exclude_accuracy ~ "missing >50% of trials; accuracy below 75%",
      exclude_missing ~ "missing >50% of trials",
      exclude_accuracy ~ "accuracy below 75%",
      T ~ NA_character_
    )
  ) |>
  select(gameID, exclude, exclusion_reason)

d_trial_info <- d_round |>
  as_tibble() |>
  mutate(
    image_options = tangramURLs |>
      str_remove_all('\\[|\\]|"') |>
      str_replace_all(",", ";"),
    trial_num = index + 1,
    round_num = repNum + 1,
    stage_num = 1
  ) |>
  left_join(d_game_quality, by = "gameID") |>
  select(roundID, gameID, image_options, trial_num, round_num, stage_num, exclude, exclusion_reason, target_image = target)

d_full <- d_actions |>
  left_join(d_trial_info) |>
  left_join(d_game_final) |>
  mutate(
    room_num = 1,
    age = ifelse(age != "", abs(as.numeric(age)), as.numeric(NA)), # guessing that hyphens are errors?
    gender = ifelse(gender != "", gender |> as.character() |> bin_gender(), as.character(NA)),
    education = case_when(
      education == "high-school" ~ "high-school",
      education == "master" ~ "advanced-degree",
      education == "bachelor" ~ "bachelors",
      T ~ NA
    ),
    race = as.character(NA),
    native_language = "English", # paper says recruited native English speakers from US UK Canada
    prior_relationship = "no",
    partner_constancy = "yes",
    role_constancy = "no",
    population = "adult",
    confederates = "no",
    modality = "written",
    feedback = "full", # confirmed this with Jess
    backchannel = "full",
    order_match = "match"
  ) |>
  select(
    condition_label, dataset_id, full_cite, short_cite,
    trial_num, round_num, stage_num, room_num,
    group_size, language, prior_relationship, partner_constancy, population, role_constancy,
    confederates, modality, feedback, backchannel, order_match,
    game_id, image_options,
    target_image, exclude, exclusion_reason, action_type,
    player_id, age, gender, education, race, native_language,
    role, time_stamp, text, message_num, message_irrelevant, selected_image
  )


validate_dataset(d_full, write = T)
