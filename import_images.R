# image import work

library(tidyverse)
library(here)
library(stringi)


DATA_LOC <- here("harmonized_data")
all_dirs <- list.dirs(DATA_LOC, full.names = FALSE) |>
  stri_remove_empty()

all_trials <- map(all_dirs, \(d) read_csv(file.path(DATA_LOC, d, "trials.csv"), show_col_types = FALSE) |>
  mutate(
    target = as.character(target),
    matchers = as.character(matchers)
  ) |>
  left_join(read_csv(file.path(DATA_LOC, d, "conditions.csv")))) |>
  list_rbind()

# we only do target, not things that occur only as distractors. Could revisit


all_images <- all_trials |>
  select(target, dataset_id) |>
  unique() |>
  mutate(
    image_type = case_when(
      dataset_id %in% c("boegels2025_power") ~ "fribble",
      dataset_id %in% c("yoon2019_audience") ~ "line drawing",
      dataset_id %in% c("hawkins2019_continual", "wang2025_lvlms") ~ "photograph",
      dataset_id %in% c(
        "boyce2024_interaction", "ji2025_adhoc", "leung2024_scaffolding",
        "hawkins2020_characterizing_cued", "hawkins2020_characterizing_uncued",
        "hawkins2021_respect", "hawkins2023_frompartners", "mankewitz2025_function",
        "boyce2026_preschoolers", "dale2011_tangram", "branigan2016_doyouknow", "dahan2023_collaboration"
      ) ~ "tangram"
    ),
    kilogram_id = case_when(
      target %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L") ~ str_c("page-", target),
      dataset_id %in% c("eliav2023_semantic") ~ target,
    ),
    image_path =
      case_when(
        dataset_id %in% c(
          "boyce2024_interaction", "ji2025_adhoc",
          "hawkins2020_characterizing_cued", "hawkins2020_characterizing_uncued",
          "hawkins2021_respect", "hawkins2023_frompartners", "branigan2016_doyouknow"
        ) ~ str_c(kilogram_id, ".svg"),
        dataset_id == "dahan2023_collaboration" ~ str_c(target, ".jpeg"),
        dataset_id == "wang2025_lvlms" ~ str_c(target, ".png"),
        dataset_id %in% c("leung2024_scaffolding") ~ str_c(target, ".jpg"),
        target == "hold" ~ "I1.jpg",
        target == "walk" ~ "B1.jpg",
        target == "swim" ~ "D1.jpg",
        target == "jump" ~ "E1.jpg",
        dataset_id == "boyce2026_preschoolers" & target %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L") ~ str_c(kilogram_id, ".svg"),
      )
  ) |>
  select(-dataset_id) |>
  rename(image_id = target) |>
  unique() |>
  write_csv(here("image_data/image_metadata.csv"))


# hilarious file processing adventure #

# get intersection of kilogram names & ours
# then mass convert

fix_svg <- function(in_file, out_file) {
  old <- readLines(in_file, warn = F)
  new <- old |>
    str_replace_all("white", "black") |>
    str_replace_all("lightgray", "black") |>
    str_replace_all('strokewidth="1"', 'stroke-width="2"')
  writeLines(new, out_file)
}

# to_fix <- all_images |> filter(!is.na(kilogram_id))

# walk(to_fix$kilogram_id, \(id) fix_svg(here("tangrams-svg", str_c(id, ".svg")), here("image_data/images", str_c(id, ".svg"))))
