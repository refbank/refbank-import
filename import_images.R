# image import work

library(tidyverse)
library(here)
library(stringi)


DATA_LOC <- here("harmonized_data")
all_dirs <- list.dirs(DATA_LOC, full.names = FALSE) |>
  stri_remove_empty()

all_trials <- map(all_dirs, \(d) read_csv(file.path(DATA_LOC, d, "trials.csv"), show_col_types = FALSE) |>
  mutate(
    target_image = as.character(target_image),
    matchers = as.character(matchers)
  ) |>
  left_join(read_csv(file.path(DATA_LOC, d, "conditions.csv")))) |>
  list_rbind()

# we only do target_image, not things that occur only as distractors. Could revisit

get_kilogram_id_fmri <- function(target_image) {
  # for hawkins frmi images -- I did not find all images in kilogram; possible that I missed some
  case_when(
    target_image == "M" ~ "page1-148",
    target_image == "N" ~ NA,
    target_image == "O" ~ "page1-159",
    target_image == "P" ~ "page5-178",
    target_image == "Q" ~ "page9-29",
    target_image == "R" ~ "page8-147",
    target_image == "S" ~ NA,
    target_image == "T" ~ "page6-164",
    target_image == "U" ~ "page4-10",
    target_image == "V" ~ "page7-14",
    target_image == "W" ~ "page4-162",
    target_image == "X" ~ "page4-24",
    target_image == "Y" ~ "page8-234",
    target_image == "Z" ~ "page8-235",
    target_image == "AA" ~ "page7-248",
    target_image == "AB" ~ "page5-244",
    target_image == "AC" ~ "page7-218",
    target_image == "AD" ~ "page5-153",
    target_image == "AE" ~ NA,
    target_image == "AF" ~ NA,
    target_image == "AG" ~ NA,
    target_image == "AH" ~ NA,
    target_image == "AI" ~ NA,
    target_image == "AJ" ~ NA,
  )
}

get_kilogram_id_reuse <- function(target_image) {
  case_when( # I recognized these three
    target_image == "base_03" ~ "page_F",
    target_image == "base_04" ~ "page_I",
    target_image == "close_06" ~ "page_A"
  )
}

all_images <- all_trials |>
  select(target_image, dataset_id) |>
  unique() |>
  mutate(
    image_type = case_when(
      dataset_id %in% c("boegels2025_power") ~ "fribble",
      dataset_id %in% c("yoon2019_audience") ~ "line drawing",
      dataset_id %in% c("hawkins2019_continual", "wang2025_lvlms") ~ "photograph",
      dataset_id %in% c(
        "boyce2024_interaction", "ji2025_adhoc", "leung2024_scaffolding",
        "hawkins2020_characterizing", "hawkins2026_fmri",
        "hawkins2021_respect", "hawkins2023_frompartners", "mankewitz2025_function",
        "boyce2026_preschoolers", "dale2011_tangram", "branigan2016_doyouknow",
        "dahan2023_collaboration", "beatty-martinez2026_tangrams", "bangerter2020_lexical", "bangerter2000_reuse"
      ) ~ "tangram"
    ),
    kilogram_id = case_when(
      target_image %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L") ~ str_c("page-", target_image),
      dataset_id %in% c("hawkins2026_fmri") ~ get_kilogram_id_fmri(target_image),
      dataset_id %in% c("bangerter2000_reuse") ~ get_kilogram_id_reuse(target_image),
      dataset_id %in% c("eliav2023_semantic") ~ target_image,
    ),
    image_path =
      case_when(
        !is.na(kilogram_id) ~ str_c(kilogram_id, ".svg"),
        dataset_id == "hawkins2026_fmri" ~ str_c("tangrams_", target_image, ".svg"),
        dataset_id == "dahan2023_collaboration" ~ str_c(target_image, ".jpeg"),
        dataset_id %in% c("wang2025_lvlms", "bangerter2000_reuse") ~ str_c(target_image, ".png"),
        dataset_id %in% c("leung2024_scaffolding") ~ str_c(target_image, ".jpg"),
        target_image == "hold" ~ "I1.jpg",
        target_image == "walk" ~ "B1.jpg",
        target_image == "swim" ~ "D1.jpg",
        target_image == "jump" ~ "E1.jpg",
      )
  ) |>
  select(-dataset_id) |>
  rename(image_id = target_image) |>
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
