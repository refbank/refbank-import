# image import work

library(tidyverse)
library(here)
library(stringi)


DATA_LOC=here("harmonized_data")
all_dirs <- list.dirs(DATA_LOC, full.names = FALSE) |> 
  stri_remove_empty()

all_trials <- map(all_dirs, \(d) read_csv(file.path(DATA_LOC, d, "trials.csv"), show_col_types = FALSE) |> 
                    mutate(target = as.character(target),
                           matchers = as.character(matchers)) |> left_join(read_csv(file.path(DATA_LOC, d, "conditions.csv")))) |> 
  list_rbind() 

#we only do target, not things that occur only as distractors. Could revisit

all_images <- all_trials |> select(target, paper_id) |> unique() |> 
  mutate(image_type=case_when(
    paper_id %in% c("yoon2019_audience") ~ "line drawing",
    paper_id %in% c("hawkins2019_continual") ~ "photograph",
    paper_id %in% c("boyce2024_interaction", "eliav2023_semantic",
                    "hawkins2020_characterizing_cued", "hawkins2020_characterizing_uncued",
                    "hawkins2021_respect", "hawkins2023_frompartners") ~ "tangram"
  ),
  kilogram_id = case_when(
    target %in% c("A", "B", "C", "D", "E", "F", "G","H", "I", "J", "K", "L") ~ str_c("page-", target),
    paper_id %in% c("eliav2023_semantic") ~  target
  ),
  image_path = 
    case_when(
      paper_id %in% c("boyce2024_interaction", "eliav2023_semantic",
                      "hawkins2020_characterizing_cued", "hawkins2020_characterizing_uncued",
                      "hawkins2021_respect", "hawkins2023_frompartners") ~ str_c(kilogram_id, ".svg")
  )) |> select(-paper_id) |> unique() |> write_csv(here("image_data/image_metadata.csv"))
  

# hilarious file processing adventure # 

# get intersection of kilogram names & ours
# then mass convert 

fix_svg <- function(in_file, out_file){
  old <- readLines(in_file, warn=F)
  new <- old |> str_replace_all("white", "black") |>
    str_replace_all("lightgray", "black") |>
    str_replace_all('strokewidth="1"', 'stroke-width="2"')
  writeLines(new, out_file)
}

to_fix <- all_images |> filter(!is.na(kilogram_id))

walk(to_fix$kilogram_id, \(id) fix_svg(here("tangrams-svg", str_c(id, ".svg")), here("image_data/images", str_c(id, ".svg"))))


