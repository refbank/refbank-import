library(here)
library(tidyverse)
library(jsonlite)

# what does "controlled" mean?? -- looks like basically non-repeating / novel from the paper


## issues identified by Alvin
# expt 2 two stage, check stage count for expt 3
# expt 1 NA texts
# page-F target

data_dir <- "import/eliav_semantic/raw_data"


ParseJSONColumn <- function(x) {
  x |> str_replace_all("\\\\", "") |> 
    str_replace_all('"', "'") |>  str_replace_all(fixed("{'"), '{"') |> 
    str_replace_all(fixed("'}"), '"}') |> 
    str_replace_all("':", '":') |> 
    str_replace_all(": '", ': "') |> 
    str_replace_all("', '", '", "') |> 
    fromJSON(flatten = T) 
}


expt_1 <- read_csv(here(data_dir, "exp1_data.csv")) |> mutate(condition_label="expt1_singlemessage") |> 
  mutate(sec_until_click=as.double(sec_until_click)) |> mutate(structure="thin")

expt_2 <- read_csv(here(data_dir, "exp2_data.csv")) |> mutate(condition_label="expt2_singlemessage") |> 
  mutate(sec_until_click=as.double(sec_until_click)) |> mutate(structure="thin")

expt_3 <- read_csv(here(data_dir, "exp3_data.csv")) |> mutate(condition_label="expt3") |> 
  mutate(sec_until_click=as.double(sec_until_click)) |> mutate(structure="medium") # TODO verify with Robert

messages_single <- expt_1 |> bind_rows(expt_2) |> select(game_id, trial_index, block, target, controlled, structure,
                                speaker_id,  context, description, time_to_message,
                                condition_label) |> 
  mutate(player_id=speaker_id, 
         role="describer",
         time_stamp=time_to_message,
         message_number=1,
         message_irrelevant=F,
         action_type="message",
         text=description)

messages_complex <- expt_3 |> select(game_id, block, target, controlled, trial_index, structure, 
                                     condition_label, description, context, speaker_id, listener_id) |> 
mutate(description=map(description, ParseJSONColumn)) |> 
  unnest(description) |> 
  group_by(game_id, block, target, trial_index) |> 
  mutate(message_number=row_number()) |> 
  ungroup() |> 
  mutate(role=ifelse(role=="speaker", "describer", "matcher"),
         player_id=ifelse(role=="describer", speaker_id, listener_id),
         action_type="message") 


choices <- expt_1 |> bind_rows(expt_2) |> bind_rows(expt_3) |> select(game_id, trial_index, block, target, controlled,
                                                                      structure, listener_id, context,
                               sec_until_press, sec_until_click, response, condition_label) |> 
  mutate(player_id=listener_id, time_stamp=ifelse(!is.na(sec_until_press), sec_until_press, sec_until_click),
         action_type="selection",choice_id=response, role="matcher")

### combine everything 
all <- messages_single |>
  bind_rows(choices) |>
  bind_rows(messages_complex) |> 
  mutate(
    paper_id = "eliav2023_semantic",
    full_cite = "Eliav, R., Ji, A., Artzi, Y., & Hawkins, R. D. (2023). Semantic uncertainty guides the extension of conventions to new referents. arXiv preprint arXiv:2305.06539.",
    short_cite = "Eliav et al (2023)",
    language = "English",
    stage_num=1,
    trial_num = trial_index + 1,
    rep_num = 1 + block,
    group_size = 2,
    target=str_replace(target, ".svg", ""),
    choice_id=str_replace(choice_id, ".svg", ""),
    exclude=NA, #TODO figure out
    exclusion_reason=as.character(NA), # TODO figure out
    option_set=str_replace_all(context, ".svg", "") |> str_replace_all("', '", ";") |> str_replace_all(fixed("['"),"") |> str_replace_all(fixed("']"),""),
    ) |>
  select(paper_id, full_cite, short_cite, language,
    condition_label, time_stamp, stage_num, 
    game_id,
    player_id,
    stage_num, 
    trial_num, rep_num,
    role, target,
    action_type, exclude, exclusion_reason,
    message_number, text,
    choice_id, option_set,
    group_size, message_irrelevant,
    structure
  )

source(here("validate.R"))

validate_dataset(all, write = T)



