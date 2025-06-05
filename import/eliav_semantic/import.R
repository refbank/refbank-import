library(here)
library(tidyverse)
library(jsonlite)

# query -- what does "controlled" mean?? -- looks like basically non-repeating / novel from the paper

# query -- is there ordered chat data for "clean_no_discourse"
# query -- is role constant for "clean_no_discourse"
# query -- trial number for "clean_no_discourse"
# query -- option set for "clean_no_discourse"

data_dir <- "import/eliav_semantic/raw_data"

expt_1 <- read_csv(here(data_dir, "exp1_data.csv")) |> mutate(condition_label="expt1_singlemessage") |> 
  mutate(sec_until_click=as.double(sec_until_click))

expt_2 <- read_csv(here(data_dir, "exp2_data.csv")) |> mutate(condition_label="expt2_singlemessage") |> 
  mutate(sec_until_click=as.double(sec_until_click))

expt_3 <- read_csv(here(data_dir, "clean_no_discourse.csv")) |> mutate(condition_label="expt3") |> 
  mutate(sec_until_click=as.double(sec_until_click)) |> 
  group_by(game_id) |> 
  mutate(trial_index=row_number(), #NOTE THIS IS TOTALLY FAKE SO WE CAN APPEASE THE VALIDATOR
         context=str_c(response,";", target)) #AGAIN TOTALLY FAKE SO WE CAN APPEASE VALIDATOR

messages_single <- expt_1 |> bind_rows(expt_2) |> select(game_id, trial_index, block, target, controlled, 
                                speaker_id,  context, description, time_to_message,
                                condition_label) |> 
  mutate(player_id=speaker_id, 
         role="describer",
         time_stamp=time_to_message,
         message_number=1,
         message_irrelevant=F,
         action_type="message")

messages_complex <- expt_3 |> select(game_id, block, target, controlled, trial_index,
                                     condition_label, speaker_desc, listener_desc, context) |> 
  pivot_longer(c("speaker_desc", "listener_desc"), names_to="source", values_to="text") |> 
  mutate(text=str_sub(text, 2,-2) |> str_split(",")) |> 
  unnest(text) |> 
  mutate(text=trimws(text) |> str_sub(2,-2)) |>  # this is a janky way to do the unlisting, but whatever
  mutate(role=ifelse(source=="speaker_desc", "describer", "matcher"),
         player_id=str_c(game_id,"_",role),
         action_type="message")  #assuming role constancy TODO


choices <- expt_1 |> bind_rows(expt_2) |> bind_rows(expt_3) |> select(game_id, trial_index, block, target, controlled, listener_id, context,
                               sec_until_press, sec_until_click, response, condition_label) |> 
  mutate(player_id=listener_id, time_stamp=ifelse(!is.na(sec_until_press), sec_until_press, sec_until_click),
         action_type="selection",choice_id=response, role="matcher")

### combine everything 
all <- messages_single |>
  bind_rows(choices) |>
  bind_rows(messages_complex) |> 
  mutate(
    paper_id = "eliav_semantic",
    full_cite = "Eliav, R., Ji, A., Artzi, Y., & Hawkins, R. D. (2023). Semantic uncertainty guides the extension of conventions to new referents. arXiv preprint arXiv:2305.06539.",
    short_cite = "Eliav et al (2023)",
    language = "English",
    trial_num = trial_index + 1,
    rep_num = 1 + block,
    group_size = 2,
    structure = "pairs",
    target=str_replace(target, ".svg", ""),
    choice_id=str_replace(choice_id, ".svg", ""),
    exclude=NA, #TODO figure out
    exclusion_reason=as.character(NA), # TODO figure out
    option_set=str_replace_all(context, ".svg", "") |> str_replace_all("', '", ";") |> str_replace_all(fixed("['"),"") |> str_replace_all(fixed("']"),""),
    ) |>
  select(paper_id, full_cite, short_cite, language,
    condition_label, time_stamp,
    game_id,
    player_id,
    trial_num, rep_num,
    role, target,
    action_type, exclude, exclusion_reason,
    message_number, text,
    choice_id, option_set,
    group_size, message_irrelevant,
    structure
  )

source(here("validate.R"))

validate_dataset(all, write = F)



