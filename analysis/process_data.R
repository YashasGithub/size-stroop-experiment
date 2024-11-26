library(here)
library(tidyverse)
library(jsonlite)
library(testthat)

processed_data_directory <- here("..","data","processed_data")
file_name <- "size_stroop"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv"))) %>%
  rename(participant_id = participant)

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#filter and select relevant data
processed_data <- exp_data %>%
  filter(trial_type=="image-keyboard-response") %>%
  select(participant_id, random_id,condition,trial_index,time_elapsed,stimulus,response,correct_response:correct,rt,participant_thoughts,participant_difficulties) %>%
  group_by(participant_id) %>%
  mutate(
    trial_number=seq(n())
  ) %>%
  relocate(
    trial_number,.after="trial_index"
  ) %>%
  #extract stimulus information
  mutate(
    clean_path = str_remove_all(stimulus, "stimuli/|\\.jpg"),  # Remove 'stimuli/' and '.jpg'
    parts = str_split(clean_path, "-"),  # Split by '_'
    part1 = map_chr(parts, 1),  # Extract the first part (or you could directly access using `str_extract`)
    part2 = map_chr(parts, 2) # Extract the second part ) 
  ) %>%
  mutate(
    object_id = part1,
    stimulus_type = case_when(
      part2 == "Inongruent" ~ "incongruent",
      part2 == "Congruent" ~ "congruent"
    )
    ) %>%
  relocate(
    object_id,.after="stimulus"
  ) %>%
  relocate(
    stimulus_type,.after="object_id"
  )  %>%
  select(-c(clean_path:part2)) %>%
  mutate(
    is_right = ifelse(correct,1,0)
  ) %>%
  relocate(
    is_right,.after="correct"
  ) %>%
  #make rt numeric
  mutate(
    rt=as.numeric(as.character(rt))
  )

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))


