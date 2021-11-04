library(readxl)
library(janitor)
library(tidyverse)
library(DT)
library(Hmisc)

df <- read_xlsx("SPARTA Raw Data (All Trials_August21).xlsx") %>% 
  left_join(read_xlsx("Key.xlsx")) %>% 
  clean_names() %>%
  separate(participant_name, into=c("event", "participant", "trial"), sep="_" ) %>% 
  filter(result=="PointsBased") %>% 
  drop_na(category) %>% 
  select(participant, event, trial, measure, description, grader_name, points_scored) 
 
#preprocessing (1) summarizes multiple OC ratings using the median score, (2) imputes median values where there are missinging ratings in the data frame, (3) joins a measure weighting to reflect importance and variability in the overall performance of the SUPRA event.  End Result: a long data frame with one measure per row.

df_preprocessed <- df %>% 
  filter(measure=="Task", event =="SH") %>%
  group_by(participant, event, trial, measure, description) %>%
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "description", values_from = "points_scored") %>% 
  ungroup() %>% 
  mutate_if(is.numeric, impute) %>% 
  gather(5:25, key = "description", value = "points_scored") %>% 
  bind_rows(
    df %>% 
      filter(measure=="Task", event =="STX") %>%
      group_by(participant, event, trial, measure, description) %>%
      summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
      pivot_wider(names_from = "description", values_from = "points_scored") %>% 
      ungroup() %>% 
      mutate_if(is.numeric, impute) %>% 
      gather(5:49, key = "description", value = "points_scored")
  ) %>% 
  bind_rows(
    df %>% 
      filter(measure=="Performance", event =="SH") %>%
      group_by(participant, event, trial, measure,description) %>%
      summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
      pivot_wider(names_from = "description", values_from = "points_scored") %>% 
      ungroup() %>% 
      mutate_if(is.numeric, impute) %>% 
      gather(5:19, key = "description", value = "points_scored")
    
  ) %>% 
  bind_rows(
    df %>% 
      filter(measure=="Performance", event =="STX") %>%
      group_by(participant, event, trial, measure, description) %>%
      summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
      pivot_wider(names_from = "description", values_from = "points_scored") %>% 
      ungroup() %>% 
      mutate_if(is.numeric, impute) %>% 
      gather(5:19, key = "description", value = "points_scored")
    
  ) %>% 
  left_join(read_xlsx("Key.xlsx", sheet="Weightings") %>% 
              select(description, weight3) %>% 
              rename(weight = weight3)) %>% 
  mutate(weight = replace_na(weight, 1)) %>% 
  mutate(weighted_raw = points_scored*weight) %>% 
  mutate_if(is.numeric, ~round(.x, 3))

df_squadscores <- df_preprocessed %>% 
  filter(measure=="Task") %>%
  group_by(participant, event,  trial, measure) %>% 
  summarise(point_total = sum(weighted_raw)/2) %>%
  rbind( df %>% 
           filter(measure=="Performance") %>% 
           group_by(participant, trial, measure, event) %>% 
           summarise(point_total = mean(points_scored, na.rm= TRUE)/10)   ) 

write.csv(df_preprocessed %>%
            mutate_if(is.numeric, ~ round(.x,2)), "SUPRA2021_RawData_preprocessed.csv"
)

#QA/QC to ensure appropriate counts of measures by each squad by event and trial
measure_counts <- df_preprocessed %>% 
  group_by(participant, event, trial, measure) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "trial", values_from = "count")

#Computation of weighted squad summary score per event (SH or STX) and trial (Trial 1, 2, and 3)
squad_summary <- df_squadscores %>%
  pivot_wider(names_from = "measure", values_from = "point_total") %>% 
  mutate(score = .5*Task + .5*Performance) %>% 
  filter(event == "SH") %>% 
  select(-c(Task, Performance)) %>% 
  mutate(trial = paste(event, trial, sep="_")) %>% 
  ungroup() %>% 
  select(-event) %>% 
  pivot_wider(names_from = "trial", values_from = "score") %>% 
  left_join(
    df_squadscores %>%
      pivot_wider(names_from = "measure", values_from = "point_total") %>% 
      mutate(score = .5*Task + .5*Performance) %>% 
      filter(event == "STX") %>% 
      select(-c(Task, Performance)) %>% 
      mutate(trial = paste(event, trial, sep="_")) %>% 
      ungroup() %>% 
      select(-event) %>% 
      pivot_wider(names_from = "trial", values_from = "score")
  ) 