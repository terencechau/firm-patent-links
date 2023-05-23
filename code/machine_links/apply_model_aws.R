# This code takes the model trained in CMF Patents/Terence Working Folders/output/linkage_model.RData
# and applies it to the candidate matches located in 
# CMF Patents/Terence Working Folders/output/applying_linking_model/1870/county_candidate_matches

# If there's enough RAM, could just run it over the entire dataset?

# Directory:
if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/")
}

library(tidyverse)
library(magrittr)
library(stringdist)
library(ranger)
library(tidytext)
source("CMF Micro Data/Terence Working Folders/firm_matching/code/linking_establishments/pre_processing/clean_names_functions_updating.R")

year <- 1870
invert_name <- function(string){
  # Split string, count words and initials
  split <- strsplit(string, "\\s+")[[1]]
  initials_loc <- grepl("\\b\\w\\b", split)
  initials <- split[initials_loc]
  initial_count <- length(initials)
  words <- split[!initials_loc]
  word_count <- length(words)
  
  # Inversion rules:
  # - Two token names have initials at the beginning always.
  # - Names with more than two tokens and one initial have the initial after
  # the first word, i.e., assume it's a middle name initial.
  # - Names with more than two tokens and more than one initial have the initials
  # at the beginning of the string, i.e., assume they're first name and middle
  # name initials
  
  if(word_count == 2 & initial_count == 0){
    return(paste(rev(words), collapse = " "))
  } else if(word_count == 1 & initial_count == 1){
    return(paste(initials, words, collapse = " "))
  } else if(word_count > 1 & initial_count == 1){
    return(paste(words[length(words)], initials, paste(words[-length(words)], collapse = " "), collapse = " "))
  } else if(word_count >= 1 & initial_count > 1){
    return(paste(paste(initials, collapse = " "), paste(words, collapse = " ")))
  } else {
    return(string)
  }
}

# Load model
load("CMF Patents/Terence Working Folders/output/rf_final.RData")
model_variable_names <- setdiff(model_variable_names, "match")

# List files to loop over
candidate_matches_files <- list.files("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/mini_batch_aws",
                                       full.names = TRUE)

# Begin loop here
lapply(candidate_matches_files, function(file){
  # Read data
  cmf_patents <- read_csv(file, col_types = "cDccccccccccc")
  short_filename <- str_extract(file, pattern = "fips.*")
  
  # Clean target names
  cmf_patents %<>%
    mutate(establishment_name = clean_string(establishment_name, alphanum_only = FALSE),
           establishment_name = tolower(establishment_name),
           inventor_name = clean_string(inventor_name, alphanum_only = FALSE),
           inventor_name = tolower(inventor_name),
           assignee_name = clean_string(assignee_name, alphanum_only = FALSE),
           assignee_name = tolower(assignee_name),
           company_name = ifelse(!proper_name(establishment_name), 1, 0))
  
  # Drop empty comparisons
  cmf_patents %<>% filter(establishment_name != "" & !is.na(establishment_name) & inventor_name != "" & !is.na(inventor_name))
  
  # Check if it's empty
  if (nrow(cmf_patents) == 0){
    return(print(paste(short_filename, "was empty", sep = " ")))
  }
  
  # Pick target names
  cmf_patents %<>% 
    rowwise() %>% 
    mutate(establishment_name_inverted = invert_name(establishment_name))

  cmf_patents %<>%
    mutate(jw_1 = stringdist(establishment_name, inventor_name, method = "jw", p = 0.1),
           jw_2 = stringdist(establishment_name_inverted, inventor_name, method = "jw", p = 0.1),
           jw_3 = stringdist(establishment_name, assignee_name, method = "jw", p = 0.1),
           jw_4 = stringdist(establishment_name_inverted, assignee_name, method = "jw", p = 0.1),
           jw_min = min(jw_1, jw_2, jw_3, jw_4, na.rm = TRUE),
           establishment_name_target = case_when(jw_min == jw_1|jw_min == jw_3 ~ establishment_name,
                                                 jw_min == jw_2|jw_min == jw_4 ~ establishment_name_inverted,
                                                 TRUE ~ as.character(NA)),
           patent_name_target = case_when(jw_min == jw_1|jw_min == jw_2 ~ inventor_name,
                                          jw_min == jw_3|jw_min == jw_4 ~ assignee_name,
                                          TRUE ~ as.character(NA)),
           patent_city = case_when(jw_min == jw_1|jw_min == jw_2 ~ inventor_city,
                                   jw_min == jw_3|jw_min == jw_4 ~ assignee_city,
                                   TRUE ~ as.character(NA)))
  
  # Clean up strings, pick right target names
  cmf_patents %<>%
    select(-c(establishment_name, establishment_name_inverted, inventor_name, 
              assignee_name, inventor_city, assignee_city)) %>% 
    rename(establishment_name = establishment_name_target,
           patent_name = patent_name_target) %>% 
    mutate(post_office = tolower(post_office),
           patent_year = lubridate::year(disposal_date))
  
  # Drop empty comparisons (again, in case an empty assignee was picked)
  cmf_patents %<>% filter(establishment_name != "" & !is.na(establishment_name) & patent_name != "" & !is.na(patent_name))
  
  # Generate distance measures
  cmf_patents$mean_jw_name <- mean_jw(cmf_patents$establishment_name, 
                                      cmf_patents$patent_name)
  cmf_patents$mean_jw_city <- mean_jw(cmf_patents$post_office, 
                                      cmf_patents$patent_city)
  cmf_patents %<>%
    mutate(jw_name = stringdist(establishment_name, patent_name, method = "jw", p = 0.1),
           mean_jw_name = ifelse(is.na(jw_name), NA, mean_jw_name),
           jw_city = stringdist(post_office, patent_city, method = "jw", p = 0.1),
           mean_jw_city = ifelse(is.na(jw_city), NA, mean_jw_city),
           year_gap = abs(year - patent_year))
  cmf_patents$per_token_jw_name <- calculate_per_token_jw(cmf_patents$establishment_name,
                                                          cmf_patents$patent_name) 
  
  # Make missing flags and impute
  cmf_patents %<>%
    mutate(jw_city_miss = ifelse(is.na(jw_city), 1, 0),
           jw_city = ifelse(is.na(jw_city), 
                            jw_city_mean_final, 
                            jw_city),
           mean_jw_city_miss = ifelse(is.na(mean_jw_city), 1, 0),
           mean_jw_city = ifelse(is.na(mean_jw_city), 
                                 mean_jw_city_mean_final, 
                                 mean_jw_city))
  
  # Cut down to model dataset
  model_data <- cmf_patents %>% 
    select(all_of(model_variable_names))
  
  # Fit model
  preds <- predict(rf_final, data = model_data)
  
  # Save all predictions with IDs
  cmf_patents %<>% drop_na(jw_name)
  preds_only <- data.frame("prediction" = preds$predictions, "id" = cmf_patents$id, 
                           "patent_number" = cmf_patents$patent_number)
  write_csv(preds_only, 
            file = paste0("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/all_preds/", short_filename))
  
  # Save all positive predictions with data
  preds_df <- data.frame("prediction" = preds$predictions, cmf_patents) %>% 
    filter(prediction == 1) %>% 
    select(-c(matches("jw_[0-9]"), "jw_min"))
  write_csv(preds_df, 
            file = paste0("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/positive_preds/", short_filename))
  
  print(paste(short_filename, "finished", sep = " "))
})





