library(tidyverse)
library(magrittr)
library(lubridate)
library(haven)

people_matches <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/people_matches_shuffled.csv",
                           col_types = cols(.default = "c"))
people_patents <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/people_patents.csv",
                                col_types = cols(.default = "c"))
people_cmf <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/people_cmf.csv",
                            col_types = cols(.default = "c")) %>% 
  mutate(id = paste(file_name, firm_number, sep = "_"))

labeled_up_to <- 8016
labeled_matches <- people_matches[1:labeled_up_to, ]

# Raw number of matches
table(labeled_matches$match)

# Total number of IDs/Pat numbers
length(unique(people_patents$patent_number))
length(unique(people_cmf$id))

# Number of matched IDs/Pat numbers
labeled_matches %>% 
  group_by(id) %>% 
  summarize(matched = any(match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

labeled_matches %>% 
  group_by(patent_number) %>% 
  summarize(matched = any(match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

# Number of matches using simple rules
people_matches %<>%
  mutate(exact_match_middle = case_when(jw_first == 0 & jw_middle == 0 & jw_last == 0 ~ 1,
                                        jw_first == 0 & is.na(jw_middle) & jw_last == 0 ~ 1,
                                        TRUE ~ 0),
         exact_match = ifelse(jw_first == 0 & jw_last == 0, 1, 0),
         close_match_middle = case_when(jw_first <= 0.05 & jw_middle <= 0.05 & jw_last <= 0.05 ~ 1,
                                      jw_first <= 0.05 & is.na(jw_middle) & jw_last <= 0.05 ~ 1,
                                      TRUE ~ 0),
         close_match = ifelse(jw_first <= 0.05 & jw_last <= 0.05, 1, 0),
         strict_exact_match = case_when(jw_first == 0 & jw_middle == 0 & jw_last == 0 ~ 1,
                                        jw_first == 0 & is.na(firm_middle_name) & is.na(patent_middle_name) & jw_last == 0 ~ 1,
                                        TRUE ~ 0))

# Number of matches under each rule
people_matches %>% 
  group_by(id) %>% 
  summarize(matched = any(strict_exact_match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(id) %>% 
  summarize(matched = any(exact_match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(id) %>% 
  summarize(matched = any(exact_match_middle == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(id) %>% 
  summarize(matched = any(close_match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(id) %>% 
  summarize(matched = any(close_match_middle == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(patent_number) %>% 
  summarize(matched = any(strict_exact_match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(patent_number) %>% 
  summarize(matched = any(exact_match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(patent_number) %>% 
  summarize(matched = any(exact_match_middle == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(patent_number) %>% 
  summarize(matched = any(close_match == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

people_matches %>% 
  group_by(patent_number) %>% 
  summarize(matched = any(close_match_middle == 1), .groups = "keep") %>% 
  ungroup() %>% 
  pull(matched) %>% 
  table()

# Under these rules, we get match rates of around:
# Firms: 15.62% for the strictest, between 28.97% - 37.52% for the others
# Patents: 12.94% for the strictest, between 27.72% - 35.22% for the others

# Get an idea for how many I'd lose, given what's in the handlinks
labeled_matches %>% filter(match == 1) %>% nrow()

labeled_matches %>% filter(match == 1 & jw_first == 0) %>% nrow()
labeled_matches %>% filter(match == 1 & jw_first <= 0.05) %>% nrow()

labeled_matches %>% filter(match == 1 & jw_last == 0) %>% nrow()
labeled_matches %>% filter(match == 1 & jw_last <= 0.05) %>% nrow()

# Within the set of linked stuff, 93.5% have a first name distance of 0, 93.8% under 0.05.
# 73.4% have a last name distance of 0, 76% under 0.05.

# Sort and save
people_matches %<>%
  select(matches("match"), everything()) %>% 
  arrange(year, id, firm_first_name, firm_last_name, patent_first_name, patent_last_name, patent_number, patent_number_id)

write_csv(people_matches, "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/people_matches_2.csv")










