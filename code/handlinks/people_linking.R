# This code takes all 1850-1870 CMF establishments with people's names and matches them to 
# all 1840-1900 patents with inventors or assignees with people's names by string distance
library(tidyverse)
library(magrittr)
library(stringdist)

patents_people_full <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/patents_people.csv")
cmf_people_full <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/cmf_people.csv",
                             col_types = cols(.default = "c")) %>% 
  mutate(id = paste(file_name, firm_number, sep = "_"),
         fips = as.character(fips),
         fips = str_pad(fips, width = 5, side = "left", pad = "0"))
county_crosswalk <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/county_crosswalk.csv")

people_matches <- data.frame()
system.time({
  for (year_to_link in c(1850, 1860, 1870)){
    patents_people <- patents_people_full %>% 
      select(disposal_date, fips, patent_number, patent_number_id, 
             first_name, middle_name, last_name, inventor) %>% 
      rename(patent_first_name = first_name,
             patent_middle_name = middle_name,
             patent_last_name = last_name)
    # Keep only establishments with non-missing first and last names
    cmf_people <- cmf_people_full %>% 
      filter(year == year_to_link) %>% 
      select(year, fips, id, first_name, middle_name, last_name) %>% 
      filter(!is.na(first_name), !is.na(last_name)) %>% 
      rename(firm_first_name = first_name,
             firm_middle_name = middle_name,
             firm_last_name = last_name)
    
    # Convert patent fips in year 2000 to linking year
    fips_var <- paste0("fips_", year_to_link)
    relevant_crosswalk <- county_crosswalk %>% 
      select(all_of(fips_var), fips_2000) 
    
    # Fill out missing ones with the other year's (to give the observations some chance of matching)
    relevant_crosswalk[[fips_var]] <- ifelse(is.na(relevant_crosswalk[[fips_var]]), 
                                             relevant_crosswalk[["fips_2000"]], 
                                             relevant_crosswalk[[fips_var]])
    relevant_crosswalk[["fips_2000"]] <- ifelse(is.na(relevant_crosswalk[["fips_2000"]]), 
                                                relevant_crosswalk[[fips_var]], 
                                                relevant_crosswalk[["fips_2000"]])
    relevant_crosswalk %<>% 
      distinct()
    
    patents_people %<>% 
      left_join(relevant_crosswalk, by = c("fips" = "fips_2000")) %>% 
      rename(fips_2000 = fips,
             fips = all_of(fips_var))
    
    # Loop over each fips, check if first, middle, and last names are 
    # similar, save names and ids
    fips_codes <- unique(c(cmf_people$fips, patents_people$fips))
    
    for (i in fips_codes){ 
      patents_people_fips <- patents_people %>% 
        filter(fips == i)
      cmf_people_fips <- cmf_people %>% 
        filter(fips == i)
      
      if (nrow(patents_people_fips) == 0|nrow(cmf_people_fips) == 0){
        next
      }
      
      for (j in 1:nrow(cmf_people_fips)){
        name <- cmf_people_fips[j, ] %>% 
          select(-fips)
        matches <- data.frame(patents_people_fips, name) %>% 
          mutate(jw_first = stringdist(firm_first_name, patent_first_name,
                                 method = "jw", p = 0),
                 jw_middle = stringdist(firm_middle_name, patent_middle_name,
                                       method = "jw", p = 0),
                 jw_last = stringdist(firm_last_name, patent_last_name,
                                       method = "jw", p = 0)) %>% 
          filter(jw_first <= 0.2 & jw_last <= 0.2)
        people_matches <- rbind(people_matches, matches)
      }
    }
  }
})

people_matches_sorted <- people_matches %>%
  mutate(match = 0) %>% 
  select(match, matches("first_name|middle_name|last_name"), fips, fips_2000, year, disposal_date,
         jw_first, jw_middle, jw_last, id, patent_number, patent_number_id) %>% 
  arrange(firm_first_name, firm_last_name, id, year, jw_last, jw_first, jw_middle, patent_number, patent_number_id) 

write_csv(people_matches_sorted, "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/people_matches.csv")

# Save shuffled version
id <- unique(people_matches$id)
sorted_df <- people_matches %>% 
  arrange(id, jw_last, jw_first, year, patent_number, patent_number_id) %>% 
  mutate(match = 0)
shuffle_df <- shuffle_id %>% 
  left_join(sorted_df, by = "id") %>% 
  select(match, matches("first_name|middle_name|last_name"), fips, fips_2000, year, disposal_date, jw_first, 
         jw_middle, jw_last, id, patent_number, patent_number_id)
# write_csv(shuffle_df, "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/people_matches_shuffled.csv")
