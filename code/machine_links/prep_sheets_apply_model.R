# This file loops over all counties for a year and creates all the candidate matches
# They get saved as .csvs in the output folder

library(tidyverse)
library(magrittr)
library(lubridate)
library(stringdist)
library(haven)
library(parallel)

if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/")
}

# Stuff to set
year <- 1870
linking_sheet_folder <- "Terence Working Folders/output/applying_linking_model/1870/county_candidate_matches/"
manifest_path <- paste0("Terence Working Folders/output/applying_linking_model/manifests/manifest_", year, ".csv")

# Load patents and CMF for that year
patents <- read_csv("Terence Working Folders/output/patents_1840_1900.csv") %>% 
  select(-c(inventor_state, inventor_county, assignee_state, assignee_county,
            matches("nber"), uspc_subclass)) %>% 
  rename(uspc_class = uspc) %>% 
  mutate(across(.cols = c(inventor_city, inventor_name, assignee_city, assignee_name), .fns = tolower))
cmf_path <- paste0("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Micro Data/Vivian Working Folders/industry_assignment/estabs", year, "_updated.dta")
cmf <- read_dta(cmf_path) 

# Load county crosswalk
county_crosswalk <- read_csv("Terence Working Folders/output/county_crosswalk.csv")

# Save all fips codes
fips_codes <- unique(cmf$fips) %>% 
  sort()

# Remove handlinked fips codes
handlinked_fips <- read_csv("Terence Working Folders/output/cmf_patents_reviewed.csv") %>% 
  filter(year == 1870) %>% 
  pull(fips) %>% 
  unique()
fips_codes <- setdiff(fips_codes, handlinked_fips)

system.time({
manifest <- mclapply(fips_codes, mc.cores = 1, FUN = function(fips_code){
  # Set fips specific output path (these match the CMF linkage formatting, but 
  # note FIPS codes are padded to 5 characters in the Berkes data so afterwards
  # they're padded to match up)
  linking_sheet_path <- paste0(linking_sheet_folder, "fips", fips_code, "_", year, ".csv")
  fips_code_unpadded <- fips_code
  fips_code <- str_pad(fips_code, width = 5, side = "left", pad = "0")
  
  # Crosswalk counties in the patent data
  fips_var <- paste0("fips_", year)
  relevant_crosswalk <- county_crosswalk %>% 
    rename(fips = fips_var[1]) %>% 
    select(city, fips, fips_2000) %>% 
    filter(fips == fips_code) %>% 
    mutate(fips = ifelse(is.na(fips), fips_2000, fips),
           fips_2000 = ifelse(is.na(fips_2000), fips, fips_2000)) %>% 
    drop_na() %>% 
    distinct()
  
  # Skip to next iteration if no city where a patent originated or was assigned is in this county
  if (nrow(relevant_crosswalk) == 0) {
    file_name <- paste0("fips", fips_code_unpadded, "_", year, ".csv")
    return(data.frame("file_name" = file_name, "number_of_rows" = 0))
  }
  
  patents_county <- patents %>% 
    filter((inventor_fips %in% relevant_crosswalk$fips_2000 & inventor_city %in% relevant_crosswalk$city)|
             (assignee_fips %in% relevant_crosswalk$fips_2000 & assignee_city %in% relevant_crosswalk$city)) %>% 
    mutate(inventor_city = tolower(inventor_city),
           assignee_city = tolower(assignee_city)) %>% 
    left_join(relevant_crosswalk, by = c("inventor_city" = "city",
                                         "inventor_fips" = "fips_2000")) %>% 
    select(-inventor_fips) %>% 
    rename(inventor_fips = fips) %>% 
    left_join(relevant_crosswalk, by = c("assignee_city" = "city",
                                         "assignee_fips" = "fips_2000")) %>% 
    select(-assignee_fips) %>% 
    rename(assignee_fips = fips) %>% 
    mutate(county_type = case_when(is.na(assignee_fips) & is.na(inventor_fips) ~ as.character(NA),
                                   is.na(assignee_fips) ~ "inventor",
                                   is.na(inventor_fips) ~ "assignee",
                                   !is.na(inventor_fips) & !is.na(assignee_fips) ~ "both")) 
  
  # Skip to next iteration if no patents originated or were assigned in this county
  if (nrow(patents_county) == 0) {
    file_name <- paste0("fips", fips_code_unpadded, "_", year, ".csv")
    return(data.frame("file_name" = file_name, "number_of_rows" = 0))
  }
  
  cmf_county <- cmf %>%
    mutate(fips = as.character(fips),
           fips = str_pad(fips, width = 5, side = "left", pad = "0"),
           id = paste(file_name, firm_number, sep = "_")) %>% 
    filter(fips == fips_code) %>% 
    select(firm_name, post_office, id) %>% 
    rename(establishment_name = firm_name)
  
  # Make linking sheet
  linking_sheet <- merge(patents_county, cmf_county, all = TRUE) %>% 
    arrange(id, patent_number)
  
  # Save
  write_csv(x = linking_sheet, file = linking_sheet_path)
  
  # Return manifest data
  file_name <- paste0("fips", fips_code_unpadded, "_", year, ".csv")
  data.frame("file_name" = file_name, "number_of_rows" = nrow(linking_sheet))
})

manifest %<>% bind_rows()
write_csv(manifest, manifest_path)
})