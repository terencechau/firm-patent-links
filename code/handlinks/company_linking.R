# This code takes all 1850-1870 CMF establishments with company-style names and matches them to 
# all 1840-1900 patents with company-style assignees by string distance

library(tidyverse)
library(magrittr)
library(stringdist)

patents_company_full <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/patents_company.csv")
cmf_company_full <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/cmf_company.csv",
                             col_types = cols(.default = "c")) %>% 
  mutate(id = paste(file_name, firm_number, sep = "_"),
         fips = as.character(fips),
         fips = str_pad(fips, width = 5, side = "left", pad = "0"))
county_crosswalk <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/county_crosswalk.csv")

company_matches <- data.frame()
system.time({
  for (year_to_link in c(1850, 1860, 1870)){
    patents_company <- patents_company_full %>% 
      select(disposal_date, patent_number, patent_number_id, fips, 
             patent_name, patent_name_clean)
    cmf_company <- cmf_company_full %>% 
      filter(year == year_to_link) %>% 
      select(year, id, fips, firm_name, firm_name_clean)
    
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
    
    patents_company %<>% 
      left_join(relevant_crosswalk, by = c("fips" = "fips_2000")) %>% 
      rename(fips_2000 = fips,
             fips = all_of(fips_var))
    
    # Loop over each fips, check if patent_name_clean and firm_name_clean are 
    # similar, save names and ids
    fips_codes <- unique(c(cmf_company$fips, patents_company$fips))
    
    for (i in fips_codes){ 
      patents_company_fips <- patents_company %>% 
        filter(fips == i)
      cmf_company_fips <- cmf_company %>% 
        filter(fips == i)
      
      if (nrow(patents_company_fips) == 0|nrow(cmf_company_fips) == 0){
        next
      }
      
      for (j in 1:nrow(cmf_company_fips)){
        name <- cmf_company_fips[j, ] %>% 
          select(-fips)
        matches <- data.frame(patents_company_fips, name) %>% 
          mutate(jw = stringdist(firm_name_clean, patent_name_clean,
                                 method = "jw", p = 0)) %>% 
          filter(jw <= 0.2)
        company_matches <- rbind(company_matches, matches)
      }
    }
  }
})

company_matches_sorted <- company_matches %>%
  mutate(match = 0) %>% 
  select(match, firm_name_clean, patent_name_clean, firm_name, patent_name,
         fips, fips_2000, year, disposal_date,
         jw, id, patent_number, patent_number_id) %>% 
  arrange(firm_name_clean, id, year, jw, patent_name_clean, patent_number, patent_number_id) 

write_csv(company_matches_sorted, "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/company_matches.csv")

