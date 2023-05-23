# This code collects and summarizes the matches in the pilot CMF-patent links
# It also produces a dataset with all the information for the model as well
# Terence Chau
# 4/14/2021

library(tidyverse)
library(magrittr)
library(readxl)
library(haven)

if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/")
}

# Read directories and bind into one dataset
# There are several kinds of files and they all need to be standardized!
# I tried different ways to set up the handlinking and that's why sheets might be slightly different
# 1. 1860 pilot counties
# 2. 1870 pilot counties ordered the same way as the 1860 ones.
# 3. 1870 pilot counties with inventor names first.
# 4. Early 1870 test sheets from last year with different sheets.
# 5. 1860 and 1870 counties that haven't been deduplicated yet.

# Each type of file is in a different folder, load them separately and check how they differ

linking_sheet_folder_1 <- "Terence Working Folders/output/pilot_linking_sheets/1860"
linking_sheet_folder_2 <- "Terence Working Folders/output/pilot_linking_sheets/1870/old_ordering"
linking_sheet_folder_3 <- "Terence Working Folders/output/pilot_linking_sheets/1870"
linking_sheet_folder_4 <- "Terence Working Folders/output/pilot_linking_sheets/1870/early_test_sheets"

# Load early test sheets and accommodate (these haven't been deduplicated!)
# Early sheets missing industry_broad, industry_detailed, power_kind, 
# load CMF data and merge in
linking_sheets_4 <- list.files(linking_sheet_folder_4, pattern = "xlsx", 
                               full.names = TRUE, recursive = FALSE)

cmf_patents_4 <- lapply(linking_sheets_4, function(file){
  read_excel(file, col_types = "text") %>% 
    mutate(fips = as.character(str_extract_all(file, pattern = "(?<=fips).*(?=\\_)")),
           year = str_sub(file, start = -9, end = -6))
}) %>% bind_rows() 

cmf_1870 <- read_dta("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Micro Data/Vivian Working Folders/industry_assignment/estabs1870.dta")
cmf_1870 %<>%
  select(file_name, firm_number, ind_detailed_industry1, ind_leontief_industry1,
         power_kind1) %>% 
  rename(industry_detailed = ind_detailed_industry1,
         industry_broad = ind_leontief_industry1,
         power_kind = power_kind1) %>% 
  mutate(firm_number = as.character(firm_number))
cmf_patents_4 %<>%
  left_join(cmf_1870, by = c("file_name", "firm_number")) %>%  
  rename(industry_raw = industry) %>%
  select(fips, year, same_establishment, establishment_name, inventor_name, 
         assignee_name, county_type, industry_raw, industry_broad, 
         industry_detailed, nber_subclass_name, nber_name, materials, products,
         power_kind, post_office, inventor_city, assignee_city, file_name, 
         firm_number, patent_number, disposal_date) 

# Load pilot counties
linking_sheets_1 <- list.files(linking_sheet_folder_1, pattern = "xlsx", 
                               full.names = TRUE, recursive = FALSE)
linking_sheets_2 <- list.files(linking_sheet_folder_2, pattern = "xlsx", 
                               full.names = TRUE, recursive = FALSE)
linking_sheets_1_2 <- c(linking_sheets_1, linking_sheets_2)

cmf_patents_1_2 <- lapply(linking_sheets_1_2, function(file){
  read_excel(file, col_types = "text") %>% 
    mutate(fips = as.character(str_extract_all(file, pattern = "(?<=fips).*(?=\\_)")),
           year = str_sub(file, start = -9, end = -6))
}) %>% bind_rows() %>% 
  select(fips, year, everything())

# Load pilot counties with the different ordering and fix column order
linking_sheets_3 <- list.files(linking_sheet_folder_3, pattern = "xlsx", 
                               full.names = TRUE, recursive = FALSE)

cmf_patents_3 <- lapply(linking_sheets_3, function(file){
  read_excel(file, col_types = "text") %>% 
    mutate(fips = as.character(str_extract_all(file, pattern = "(?<=fips).*(?=\\_)")),
           year = str_sub(file, start = -9, end = -6))
}) %>% bind_rows() %>% 
  select(fips, year, same_establishment, establishment_name, inventor_name, 
         assignee_name,everything())

# Load last set of handlinks (not deduplicated yet)
hampden_1860 <- read_excel("Terence Working Folders/output/pilot_linking_sheets/1860/not_deduplicated/fips25013_1860.xlsx")
hampden_1870 <- read_excel("Terence Working Folders/output/pilot_linking_sheets/1870/not_deduplicated/fips25013_1870.xlsx")

hampden_1860 %<>%
  mutate(fips = "25013",
         year = "1860")
hampden_1870 %<>%
  mutate(fips = "25013",
         year = "1870")

cmf_patents_5 <- bind_rows(hampden_1860, hampden_1870) %>% 
  select(fips, year, everything())

# Append everything
cmf_patents <- bind_rows(cmf_patents_1_2, cmf_patents_3, cmf_patents_4, 
                         cmf_patents_5)

rm(cmf_patents_1_2, cmf_patents_3, cmf_patents_4, cmf_patents_5, cmf_1870,
   hampden_1860, hampden_1870)

# Clean up
# YOU WERE HERE
drop_blank_rows <- function(df){
  df[rowSums(!is.na(df)) > 2, ] # > 2 because fips and year are never missing
}

cmf_patents %<>% drop_blank_rows()

# Tabulate links
tab_links <- table(cmf_patents$same_establishment, useNA = "ifany")
match_rate <- (tab_links[2]/(tab_links[1] + tab_links[2])) * 100

# How many distinct establishments were linked?
tab_estabs <- cmf_patents %>% 
  mutate(id = paste(file_name, firm_number, sep = "_")) %>% 
  group_by(id) %>% 
  summarize(any_match = ifelse(any(same_establishment == "y"), 1, 0)) %>% 
  ungroup() %>% 
  pull(any_match) %>% 
  table()

tab_estabs_1860 <- cmf_patents %>% 
  filter(year == "1860") %>% 
  mutate(id = paste(file_name, firm_number, sep = "_")) %>% 
  group_by(id) %>% 
  summarize(any_match = ifelse(any(same_establishment == "y"), 1, 0)) %>% 
  ungroup() %>% 
  pull(any_match) %>% 
  table()

tab_estabs_1870 <- cmf_patents %>% 
  filter(year == "1870") %>% 
  mutate(id = paste(file_name, firm_number, sep = "_")) %>% 
  group_by(id) %>% 
  summarize(any_match = ifelse(any(same_establishment == "y"), 1, 0)) %>% 
  ungroup() %>% 
  pull(any_match) %>% 
  table()

# How many distinct patents were linked?
tab_patents <- cmf_patents %>% 
  mutate(id = paste(file_name, firm_number, sep = "_")) %>% 
  group_by(patent_number) %>% 
  summarize(any_match = ifelse(any(same_establishment == "y"), 1, 0)) %>% 
  ungroup() %>% 
  pull(any_match) %>% 
  table()

tab_patents_1860 <- cmf_patents %>% 
  filter(year == "1860") %>% 
  mutate(id = paste(file_name, firm_number, sep = "_")) %>% 
  group_by(patent_number) %>% 
  summarize(any_match = ifelse(any(same_establishment == "y"), 1, 0)) %>% 
  ungroup() %>% 
  pull(any_match) %>% 
  table()

tab_patents_1870 <- cmf_patents %>% 
  filter(year == "1870") %>% 
  mutate(id = paste(file_name, firm_number, sep = "_")) %>% 
  group_by(patent_number) %>% 
  summarize(any_match = ifelse(any(same_establishment == "y"), 1, 0)) %>% 
  ungroup() %>% 
  pull(any_match) %>% 
  table()

print(paste0("The number of matched establishments was ", tab_estabs[2], 
             " out of ", tab_estabs[1] + tab_estabs[2], ", or ",
             round(100 * tab_estabs[2]/(tab_estabs[1] + tab_estabs[2]), 3), "%"))
print(paste0("The number of matched patents was ", tab_patents[2], 
             " out of ", tab_patents[1] + tab_patents[2], ", or ",
             round(100 * tab_patents[2]/(tab_patents[1] + tab_patents[2]), 3), "%"))

print(paste0("The number of matched establishments in 1860 was ", tab_estabs_1860[2], 
             " out of ", tab_estabs_1860[1] + tab_estabs_1860[2], ", or ",
             round(100 * tab_estabs_1860[2]/(tab_estabs_1860[1] + tab_estabs_1860[2]), 3), "%"))
print(paste0("The number of matched patents was ", tab_patents_1860[2], 
             " out of ", tab_patents_1860[1] + tab_patents_1860[2], ", or ",
             round(100 * tab_patents_1860[2]/(tab_patents_1860[1] + tab_patents_1860[2]), 3), "%"))

print(paste0("The number of matched establishments in 1870 was ", tab_estabs_1870[2], 
             " out of ", tab_estabs_1870[1] + tab_estabs_1870[2], ", or ",
             round(100 * tab_estabs_1870[2]/(tab_estabs_1870[1] + tab_estabs_1870[2]), 3), "%"))
print(paste0("The number of matched patents in 1870 was ", tab_patents_1870[2], 
             " out of ", tab_patents_1870[1] + tab_patents_1870[2], ", or ",
             round(100 * tab_patents_1870[2]/(tab_patents_1870[1] + tab_patents_1870[2]), 3), "%"))

# Add in the USPC classifications (they're codes, not names, unfortunately)
# US patents have one single original class (OR) and/or many cross-reference classes (XR)
# The OR is the main class in which an innovation is happening, the XRs are secondary.
# All patents have more consistently applied ORs, but XRs are examiner dependent.
# We only use ORs for now. IPC classes are also available, but missing for 644 of 
# these patents, so forget about those for now as well.

uspc <- read_csv("Patent Data/Berkes/patents_uspto_categories_1840_1900.csv") %>% 
  filter(main_class == 1) %>% 
  select(-main_class) %>% 
  rename(patent_number = patnum)

cmf_patents %<>%
  left_join(uspc, by = "patent_number") %>%
  select(fips, year, same_establishment, establishment_name, inventor_name, 
         assignee_name, county_type, industry_raw, industry_broad, 
         industry_detailed, uspc_class, nber_subclass_name, nber_name, materials, 
         products, power_kind, post_office, inventor_city, assignee_city, file_name, 
         firm_number, patent_number, disposal_date) 

# Save handlinked dataset for modeling
# write_csv(cmf_patents, "Terence Working Folders/output/cmf_patents.csv")



