library(tidyverse)
library(magrittr)
library(lubridate)
library(haven)
library(stringdist)
library(openxlsx)

if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/")
}

# Set year
year <- 1860

# Load patents and CMF data
patents <- read_csv("Terence Working Folders/output/patents_1840_1900.csv") %>% 
  filter(year(disposal_date) <= year)
cmf_path <- paste0("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Micro Data/Vivian Working Folders/industry_assignment/estabs", year, ".dta")
# cmf_path <- paste0("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Micro Data/Vivian Working Folders/industry_assignment/", year, "_pilot_ind_assigned.dta")
cmf <- read_dta(cmf_path) 

# Load county crosswalk
county_crosswalk <- read_csv("Terence Working Folders/output/county_crosswalk.csv")

# Define output paths
linking_sheet_folder <- paste0("Terence Working Folders/output/pilot_linking_sheets/", year, "/backup/")
manifest_path <- paste0("Terence Working Folders/output/pilot_linking_sheets/manifests/manifest_", year, ".csv")

# Loop over all fips codes in the data and make a sheet for each one
fips_codes <- unique(cmf$fips) %>% sort()
fips_codes <- 25013

# Before loop over counties, define a function that saves a formatted Excel
# Make Excel sheet
write_formatted_excel <- function(df, output_path, 
                                  has_blanks = FALSE, blank_indices = NULL){
  # Create an Excel sheet
  excel <- createWorkbook()
  addWorksheet(excel, "Sheet 1")
  writeData(excel, sheet = "Sheet 1", df)
  
  # Color blank rows differently
  if (has_blanks){
    blank_fill <- createStyle(fgFill = "gray95")
    for (index in blank_indices){
      addStyle(excel, "Sheet 1", blank_fill, 
               rows = index + 1, cols = 1:ncol(df))
    }
  }
  
  # Color CMF, inventor and assignee columns differently
  index_inventor <- str_which(colnames(df), pattern = "inventor|nber")
  cell_fill <- createStyle(fgFill = "gray80", border = "TopBottomLeftRight", 
                           borderColour = "darkgray")
  for (index in index_inventor){
    addStyle(excel, "Sheet 1", cell_fill, 
             rows = 1:(nrow(df) + 1), cols = index)
  }
  
  index_assignee <- str_which(colnames(df), pattern = "assignee")
  cell_fill <- createStyle(fgFill = "wheat2", border = "TopBottomLeftRight", 
                           borderColour = "darkgray")
  for (index in index_assignee){
    addStyle(excel, "Sheet 1", cell_fill, 
             rows = 1:(nrow(df) + 1), cols = index)
  }
  
  # Add borders
  border_top <- createStyle(border = "Bottom", borderStyle = "medium")
  border_top_2 <- createStyle(border = "BottomRight", borderStyle = "medium")
  border_middle <- createStyle(border = "LeftRight", borderStyle = "medium")
  addStyle(excel, "Sheet 1", border_top, 
           rows = 1, cols = 1:ncol(df))
  addStyle(excel, "Sheet 1", border_top_2, rows = 1, cols = 1)
  addStyle(excel, "Sheet 1", border_middle, 
           rows = 2:(nrow(df) + 1), cols = 1)
  
  # Fix blank row borders
  if (has_blanks){
    blank_fill_2 <- createStyle(fgFill = "gray95", border = "LeftRight", 
                                borderStyle = "medium")
    addStyle(excel, "Sheet 1", blank_fill_2, 
             rows = (blank_indices + 1), cols = 1)
  }
  
  # Freze first row
  freezePane(excel, "Sheet 1", firstRow = TRUE)
  
  # Adjust column widths
  width_vec <- apply(df, 2, function(x){
    max(nchar(as.character(x)) + 1, na.rm = TRUE)
  })
  width_vec_header <- nchar(colnames(df)) + 1
  width_vec_max <- pmax(width_vec, width_vec_header)
  setColWidths(excel, "Sheet 1", 
               cols = 1:ncol(df), widths = width_vec_max)
  
  # Export
  saveWorkbook(excel, file = output_path, overwrite = TRUE)
}

# LOOP OVER COUNTIES STARTS HERE
# Saves a linking sheet for each county
# Returns data for a manifest sheet
manifest <- lapply(fips_codes, function(fips_code){
  # Set fips specific output path (these match the CMF linkage formatting, but 
  # note FIPS codes are padded to 5 characters in the Berkes data so afterwards
  # they're padded to match up)
  linking_sheet_path <- paste0(linking_sheet_folder, "fips", fips_code, "_", year, ".xlsx")
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
    file_name <- paste0("fips", fips_code_unpadded, "_", year, ".xlsx")
    return(data.frame("file_name" = file_name, "number_of_rows" = 0))
  }
  
  patents_county <- patents %>% 
    filter(inventor_fips %in% relevant_crosswalk$fips_2000|
           assignee_fips %in% relevant_crosswalk$fips_2000) %>% 
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
    mutate(county_type = case_when(is.na(assignee_fips) ~ "inventor",
                                   is.na(inventor_fips) ~ "assignee",
                                   !is.na(inventor_fips) & !is.na(assignee_fips) ~ "both"))
  
  # Skip to next iteration if no patents originated or were assigned in this county
  if (nrow(patents_county) == 0) {
    file_name <- paste0("fips", fips_code_unpadded, "_", year, ".xlsx")
    return(data.frame("file_name" = file_name, "number_of_rows" = 0))
  }
  
  cmf_county <- cmf %>%
    mutate(fips = as.character(fips),
           fips = str_pad(fips, width = 5, side = "left", pad = "0")) %>% 
    filter(fips == fips_code) %>% 
    select(firm_name, post_office, 
           industry_raw1, ind_leontief_industry1, ind_detailed_industry1,
           materials_kinds_raw1, production_kinds_raw1, power_kind1,
           file_name, firm_number) %>% 
    rename(establishment_name = firm_name,
           industry_raw = industry_raw1,
           industry_broad = ind_leontief_industry1,
           industry_detailed = ind_detailed_industry1,
           materials = materials_kinds_raw1,
           products = production_kinds_raw1,
           power_kind = power_kind1)
  
  # Make linking sheet, get Jaro-Winkler, sort
  # To do establishment blocks instead of patent blocks, arrange on
  # establishment_name, file_name, firm_number first
  linking_sheet <- merge(patents_county, cmf_county, all = TRUE) %>% 
    mutate(same_establishment = "",
           jaro_winkler = stringdist(establishment_name, inventor_name,
                                     method = "jw", p = 0.1)) %>% 
    # arrange(establishment_name, file_name, firm_number, jaro_winkler,
    #         inventor_name, assignee_name, patent_number) %>% 
    arrange(inventor_name, assignee_name, patent_number, jaro_winkler,
            establishment_name, file_name, firm_number) %>% 
    select(same_establishment, 
           inventor_name, assignee_name, establishment_name, # name variables
           county_type, # where to look for a match
           industry_raw, industry_broad, industry_detailed, # industry classifications 
           nber_subclass_name, nber_name, # patent classifications 
           # uspc_subclass, uspc, # patent classifications 2
           materials, products, power_kind, # aux establishment variables
           post_office, inventor_city, assignee_city, # finer location variables
           file_name, firm_number, patent_number, disposal_date) %>% # identifiers 
    mutate(across(.cols = everything(), as.character))
  
  # Sheet formatting
  # Replace all NAs with blanks
  linking_sheet[is.na(linking_sheet)] <- ""
  
  # Insert blank row between each patent block
  # To do it between each CMF establishment block, group on establishment_name, file_name, firm_number
  linking_sheet %<>%
    # group_by(establishment_name, file_name, firm_number) %>%
    group_by(inventor_name, assignee_name, patent_number) %>%
    do(add_row(.)) %>% 
    ungroup()
  blank_indices <- which(rowSums(is.na(linking_sheet)) == ncol(linking_sheet))
  blanks <- matrix(data = "", nrow = length(blank_indices), ncol = ncol(linking_sheet))
  linking_sheet[blank_indices, ] <- blanks
  
  # Save
  write_formatted_excel(df = linking_sheet, output_path = linking_sheet_path,
                        has_blanks = TRUE, blank_indices = blank_indices)
  
  # Return manifest data
  file_name <- paste0("fips", fips_code_unpadded, "_", year, ".xlsx")
  data.frame("file_name" = file_name, "number_of_rows" = nrow(linking_sheet))
})

# manifest %<>% bind_rows()
# write_csv(manifest, manifest_path)
