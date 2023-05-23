if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/")
}

library(tidyverse)
library(magrittr)

split_fips <- c(17031, 29189, 39061, 9009, 6075, 9001, 9003)

positive_preds_files <- list.files("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/positive_preds",
                                   full.names = TRUE)
positive_preds_files_split <- positive_preds_files[str_detect(positive_preds_files, pattern = "_[0-9]{1,2}.csv")]
all_preds_files <- list.files("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/all_preds",
                              full.names = TRUE)
all_preds_files_split <- all_preds_files[str_detect(all_preds_files, pattern = "_[0-9]{1,2}.csv")]

lapply(split_fips, function(fips_code){
  # Find split positive preds
  positive_preds_files_split_fips <- positive_preds_files_split[str_detect(positive_preds_files_split, pattern = paste0("fips", fips_code))]
  
  # Load all of them and bind them
  positive_preds <- lapply(positive_preds_files_split_fips, function(file){
    read_csv(file, col_types = cols(.default = "c"))
  }) %>% bind_rows()
  
  # Save
  write_csv(positive_preds, 
            file = paste0("/Users/terencechau/Desktop/test/fips", fips_code, "_1870.csv"))
  
  # Find split all preds
  all_preds_files_split_fips <- all_preds_files_split[str_detect(all_preds_files_split, pattern = paste0("fips", fips_code))]
  
  # Load all of them and bind them
  all_preds <- lapply(all_preds_files_split_fips, function(file){
    read_csv(file, col_types = cols(.default = "c"))
  }) %>% bind_rows()
  
  # Save
  write_csv(all_preds,
            file = paste0("/Users/terencechau/Desktop/test2/fips", fips_code, "_1870.csv"))
})

