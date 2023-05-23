library(tidyverse)

if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/")
}

fips <- 9001

df <- read_csv(paste0("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/mini_batch_aws/fips", fips, "_1870.csv"), col_types = "cDccccccccccc")
df$group <- rep(1:ceiling(nrow(df)/1000000), each = 1000000, length.out = nrow(df))
df_list <- split(df, df$group)
df_list <- lapply(df_list, function(df){df %>% select(-group)})

# make filenames
file_names <- paste0("CMF Patents/Terence Working Folders/output/applying_linking_model/1870/mini_batch_aws/fips", fips, "_1870_", 1:length(df_list), ".csv")

lapply(seq_along(df_list), function(i){
  write_csv(df_list[[i]], file = file_names[i])
})