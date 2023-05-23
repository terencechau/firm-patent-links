library(tidyverse)
library(magrittr)

# Count up positive predictions by county
positive_preds_files <- list.files("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/applying_linking_model/1870/positive_preds_2",
                                   full.names = TRUE,
                                   pattern = "csv")

match_counts_county <- lapply(positive_preds_files, function(file){
  df <- read_csv(file, col_types = cols(.default = "c"))
  count <- nrow(df)
  fips <- str_extract(file, pattern = "(?<=fips).*(?=_1870.csv)")
  unique_matched_estabs <- length(unique(df$id))
  unique_matched_pats <- length(unique(df$patent_number))
  data.frame("match_count" = count, "fips" = fips, unique_matched_estabs, unique_matched_pats)
}) %>% bind_rows()

# How many counties had a link
n_matched_counties <- match_counts_county %>% 
  filter(match_count > 0) %>% 
  nrow()
# There are 2317 counties in the 1870 CMF
n_matched_counties
n_matched_counties/2317

# How many unique establishments were linked
n_matched_estabs <- sum(match_counts_county$unique_matched_estabs)
n_matched_estabs

# How many unique patents were linked
n_matched_pats <- sum(match_counts_county$unique_matched_pats)
n_matched_pats

# Latex table for county matches
library(kableExtra)
match_counts_county_table <- match_counts_county %>% 
  select(fips, match_count, unique_matched_estabs, unique_matched_pats) %>% 
  arrange(-match_count) %>% 
  slice_head(n = 12) %>% 
  kable(format = "latex")

# Correlate match count to county size
library(haven)
# cmf <- read_dta("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Micro Data/Vivian Working Folders/industry_assignment/estabs1870_updated.dta")
# n_estabs <- cmf %>% 
#   count(fips) %>% 
#   mutate(fips = as.character(fips))
foo <- match_counts_county %>% 
  filter(match_count > 0) %>% 
  select(fips, match_count) %>% 
  left_join(n_estabs, by = "fips")
cor(foo$match_count, foo$n)

# Count up positive predictions by establishment
match_counts_estab <- lapply(positive_preds_files, function(file){
  df <- read_csv(file, col_types = cols(.default = "c")) %>% 
    count(id) %>% 
    rename(match_count = n)
}) %>% bind_rows()

match_counts_estab %>% 
  ggplot(aes(x = match_count)) +
  geom_density()

# Match count distributions
summary(match_counts_county %>% filter(match_count > 0) %>% pull(match_count))
summary(match_counts_estab %>% filter(match_count > 0) %>% pull(match_count))

# Collect matches then take a random subset for handchecking
set.seed(60165)
handcheck <- lapply(positive_preds_files, function(file){
  df <- read_csv(file, col_types = cols(.default = "c"))
}) %>% bind_rows() %>% 
  slice_sample(n = 1000) %>% 
  mutate(agree = "") %>% 
  select(prediction, agree, establishment_name, patent_name, post_office, patent_city, patent_year, everything())
# write_csv(handcheck, file = "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/applying_linking_model/1870/handcheck.csv")



