# Put together an example sheet for hand linking
library(tidyverse)
library(magrittr)
library(lubridate)

# Berkes' data hasn't been crosswalked to industries, but the USPTO HPDF has
# Load first because it's a large file
# Trim to relevant dates
hpdf <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Patent Data/USPTO HPDF/historical_masterfile.csv")
hpdf %<>% 
  select(patent, nber, uspc, uspc_sub, disp_dt) %>% 
  mutate(disp_dt = dmy(disp_dt)) %>% 
  filter(year(disp_dt) <= 1900 & year(disp_dt) >= 1840) %>% 
  rename(patent_number = patent,
         nber_subclass = nber,
         uspc_subclass = uspc_sub,
         disposal_date = disp_dt)

# Load scraped NBER categories
nber_classes <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/input/nber_classes.csv") %>% 
  rename(nber = nber_class,
         nber_name = nber_class_name)

# Load inventor data and assignee data from Berkes' data
inventors <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Patent Data/Berkes/patents_inventor_name_location_1840_1900.csv") %>% 
  filter(inv_country == "us") %>% 
  select(-c(inv_country, inv_lat, inv_long)) %>% 
  rename(patent_number = patnum,
         inventor_name = inv_name,
         inventor_city = inv_city,
         inventor_county = inv_county,
         inventor_fips = inv_fips,
         inventor_state = inv_state)

assignees <- read_csv("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Patent Data/Berkes/patents_assignee_name_location_1840_1900.csv") %>% 
  filter(assignee_country == "us") %>% 
  select(-c(assignee_country, assignee_lat, assignee_long)) %>% 
  rename(patent_number = patnum)

# Put data together
# Only keep rows with at least inventor, class, and date data
patents <- left_join(hpdf, inventors, by = "patent_number") %>% 
  left_join(assignees, by = "patent_number") %>% 
  left_join(nber_classes, by = "nber_subclass") %>% 
  drop_na(patent_number, disposal_date, inventor_name, inventor_fips) %>% 
  mutate(inventor_county = str_remove(inventor_county, 
                                      pattern = " county| city| parish"),
         assignee_county = str_remove(assignee_county, 
                                      pattern = " county| city| parish")) %>% 
  select(patent_number, disposal_date, 
         inventor_fips, inventor_state, inventor_county, inventor_city,
         inventor_name, 
         assignee_fips, assignee_state, assignee_county, assignee_city,
         assignee_name, 
         nber_subclass, nber_subclass_name, nber, nber_name,
         uspc, uspc_subclass, everything())

# Save file covering all years
write_csv(patents, "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/patents_1840_1900.csv")



