# The Berkes data assigned patents to counties by mapping the latitude and 
# longitude to a particular county going by county borders in the year 2000.
# This code reassigns patents to counties based off of each year in the CMF
# micro data, resulting in the same files but with a county FIPS column for each
# CMF year.

if(Sys.info()[[1]] == "Windows"){
  setwd("C:/Users/tchau/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents")
} else {
  setwd("~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents")
}

library(tidyverse)
library(magrittr)
library(sf)
library(s2)
library(tmap)
library(USAboundaries)

# Load data, keep only observations in the US with valid latitude and longitude
inventors <- read_csv("Patent Data/Berkes/patents_inventor_name_location_1840_1900.csv") %>% 
  filter(inv_country == "us" & inv_lat != -999 & inv_long != -999) %>% 
  select(-inv_country)
assignees <- read_csv("Patent Data/Berkes/patents_assignee_name_location_1840_1900.csv") %>% 
  filter(assignee_country == "us" & assignee_lat != -999 & assignee_long != -999) %>% 
  select(-assignee_country)

# Save all city-state pairs in both datasets, turn into geo object
inventor_cities <- inventors %>% 
  select(inv_city, inv_state, inv_county, inv_fips, inv_lat, inv_long)
assignee_cities <- assignees %>% 
  select(assignee_city, assignee_state, assignee_county, assignee_fips, 
         assignee_lat, assignee_long)
colnames(inventor_cities) <- c("city", "state_2000", "county_2000", "fips_2000", "lat", "long")
colnames(assignee_cities) <- c("city", "state_2000", "county_2000", "fips_2000", "lat", "long")
cities <- bind_rows(inventor_cities, assignee_cities) %>% 
  mutate(county_2000 = str_remove(county_2000, pattern = " county| city| parish"),
         city = tolower(city)) %>% 
  distinct()
cities <- st_as_sf(cities, coords = c("long", "lat"), crs = 4326)

# Get county borders for each year
years <- c(1850, 1860, 1870, 1880)
borders <- lapply(years, function(year){
  date <- paste0(year, "-01-01")
  county_var <- paste0("county_", year)
  state_var <- paste0("state_", year)
  fips_var <- paste0("fips_", year)
  borders <- us_counties(date) %>% 
    select(state_abbr, name, fips, geometry) %>% 
    mutate(name = tolower(name),
           state_abbr = tolower(state_abbr)) 
  colnames(borders) <- c(state_var, county_var, fips_var, "geometry")
  return(borders)
})

# Intersect and build a city to county crosswalk for each CMF year
cities_counties <- st_join(cities, borders[[1]], join = st_within) %>%
  st_join(borders[[2]], join = st_within) %>% 
  st_join(borders[[3]], join = st_within) %>% 
  st_join(borders[[4]], join = st_within)

cities_counties %<>% 
  relocate(matches("_2000"), .after = last_col()) %>% 
  data.frame() %>% 
  select(-geometry) %>% 
  arrange(state_2000, county_2000)

# Save
write_csv(cities_counties, "~/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/county_crosswalk.csv")








