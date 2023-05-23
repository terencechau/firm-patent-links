# Compare variables when using and not using Census names

library(tidyverse)

load("/Users/terencechau/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model.RData")
cmf_patents_no_census <- cmf_patents
rm(list = setdiff(ls(), "cmf_patents_no_census"))
load("/Users/terencechau/Dropbox/Work/UChicago/RA/RA Hornbeck/CMF Patents/Terence Working Folders/output/linkage_model_split_names.RData")
cmf_patents_census <- cmf_patents
rm(list = setdiff(ls(), c("cms_patents_census", "cmf_patents_no_census")))

# Name correlations
cor(cmf_patents_no_census$jw_name, cmf_patents_census$jw_name)
cor(cmf_patents_no_census$per_token_jw_name, cmf_patents_census$per_token_jw_name)
cor(cmf_patents_no_census$mean_jw_name, cmf_patents_census$mean_jw_name)

# Place correlations
cor(cmf_patents_no_census %>% select(jw_city) %>% drop_na() %>% pull(jw_city), 
    cmf_patents_census %>% select(jw_city) %>% drop_na() %>% pull(jw_city))
cor(cmf_patents_no_census %>% select(jw_city) %>% drop_na() %>% pull(jw_city), 
    cmf_patents_census %>% select(jw_city) %>% drop_na() %>% pull(jw_city))
cor(cmf_patents_no_census %>% select(mean_jw_city) %>% drop_na() %>% pull(mean_jw_city), 
      cmf_patents_census %>% select(mean_jw_city) %>% drop_na() %>% pull(mean_jw_city))

# Year gap
cor(cmf_patents_no_census$year_gap, cmf_patents_census$year_gap)

# Matching names
names <- data.frame(cmf_patents_census$establishment_name_original, cmf_patents_census$establishment_name, cmf_patents_no_census$establishment_name) %>% 
  mutate(cmf_patents_census.establishment_name = gsub(x = cmf_patents_census.establishment_name, pattern = "\\.", replacement = " "),
         cmf_patents_census.establishment_name = str_squish(cmf_patents_census.establishment_name),
         cmf_patents_census.establishment_name = str_trim(cmf_patents_census.establishment_name, side = "both"),
         cmf_patents_census.establishment_name_original = gsub(x = cmf_patents_census.establishment_name_original, pattern = "\\.", replacement = " "),
         cmf_patents_census.establishment_name_original = str_squish(cmf_patents_census.establishment_name_original),
         cmf_patents_census.establishment_name_original = str_trim(cmf_patents_census.establishment_name_original, side = "both"))

sum(names$cmf_patents_census.establishment_name_original == names$cmf_patents_no_census.establishment_name)
sum(names$cmf_patents_census.establishment_name_original == names$cmf_patents_census.establishment_name)
sum(names$cmf_patents_census.establishment_name == names$cmf_patents_no_census.establishment_name)

patent_names <- data.frame(cmf_patents_census$patent_name, cmf_patents_no_census$patent_name)
sum(patent_names$cmf_patents_census.patent_name == patent_names$cmf_patents_no_census.patent_name)
