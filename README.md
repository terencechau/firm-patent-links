# firm-patent-links
This repository serves as a replication package for Linking Historical Patents to Firms: Comparing Supervised Learning to Handlinking Approaches. The folders contain scripts used in both to train the supervised learning models in the paper, and to create an alternative, handlinked dataset:

* `prep_patent_data.R`: Prepares CUSP data for linking tasks.
* `border_fixes.R`: Takes the CUSP geolocation data, which pins each patent to a city in the year 2000, and remaps it back to historical, year appropriate counties.
* `handlinks/company_linking.R`: Takes all 1850-1870 CMF establishments with company-style names and matches them to all 1840-1900 patents with company-style assignees by string distance, where company-style refers to establishments whose names do not refer to sole ownership. The resulting dataset is filtered to potential matches whose Jaro-Winkler string distance is 0.2 or smaller. Potential handmatches in this set are then determined to be matches or non-matches manually.
* `handlinks/people_linking.R`: Takes all 1850-1870 CMF establishments with peo- ple’s names and matches them to all 1840-1900 patents with inventors or assignees with people’s names by string distance, where potential matches are determined by having a first and last name Jaro-Winkler of 0.2. A sample of potential handmatches in this set are then determined to be matches or non-matches manually.
* `handlinks/people_linking_2.R`: After manual linking of the output from `people_linking.R`, collects and summarizes the manual links.
* `machine_links/prep_matching_sheet.R`: Prepares sheets for handlinking of training data.
* `machine_links/summarize_matches.R`: Collects the sheets from the previous script to create a training dataset.
* `machine_links/linkage_model.Rmd`: Trains a random forest and other models to predict linkage probability.
* `machine_links/linkage_model_split_names.Rmd`: Trains a random forest and other models to predict linkage probability using additional data from the Census of Population.
* `machine_links/performance_metrics.R`: Defines helper functions to measure model performance.
* `machine_links/prep_sheets_apply_model.R`: Prepares candidate matches for all other counties to apply pre-trained model.
* `machine_links/apply_model.R`: Applies pre-trained model to all other counties and creates predictions.
* `machine_links/split_large_files.R`: Splits large counties into multiple files to re- duce computational burden.
* `machine_links/collect_split_files.R`: Collects split counties.
* `machine_links/collect_links.R`: Collects all links after applying the model at scale.
