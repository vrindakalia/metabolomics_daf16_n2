#######
# Extraction information
#######

library(tidyverse)
#install.packages("janitor")
library(janitor)

# Call in feature table
# Change pathname to location of file on your computer
features_meta <- read_tsv("raw_data/ComBat_mzcalibrated_untargeted_averaged_featuretable.txt") %>% 
select(-starts_with("VK"))

features_meta[1:5,1:5]


