######
# PCA with all samples
######

library(tidyverse)
library(janitor)

# Install ggbiplot through github
# library(devtools)
# install_github("vqv/ggbiplot")

library(ggbiplot)

# LOAD FUNCTION AT THE END OF THIS SCRIPT

# Call in map file
map <- read_csv("raw_data/File_map.csv") %>% 
    mutate(inj = rep(1:6, times = nrow(.)/6)) %>% 
    mutate(strain = unlist(lapply(str_split(.$`Sample ID`, "_"),  function(x) x[1]))) %>% 
    mutate(pos = unlist(lapply(str_split(.$`Sample ID`, "_"),  function(x) x[2]))) %>% 
    mutate(rep = unlist(lapply(str_split(.$`Sample ID`, "_"),  function(x) x[3]))) %>% 
    filter(inj == 1)

daf16.all <- c("daf16", "N2", "M9", "MilliQ") # samples of interest for the daf-16 dataset

map.daf16.all <- map %>% 
    filter(strain %in% daf16.all) %>% 
    filter(`Sample ID` != "N2_P2_W2_1")  #this file was dropped from the feature table

# Call in feature table
features <- read_tsv("raw_data/ComBat_mzcalibrated_untargeted_averaged_featuretable.txt") %>% 
    select(mz, time, starts_with("VK")) 

# Remove .mzXML tag in column names
names(features) <- gsub(".mzXML", "", names(features))

# Feature table with m9, milliq, daf16 and n2 samples
features.daf16.all <- features %>% 
    select(mz, time, map.daf16.all$`File Name`)

# Transpose feature table - easier to manipulate
feat.daf16.all.trans <- feature.transpose(features.daf16.all)

# Add labels to the feature table to make PCA plot
map.label <- map.daf16.all %>% 
    select(File.Name = `File Name`, strain)

# Adding this to feature table
feature.all.label <- feat.daf16.all.trans  %>% 
    left_join(map.label, by = "File.Name") 

# Save this file for other analyses
feature.all.label %>% 
    select(File.Name, strain, everything()) %>% 
    write_tsv("results/features_labels_transposed.txt")

#### PCA plot
pca.all <- feature.all.label %>% 
    select(-File.Name, -strain) %>% 
    prcomp(., scale. = T, center = T)

# biplot
ggbiplot(pca.all, ellipse = T, 
         groups =  factor(feature.all.label$strain), var.axes =  F, obs.scale = 1, var.scale = 1) +
    theme_minimal() +
    labs(title = "PCA: Worm samples cluster differently from the M9 \nand MilliQ samples")


####################################
# Create a function to transpose and create a single indicator for each feature
feature.transpose <- function(featuretable){
    featuretable %>% 
        mutate(mz_time = paste0(mz, "_", time)) %>% 
        select(mz_time, everything(), -mz, -time) %>% 
        t(.) %>% 
        as.data.frame() %>% 
        row_to_names(1) %>% 
        rownames_to_column(var = "File.Name") %>% 
        mutate_if(is.factor, ~ as.numeric(as.character(.x))) %>% 
        as_tibble()
}
