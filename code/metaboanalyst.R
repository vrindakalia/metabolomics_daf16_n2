#######
# Creating file for metaboanalyst
#######

# Things to do:
# Subtract m9 features
# call in transposed feature table
# See function at the end!

feat.labeled <- read_tsv("results/features_labels_transposed.txt")

# Get average intensity of each feature in the M9 samples
m9.avg <- feat.labeled %>% 
    filter(strain == "M9") %>% 
    select(contains("_")) %>% 
    map_dbl(mean)

# select N2 and daf16 samples
feat.comp <- feat.labeled %>% 
    filter(strain %in% c("daf16", "N2")) %>% 
    select(contains("_")) %>% 
    t(.) %>% 
    as.data.frame() 

# Save the filenames
daf.n2.names <- filter(feat.labeled, strain %in% c("daf16", "N2"))$File.Name

# Add names back to transposed table
names(feat.comp) <- daf.n2.names

head(feat.comp)

# Add a column with the average M9 feature intensities
feat.comp$m9.avg <- m9.avg

# create a new dataframe with indicator for whether feature intensity is
# 1.5 times greater than the intensity in the daf16 and n2 samples
m9.compare <- feat.comp %>% 
    select(contains("_")) %>% 
    map_df(., function(x) x > 1.5*feat.comp$m9.avg) %>% 
    as.data.frame()

# Add row names to the compare filename
m9.compare$feature <- rownames(feat.comp)

head(m9.compare)

# Add a new variable to get the number of samples a feature meets the criteria
m9.compare$prop <- apply(select(m9.compare, contains("_")), 1, mean)

#ggplot(m9.compare, aes(x = feature, y = prop)) +
#    geom_bar(stat = "identity")

# Select a feature if present in at least 50% of the samples
feat.thresh <- m9.compare %>% 
    select(prop, feature) %>% 
    filter(prop > 0.5)

# Extract features that meet the threshold
feat.good <- feat.labeled %>% 
    select("File.Name", "strain", feat.thresh$feature) %>% 
    filter(strain %in% c("daf16", "N2")) %>% 
    as.data.frame() 

# Impute missing values
feat.imputed <-  feat.good %>% 
    select(contains("_")) %>% 
    impute_half_min_if(is.numeric, ~ .x == 0)

# Create file for metaboanalyst    
feat.metaboanlayst <- feat.imputed %>% 
    mutate(strain = feat.good$strain, File.Name = feat.good$File.Name) %>% 
    select(File.Name, strain, everything()) %>% 
    t() %>% 
    as.data.frame() %>% 
    row_to_names(1)

################
# LOAD THIS FUNCTION

# Impute missing values with min/2
# Call in function
min_set <- function(vec, .p) {
    modify_if(vec, .p , ~ min(vec[vec>0], na.rm = T)/2) %>% 
        reduce(c)
}
impute_half_min_if <- function(tbl, .p, .pp) {
    modify_if(tbl, .p, ~ min_set(.x, .pp))
} 
