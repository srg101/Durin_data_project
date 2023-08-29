# # Fetch the data -----
library(tidyverse)
library(dataDownloader)

## from OSF DURIN ----
# Download orginal scan data
# This function doesn't work while DURIN is not public
# Download manually for now
get_file(node = "f4v9t",
         file = "DURIN_2023_Leaf_Areas.zip",
         path = "raw_data",
         remote_path = "Vegetation/raw_data/Leaf scans")

#Unzip microclimate data
unzip("raw_data/DURIN_2023_Leaf_Areas.zip", exdir = "raw_data/leaf_scans")
file.remove("raw_data/DURIN_2023_Leaf_Areas.zip") #let's free some space

## Read in files ----
# Make file list
filesDURIN <- dir(path = "raw_data/leaf_scans/DURIN_2023_Leaf_Areas", pattern = ".csv", full.names = TRUE, recursive = TRUE)

# Read in data
tempDURIN <- map_df(set_names(filesDURIN), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read.csv(file = file)) #important! reading in American format
}, .id = "File")

str(tempDURIN)
