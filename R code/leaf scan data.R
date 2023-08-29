# # Fetch the data -----
library(tidyverse)
library(dataDownloader)

## from OSF DURIN ----
# Download orginal scan data
# This function doesn't work while DURIN is not public
# Download manually for now
get_file(node = "f4v9t",
         file = "PFTC6_microclimate_2022.zip",
         path = "raw_data",
         remote_path = "Vegetation/raw_data/Leaf scans")

#Unzip microclimate data
unzip("raw_data/PFTC6_microclimate_2022.zip", exdir = "raw_data/leaf_scans")
file.remove("raw_data/PFTC6_microclimate_2022.zip") #let's free some space

## Read in files ----
# Make file list
filesPFTC6 <- dir(path = "raw_data/leaf_scans", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Read in data
tempPFTC6 <- map_df(set_names(files), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_csv2(file = file, col_names = FALSE)) #important! read_csv2 reads in European format
}, .id = "File")
