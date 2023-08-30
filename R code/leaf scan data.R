# # Fetch the data -----
library(tidyverse)
library(dataDownloader)
library(tidylog)

## from OSF DURIN
# Download original scan data ----
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
}, .id = "File") |>
  select(-X)

str(tempDURIN)

# Export data for upload to OSF
write.csv(tempDURIN, "output/2023.08.29_LeafScanData_FirstRound.csv")

# Clean data ----
leafscans.clean = tempDURIN |>
  # Drop the file path for the distinct to work
  select(-File) |>
  # Rename columns to match main dataset
  rename(envelope_ID = ID, bulk_nr_leaves = n) |>
  # Drop accidental duplicates
  distinct() |>
  mutate(cutting = case_when(
    # One phys team scan has two values
    # Keeping the earliest one
    envelope_ID == "BLB9742" & leaf_area < 4.9 ~ "cut",
    # Duplicates of 'out.jpeg' will need solving at some point
    envelope_ID == "out.jpe" ~ "cut",
    TRUE ~ "keep")) |>
  filter(cutting == "keep") |>
  select(-cutting)

# Check that the duplicate scans successfully filtered
dup.check = leafscans.clean |>
  select(envelope_ID) |>
  filter(duplicated(envelope_ID))


