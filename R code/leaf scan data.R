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
  rename(envelope_ID = ID, bulk_nr_leaves_scanned = n) |>
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

##

# testing bulk leaf replacement function
# https://stackoverflow.com/questions/50010196/replacing-na-values-from-another-dataframe-by-id
durin.test = durin |>
  left_join(leafscans.clean) |>
  mutate(bulk_nr_leaves_clean = coalesce(bulk_nr_leaves, bulk_nr_leaves_scanned))

na.check = durin.test |>
  filter(is.na(leaf_area)) |>
  relocate(c(species, siteID, day, month, project, bulk_nr_leaves, bulk_nr_leaves_scanned), .after = envelope_ID) |>
  filter(project == "Field - Traits")

table(na.check$siteID)

write.csv(na.check, "output/2023.08.29_missing scans.csv")

# List all files in Tjøtta
filesTjotta <- as.data.frame(dir(path = "raw_data/Tjøtta", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  rename(x = "dir(path = \"raw_data/Tjøtta\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
  mutate(envelope_ID = str_sub(x, -12, -7)) |>
  select(envelope_ID)

na.check.Tjotta = na.check |>
  right_join(filesTjotta)

# List all files in Sogndal
filesSogndal <- as.data.frame(dir(path = "raw_data/DURIN", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  rename(x = "dir(path = \"raw_data/DURIN\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
  mutate(envelope_ID = str_sub(x, -12, -7)) |>
  select(envelope_ID)

na.check.Sogndal = na.check |>
  right_join(filesSogndal)
