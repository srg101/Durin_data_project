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
  select(-X) |>
  # Code in the prioritizing for keeping duplicates
  mutate(priority = case_when(
    str_detect(File, "Edit") ~ 1,
    str_detect(File, "Found") ~ 2,
    TRUE ~ 3
  ))

str(tempDURIN)

# Export data for upload to OSF
write.csv(tempDURIN, "output/2023.09.07_LeafScanData_Raw.csv")

# Clean data ----
leafscans.clean = tempDURIN |>
  # Rename columns to match main dataset
  rename(envelope_ID = ID, bulk_nr_leaves_scanned = n) |>
  # Choose edited and found over original
  group_by(envelope_ID) |>
  slice_min(priority) |>
  # We have some honest duplicates thanks to the 22 Jun mixup
  # Use distinct to get rid of those
  select(-File) |>
  distinct() |>
  # Remove doubled scans
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
durin.test = read.csv("raw_data/2023.08.04_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                      na.strings=c("","NA")) |>
  left_join(leafscans.clean) |>
  mutate(bulk_nr_leaves_clean = coalesce(bulk_nr_leaves, bulk_nr_leaves_scanned))

na.check = durin.test |>
  filter(is.na(leaf_area)) |>
  relocate(c(species, siteID, day, month, project, bulk_nr_leaves, bulk_nr_leaves_scanned), .after = envelope_ID) |>
  filter(project == "Field - Traits")

table(na.check$siteID)

na.check |> group_by(species,siteID) |> summarize(n = length(day))

write.csv(na.check, "output/2023.08.29_missing scans.csv")

# Troubleshooting for missing scans -----

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

# List all files for Akuonani's uploads
# Jun 22 C3PO
            # List all the files within the folder
Jun22C3PO <- as.data.frame(dir(path = "raw_data/DURIN LEAF SCANS/2023.06.22 C3PO", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  # Rename that awkward column name
  rename(x = "dir(path = \"raw_data/DURIN LEAF SCANS/2023.06.22 C3PO\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
  # Subset the file name so it's an envelope_ID
  mutate(envelope_ID = str_sub(x, 1,7)) |>
  # Select only the envelope_ID column
  select(envelope_ID)

                     # Use the list of missing scans (na.check, above)
na.check.Jun22C3PO = na.check |>
  # Filter join the list of files in the folder to match between missing and found scans
  right_join(Jun22C3PO)

# Write output list for Sonya
write.csv(na.check.Jun22C3PO, "output/2023.09.01_FoundScans_Jun22C3PO.csv")

# Do the same for the other two folders in this zip
Jun22Spock <- as.data.frame(dir(path = "raw_data/DURIN LEAF SCANS/2023.06.22 Spock", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  rename(x = "dir(path = \"raw_data/DURIN LEAF SCANS/2023.06.22 Spock\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
  mutate(envelope_ID = str_sub(x, 1,7)) |>
  select(envelope_ID)

na.check.Jun22Spock = na.check |>
  right_join(Jun22Spock)

Jun27C3PO <- as.data.frame(dir(path = "raw_data/DURIN LEAF SCANS/2023-06-27 c3po", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  rename(x = "dir(path = \"raw_data/DURIN LEAF SCANS/2023-06-27 c3po\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
  mutate(envelope_ID = str_sub(x, 1,7)) |>
  select(envelope_ID)

na.check.Jun27C3PO = na.check |>
  right_join(Jun27C3PO)

write.csv(na.check.Jun27C3PO, "output/2023.09.01_FoundScans_Jun27C3PO.csv")

# Do this again with the DURIN folder re-uploaded to OSF ----
               # List all the files within the folder
AllUSBScans <- as.data.frame(dir(path = "raw_data/DURIN", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  # Rename that awkward column name
  rename(x = "dir(path = \"raw_data/DURIN\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
         # Subset the file name so it's an envelope_ID
  mutate(envelope_ID = str_sub(x, -12, -6),
         # Subset so we know where each scan is
         filepath = str_sub(x, 1, -14)) |>
  # Drop the unused column
  select(-x)

                    # Use the list of missing scans (na.check, above)
na.check.AllUSBScans = na.check |>
  # Something isn't working with the filter join
  # Workaround: add column for filtering just the missing scans
  mutate(status = "missing") |>
  # Filter join the list of found scans (but note this isn't working!
  # It will return all the found scans regardless of if they match na.check)
  right_join(AllUSBScans, by = "envelope_ID") |>
  # Filter to only the relevant scans
  filter(status == "missing")

# Export for Sonya
table(na.check.AllUSBScans$filepath)

write.csv(na.check.AllUSBScans, "output/2023.09.01_FoundScans_AllUSBScans.csv")

# Do this again with all the Pi folders re-uploaded to OSF ----
# List all the files within the folder
AllPiScans <- as.data.frame(dir(path = "raw_data/Pi's leaf scans", pattern = ".jpeg", full.names = FALSE, recursive = TRUE)) |>
  # Rename that awkward column name
  rename(x = "dir(path = \"raw_data/Pi's leaf scans\", pattern = \".jpeg\", full.names = FALSE, recursive = TRUE)") |>
  # Subset the file name so it's an envelope_ID
  mutate(envelope_ID = str_sub(x, -12, -6),
         # Subset so we know where each scan is
         filepath = str_sub(x, 1, -14)) |>
  # Drop the unused column
  select(-x)

# Use the list of missing scans (na.check, above)
na.check.AllPiScans = na.check |>
  # Something isn't working with the filter join
  # Workaround: add column for filtering just the missing scans
  mutate(status = "missing") |>
  # Filter join the list of found scans (but note this isn't working!
  # It will return all the found scans regardless of if they match na.check)
  right_join(AllPiScans, by = "envelope_ID") |>
  # Filter to only the relevant scans
  filter(status == "missing") |>
  # Select the relevant columns
  select(envelope_ID, siteID, filepath)

na.check.stillmissing = na.check |>
  anti_join(AllPiScans)

write.csv(na.check.stillmissing, "output/2023.09.07_StillMissingScans.csv")

# Export for Sonya
table(na.check.AllPiScans$filepath)
table(na.check.AllPiScans$siteID)
table(na.check$siteID)

write.csv(na.check.AllPiScans, "output/2023.09.05_FoundScans_Pi's leaf scans.csv")

# Move the files to a new one so we can visually inspect all the found scans ----
# This isn't tidy, sorry...
library(filesstrings)

file.mover = na.check.AllPiScans |>
  mutate(destination = paste0("output/found_scans/", filepath),
         filename = paste0("raw_data/Pi's leaf scans/",filepath, "/",envelope_ID, ".jpeg")) |>
  distinct()

move_files(file.mover$filename, file.mover$destination)

