#### USE THIS PART TO CLEAN THE DATA ####
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
#Unzip leaf area data
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
# write.csv(tempDURIN, "output/2023.09.27_LeafScanData_Raw.csv")

# Clean data ----
leafscans.clean = tempDURIN |>
  # Rename columns to match main dataset
  rename(envelope_ID = ID, bulk_nr_leaves_scanned = n) |>
  # Choose edited and found over original
  # Code in the prioritizing for keeping duplicates
  mutate(priority = case_when(
    str_detect(File, "round2") ~ 0,
    str_detect(File, "Edit") ~ 1,
    str_detect(File, "Found") ~ 2,
    TRUE ~ 3
  )) |>
  group_by(envelope_ID) |>
  # Select only the most relevant
  slice_min(priority) |>
  # Rename the known 'out.jpeg' scans
  mutate(envelope_ID = case_when(
    envelope_ID == "out.jpe" & File == "raw_data/leaf_scans/DURIN_2023_Leaf_Areas/Tjotta_2023.07.06Spock_leaf_area.csv" ~ "ECT1701",
    envelope_ID == "out.jpe" & File == "raw_data/leaf_scans/DURIN_2023_Leaf_Areas/Senja_2023-07-11_deathstar_leaf_area.csv" ~ "BQV6602",
    TRUE ~ envelope_ID)) |>
  # We have some honest duplicates thanks to the 22 Jun mixup
  # Use distinct to get rid of those
  dplyr::select(-c(File)) |>
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

## Code to add leaf areas to the main DURIN object ----
# To be moved to the main cleaning script once finished
durin.leafarea = durin |>
  # Join in the leaf area scans
  left_join(leafscans.clean) |>
  # Manually replace bulk_nr_leaves for ones the scanned number is more accurate
  mutate(bulk_nr_leaves = case_when(
    envelope_ID %in% c("BFN9270", "BHY0712", "AYP7221", "ALA0711", "BHK3198",
                       "AUW3217") ~ bulk_nr_leaves_scanned,
    envelope_ID %in% c("EES8345") ~ 2, # Leaf cut in half on scan
    TRUE ~ bulk_nr_leaves
  ),
  # Make updated bulk_nr_leaves tab
  bulk_nr_leaves_clean = coalesce(bulk_nr_leaves, bulk_nr_leaves_scanned)) |>
  # Calculate specific leaf area
  mutate(SLA.wet = leaf_area/wet_mass_g) |>
  # Calculate individual leaf values
  # Do this AFTER SLA calc to not do an average of an average
  mutate(wet_mass_g = wet_mass_g/bulk_nr_leaves_clean,
         leaf_area = leaf_area/bulk_nr_leaves_clean)

#### TROUBLESHOOTING SECTION ####
# testing bulk leaf replacement function -----
# https://stackoverflow.com/questions/50010196/replacing-na-values-from-another-dataframe-by-id
durin.test = read.csv("raw_data/2023.08.04_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                      na.strings=c("","NA")) |>
  left_join(leafscans.clean) |>
  mutate(bulk_nr_leaves_clean = coalesce(bulk_nr_leaves, bulk_nr_leaves_scanned)) |>
  relocate(c(bulk_nr_leaves_clean,bulk_nr_leaves_scanned,bulk_nr_leaves), .after = plant_height)

na.check = durin.leafarea |>
  filter(is.na(leaf_area)) |>
  relocate(c(species, siteID, day, month, project, bulk_nr_leaves, bulk_nr_leaves_scanned), .after = envelope_ID) |>
  filter(project == "Field - Traits")

table(na.check$siteID)

na.check |> group_by(species,siteID) |> summarize(n = length(day))

# write.csv(na.check, "output/2023.08.29_missing scans.csv")

## Check any mismatches in the scanned vs counted leaves ----
### Make file path object ----
scanpaths = tempDURIN |>
  # Code in the prioritizing for keeping duplicates
  mutate(priority = case_when(
    str_detect(File, "round2") ~ 0,
    str_detect(File, "Edit") ~ 1,
    str_detect(File, "Found") ~ 2,
    TRUE ~ 3
  )) |>
  # Rename columns to match main dataset
  rename(envelope_ID = ID, bulk_nr_leaves_scanned = n) |>
  # Choose edited and found over original
  group_by(envelope_ID) |>
  slice_min(priority) |>
  select(envelope_ID, File)

leafnr.check = durin.leafarea |>
  # Only working with trait team leaves
  filter(project == "Field - Traits") |>
  select(envelope_ID, siteID, bulk_nr_leaves_scanned,bulk_nr_leaves) |>
  # Not interested in the NA mismatch
  drop_na(bulk_nr_leaves) |>
  # Filter for mismatches
  filter(bulk_nr_leaves != bulk_nr_leaves_scanned) |>
  # Bring back in file paths so we can look at images
  left_join(scanpaths) |>
  # Get rid of the honest duplicates from the 22 Jun error
  group_by(envelope_ID) |>
  slice_head()

write.csv(leafnr.check, "output/2023.09.15_leafnr.check.csv")

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

## Do this again with the DURIN folder re-uploaded to OSF ----
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

## Do this again with all the Pi folders re-uploaded to OSF ----
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

write.csv(na.check.stillmissing, "output/2023.09.12_StillMissingScans.csv")

na.check.recollect = na.check.stillmissing |>
  mutate(spp.code = case_when(
    species == "Calluna vulgaris" ~ "CV",
    species == "Vaccinium vitis-idaea" ~ "VV",
    species == "Vaccinium myrtillus" ~ "VM",
    species == "Empetrum nigrum" ~ "EN",
    TRUE ~ "unknown"
  ),
    plotID = case_when(
    is.na(DURIN_plot) ~ paste0(ageClass, DroughtTrt, DroughNet_plotID, spp.code),
    is.na(ageClass) ~ DURIN_plot,
    TRUE ~ "unknown"
 ),
 missing.code = paste(plotID, "-", plant_nr)
  ) |>
  group_by(siteID, plotID, missing.code, leaf_age) |>
  summarize(n = length(envelope_ID))

write.csv(na.check.recollect, "output/2023.09.13_PlantsToReCollect.csv")

# Export for Sonya
table(na.check.AllPiScans$filepath)
table(na.check.AllPiScans$siteID)
table(na.check$siteID)

write.csv(na.check.AllPiScans, "output/2023.09.05_FoundScans_Pi's leaf scans.csv")

## Move the files to a new one so we can visually inspect all the found scans ----
library(filesstrings)

file.mover = na.check.AllPiScans |>
  mutate(destination = paste0("output/found_scans/", filepath),
         filename = paste0("raw_data/Pi's leaf scans/",filepath, "/",envelope_ID, ".jpeg")) |>
  distinct()

move_files(file.mover$filename, file.mover$destination)

# Check matched leaf scan areas ----
error.leafarea = tempDURIN |>
  # Code in the prioritizing for keeping duplicates
  mutate(priority = case_when(
    str_detect(File, "round2") ~ 0,
    str_detect(File, "Edit") ~ 1,
    str_detect(File, "Found") ~ 2,
    TRUE ~ 3
  )) |>
  slice_head(by = ID) |>
  slice_min(priority) |>
  group_by(n, leaf_area) |>
  filter(n()>1) |>
  ungroup()

# Sites and Envelope IDs
sites = durin |>
  select(envelope_ID, siteID) |>
  rename(ID = envelope_ID)

error.leafarea = tempDURIN |>
  # Code in the prioritizing for keeping duplicates
  mutate(priority = case_when(
    str_detect(File, "round2") ~ 0,
    str_detect(File, "Edit") ~ 1,
    str_detect(File, "Found") ~ 2,
    TRUE ~ 3
  )) |>
  slice_head(by = ID) |>
  group_by(ID) |>
  slice_min(priority) |>
  # join sites
  left_join(sites) |>
  group_by(siteID, n, leaf_area) |>
  filter(n()>1) |>
  ungroup()

write.csv(error.leafarea, "output/2023.09.18_PossibleDuplicateLeaves.csv")

# Outlier checks ----
## SLA x Area ----
ggplot(durin.leafarea |> drop_na(leaf_age),
       aes(x = leaf_area, y = SLA.wet, color = leaf_age)) +
  geom_point(alpha = 0.3) +
  # geom_point(data = durin.leafarea |> filter(envelope_ID %in% c("DQA1214", "DII6743", "ETH1438")), color = "red") +
  facet_wrap(~species, scales = "free", ncol = 4) +
  theme_bw()

# From these visuals, we can estimate reasonable values
error.SLAxArea = durin.leafarea |>
  # Flag possible errors
  mutate(flag_SLA = case_when(
    species == "Calluna vulgaris" & (SLA.wet < 10 | SLA.wet > 50) ~ "CV SLA error",
    species == "Empetrum nigrum" & (SLA.wet < 10 | SLA.wet > 60) ~ "EN SLA error",
    species == "Vaccinium myrtillus" & (SLA.wet < 40 | SLA.wet > 125) ~ "VM SLA error",
    species == "Vaccinium vitis-idaea" & (SLA.wet < 20 | SLA.wet > 55) ~ "VV SLA error",
    TRUE ~ "okay"
  ),
  flag_area = case_when(
    species == "Calluna vulgaris" & (leaf_area < 0 | leaf_area > 0.3) ~ "CV area error",
    species == "Empetrum nigrum" & (leaf_area < 0 | leaf_area > 0.2) ~ "EN area error",
    species == "Vaccinium myrtillus" & (leaf_area < 0 | leaf_area > 3.5) ~ "VM area error",
    species == "Vaccinium vitis-idaea" & (leaf_area < 0 | leaf_area > 3) ~ "VV area error",
    TRUE ~ "okay"
  )) |>
  filter(flag_SLA != "okay" | flag_area != "okay") |>
  select(envelope_ID, flag_SLA, flag_area)

ggsave("visualizations/2023.09.19_SLAxAreaErrors.png")

write.csv(error.SLAxArea, "output/2023.09.19_SLAxAreaErrors.csv")
