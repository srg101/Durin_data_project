# How to clean DURIN data by cross-checking fields
# This script should be used on the `durin` object generated in `clean_trait_data.R`
# All outputs should be recorded to the data cleaning tabs in the Google Sheet

# Run TidyLog at all times to catch errors
library(tidylog)

# Useful custom functions ----
## Look up an individual envelope ID number to see its info
# Modify the select() to change which data are displayed
check = function(barcode) {
  data = durin |>
    select(envelope_ID, siteID,
           DURIN_plot, ageClass, DroughtTrt, DroughNet_plotID,
           plotNR, habitat,
           species, plant_nr, leaf_nr, plant_height, wet_mass_g,
           leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm) |>
    filter(envelope_ID == barcode)
  data
}

# Metadata: Check leaves are correctly labelled ----
## DURIN plot info ----
### Does the envelope site match the plot site? ----
error.durin.site = durin |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv")) |>
  # Create abbreviated site codes
  mutate(site.code = case_when(
    siteID == "Lygra" ~ "LY",
    siteID == "Sogndal" ~ "SO",
    siteID == "Tjøtta" ~ "TJ",
    siteID == "Senja" ~ "SE",
    siteID == "Kautokeino" ~ "KA",
    TRUE ~ "unknown"
  )) |>
  # Filter to problem envelopes
  filter(site.code != site.abbrv) |>
  # Make it human readable
  relocate(c(site.code, site.abbrv), .after = envelope_ID)

### Does the envelope habitat match the DURIN plot habitat? ----
error.durin.habitat = durin |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv")) |>
  # Create abbreviated species codes
  mutate(habitat.assigned = case_when(
    habitat == "Open" ~ "O",
    habitat == "Forested" ~ "F",
    TRUE ~ "unknown"
  )) |>
  # Filter to problem leaves
  filter(habitat.assigned != habitat.abbrv) |>
  # Make it human readable
  relocate(c(habitat.assigned, habitat.abbrv), .after = envelope_ID)

### Does the PlotNR field match the DURIN plot number? ----
error.durin.plotnr = durin |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv"), remove = FALSE) |>
  # Filter to problem envelopes
  filter(plotNR != rep.abbrv) |>
  # Make it human readable
  relocate(c(plotNR, rep.abbrv, DURIN_plot), .after = envelope_ID)

### Does the envelope species match the DURIN plot species? ----
error.durin.spp = durin |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv")) |>
  # Create abbreviated species codes
  mutate(spp.code = case_when(
    species == "Calluna vulgaris" ~ "CV",
    species == "Vaccinium vitis-idaea" ~ "VV",
    species == "Vaccinium myrtillus" ~ "VM",
    species == "Empetrum nigrum" ~ "EN",
    TRUE ~ "unknown"
  )) |>
  # Filter to problem leaves
  filter(spp.code != spp.abbrv) |>
  # Make it human readable
  relocate(c(spp.code, spp.abbrv), .after = envelope_ID)

### How many plants are in each plot? ----
# RECOMMENDED you run this AFTER updating all the other fields listed above
# Calculate the maximum number of plants in each plot
durin.max.plant_nr = durin |>
  # Select the columns needed
  select(DURIN_plot, DroughNet_plotID, species, plant_nr) |>
  # Remove duplicate data
  distinct() |>
  # Sort by plant number and slice the maximum
  group_by(DURIN_plot, DroughNet_plotID, species) |>
  slice_max(plant_nr) |>
  rename(max.plant.n = plant_nr)

error.durin.plots = durin |>
  # Bring to just the level of unique plants
  select(siteID, DURIN_plot, plant_nr, plant_height) |>
  drop_na(DURIN_plot) |>
  distinct() |>
  # Summarize
  group_by(siteID, DURIN_plot) |>
  summarize(n = length(plant_height)) |>
  ungroup() |>
  # Add in max plant nr data
  left_join(durin.max.plant_nr) |>
  # Filter to the errors
  filter(n > max.plant.n)


## DroughtNet plot info ----
### ageClass -----
### Drought treatment ----

## Replicates ----
### Plant number -----
# Which envelopes are missing plant number?
error.durin.plantnr.missing = durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter out known issues (wrong species)
  filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
                             "CMH5663", "DAI1197")) |>
  # Filter to NA values
  filter(is.na(plant_nr))

nr.missing.list = as.list(error.durin.plantnr.missing$envelope_ID)

# Which plots are missing plants?
## SEE CODE ABOVE as this is done separately for DURIN and DroughtNet plots

### Leaf number ----
# Which envelopes are missing leaf numbers?
error.durin.leafnr.missing = durin |>
  # Filter to NA values
  filter(is.na(leaf_nr))

### Leaf age ----
# Which envelopes are missing leaf ages?
error.durin.age.missing = durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter to NA values
  filter(is.na(leaf_age))

age.missing.list = as.list(error.durin.age.missing$envelope_ID)



# Data: Check that leaves are correctly measured ----
## Do plant heights match between fieldsheet and envelope? ----
error.durin.height.W.fielddata = durin |>
  # Rename column
  rename(plant_height.entered = plant_height) |>
  # Join in field sheet data
  left_join(read.csv("raw_data/Plant height from field sheet.csv", na.strings=c("","NA"))) |>
  mutate(plant_height.entered = as.numeric(plant_height.entered),
         plant_height = as.numeric(plant_height)) |>
  # Filter out plants that don't have field sheet data
  drop_na(plant_height) |>
  # Filter to leaves with incorrect heights
  filter(plant_height != plant_height.entered) |>
  relocate(c(plant_height, plant_height.entered, siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age, leaf_nr),
           .after = envelope_ID) |>
  arrange(siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age, leaf_nr)

write.csv(error.durin.height.Wfielddata, "output/error.durin.height.withFieldData.csv")

## Do plant heights match between individual plants? ----
error.durin.height = durin |>
  # make list of plants with more than one height
  select(siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, plant_height) |>
  distinct() |>
  group_by(siteID, DURIN_plot, DroughNet_plotID, species, plant_nr) |>
  summarize(n = length(plant_height)) |>
  filter(n > 1) |>
  # Join back in large dataset to find envelope IDs
  left_join(durin) |>
  relocate(c(plant_height, siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age, leaf_nr),
           .after = envelope_ID) |>
  arrange(siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age, leaf_nr)

# make manual judgement calls about which height needs correcting using Excel (sorry...)
write.csv(error.durin.height, "output/error.durin.height.comparison.csv")
