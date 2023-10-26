# How to clean DURIN data by cross-checking fields
# This script should be used on the `durin` object generated in `clean_trait_data.R`
# All outputs should be recorded to the data cleaning tabs in the Google Sheet

# Run TidyLog at all times to catch errors
library(tidylog)

# Useful custom functions ----
## Look up an individual envelope ID number to see its info ----
# Modify the select() to change which data are displayed
check <- function(barcode) {
  data <- durin |>
    select(envelope_ID, siteID,
           DURIN_plot, ageClass, DroughtTrt, DroughNet_plotID,
           plotNR, habitat,
           species, plant_nr, leaf_nr, leaf_age, calluna_shoot_type,
           leaf_nr, plant_height, wet_mass_g,
           leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm) |>
    filter(envelope_ID == barcode)
  data
}

# Metadata: Check leaves are correctly labelled ----
## Young/old ----
error.oldVM <- durin |>
  # Filter to young-only plants
  filter(species %in% c("Calluna vulgaris", "Vaccinium myrtillus")) |>
  # Filter to mislabelled leaf ages
  filter(leaf_age == "old")

## Find duplicate barcodes ----
error.duplicates <- durin |>
  # We don't care about exact duplicates so distinct() merges those
  distinct() |>
  # Then this filters to only duplicates
  group_by(envelope_ID) %>%
  filter(n() > 1)

## DURIN plot info ----
### Does the envelope site match the plot site? ----
error.durin.site <- durin |>
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
error.durin.habitat <- durin |>
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
error.durin.plotnr <- durin |>
  #Filter to just DURIN
  drop_na(DURIN_plot) |>
  # Separate out the plot ID for identifying species
  separate(DURIN_plot, into = c("site.abbrv", "habitat.abbrv", "spp.abbrv", "rep.abbrv"), remove = FALSE) |>
  # Filter to problem envelopes
  filter(plotNR != rep.abbrv) |>
  # Make it human readable
  relocate(c(plotNR, rep.abbrv, DURIN_plot), .after = envelope_ID)

### Does the envelope species match the DURIN plot species? ----
error.durin.spp <- durin |>
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
durin.max.plant_nr <- durin |>
  # Select the columns needed
  select(DURIN_plot, DroughNet_plotID, species, plant_nr) |>
  # Remove duplicate data
  distinct() |>
  # Sort by plant number and slice the maximum
  group_by(DURIN_plot, DroughNet_plotID, species) |>
  slice_max(plant_nr) |>
  rename(max.plant.n = plant_nr)

error.durin.plots <- durin |>
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
error.durin.plantnr.missing <- durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter out known issues (wrong species)
  filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
                             "CMH5663", "DAI1197")) |>
  # Filter to NA values
  filter(is.na(plant_nr))

nr.missing.list <- as.list(error.durin.plantnr.missing$envelope_ID)

# Which plots are missing plants?
## SEE CODE ABOVE as this is done separately for DURIN and DroughtNet plots

### Leaf number ----
check.plant <- function(plot, plantnr) {
  data = durin |>
    select(envelope_ID, siteID,
           DURIN_plot,
           plotNR, habitat,
           species, plant_nr, leaf_nr, leaf_age, plant_height,
           calluna_shoot_type) |>
    filter(DURIN_plot == plot & plant_nr == plantnr) |>
    arrange(plant_nr, calluna_shoot_type, leaf_age, leaf_nr)
  data
}

# Which envelopes are missing leaf numbers?
error.durin.leafnr.missing <- durin |>
  # Filter to NA values
  filter(is.na(leaf_nr))

# Which plants are missing leaves (or have extras)?
error.durin.leaves <- durin %>%
  # Select just the relevant data
  drop_na(DURIN_plot) |>
  filter(project == "Field - Traits") |>
  dplyr::group_by(siteID, DURIN_plot, plant_nr, leaf_age, calluna_shoot_type) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n != 3)

### Leaf age ----
# Which envelopes are missing leaf ages?
error.durin.age.missing <- durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter to NA values
  filter(is.na(leaf_age))

age.missing.list <- as.list(error.durin.age.missing$envelope_ID)

# Data: Check that leaves are correctly measured ----
## Do plant heights match between fieldsheet and envelope? ----
error.durin.height.W.fielddata <- durin |>
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
error.durin.height <- durin |>
  # Filter out Phys team
  filter(project == "Field - Traits") |>
  # make list of plants with more than one height
  select(siteID, DURIN_plot, DroughNet_plotID, ageClass, species, plant_nr, plant_height) |>
  distinct() |>
  group_by(siteID, DURIN_plot, DroughNet_plotID, ageClass, species, plant_nr) |>
  summarize(n = length(plant_height)) |>
  filter(n > 1) |>
  # Join back in large dataset to find envelope IDs
  left_join(durin) |>
  relocate(c(plant_height, siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age, leaf_nr),
           .after = envelope_ID) |>
  arrange(siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age, leaf_nr)

# make manual judgement calls about which height needs correcting using Excel (sorry...)
write.csv(error.durin.height, "output/error.durin.height.comparison.csv")

## Do wet and dry mass hold an expected relationship? ----
library(ggh4x)

ggplot(durin,
       aes(x = wet_mass_g, y = dry_mass_g, color = leaf_age)) +
  geom_point(alpha = 0.5) +
  facet_wrap2(~species, scales = "free") +
  theme_bw()

ggsave("visualizations/2023.10.18_dry.wetMass_scaledtobulkleafnr.png")

ggplot(durin |> mutate(dry_mass_g = as.numeric(dry_mass_g))|>
         mutate(mass_ratio = dry_mass_g/wet_mass_g),
       aes(x = species, y = mass_ratio, color = leaf_age)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  geom_point(data = durin |> mutate(mass_ratio = dry_mass_g/wet_mass_g) |>
               filter(envelope_ID %in% reweigh), color = "red") +
  facet_wrap2(~species, scales = "free") +
  theme_bw()

ggsave()

# Identify the outliers
error.massratio <- durin |>
  # Make dry mass numeric
  mutate(dry_mass_g = as.numeric(dry_mass_g)) |>
  # Make ratio of dry to wet mass
  mutate(mass_ratio = dry_mass_g/wet_mass_g) |>
  # Outlier values are eyeballed from the above graph
  mutate(flag = case_when(
    species == "Calluna vulgaris" & wet_mass_g > 0.1 ~ "high wet_mass_g",
    species == "Calluna vulgaris" & dry_mass_g > 0.007 ~ "high dry_mass_g",
    species == "Empetrum nigrum" & wet_mass_g > 0.05 ~ "high wet_mass_g",
    species == "Empetrum nigrum" & dry_mass_g > 0.01 ~ "high dry_mass_g",
    species == "Vaccinium myrtillus" & wet_mass_g > 0.1 ~ "high wet_mass_g",
    species == "Vaccinium myrtillus" & dry_mass_g > 0.027 ~ "high dry_mass_g",
    species == "Vaccinium vitis-idaea" & wet_mass_g > 0.25 ~ "high wet_mass_g",
    species == "Vaccinium vitis-idaea" & dry_mass_g > 0.075 ~ "high dry_mass_g",
    species == "Calluna vulgaris" & mass_ratio > 0.75 ~ "high dry to wet mass ratio",
    species == "Empetrum nigrum" & mass_ratio > 0.75 ~ "high dry to wet mass ratio",
    species == "Vaccinium myrtillus" & mass_ratio > 0.6 ~ "high dry to wet mass ratio",
    species == "Vaccinium myrtillus" & mass_ratio < 0.2 ~ "low dry to wet mass ratio",
    species == "Vaccinium vitis-idaea" & mass_ratio > 0.75 ~ "high dry to wet mass ratio",
    species == "Vaccinium vitis-idaea" & mass_ratio < 0.2 ~ "low dry to wet mass ratio",
    dry_mass_g > wet_mass_g ~ "dry mass more than wet mass",
    TRUE ~ "ok"
  )) |>
  # Filter to the problem columns
  filter(flag != "ok")|>
  # Prep for the envelope check sheet
  mutate(`Supporting Text Comment` = paste0("Check envelope", ". ", flag)) |>
  select(envelope_ID, `Supporting Text Comment`, mass_ratio) |>
  arrange(`Supporting Text Comment`)

# For some reason the flag for dry mass greater than wet doesn't work
table(error.massratio$flag)

write.csv(error.massratio, "output/2023.10.10_error.massratio.csv")

## Do SLA and leaf area hold an expected relationship? ----
### Visualize outliers

ggplot(durin,
       aes(x = leaf_area, y = SLA, color = leaf_age)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~species, scales = "free", ncol = 4) +
  theme_bw()

ggsave("visualizations/2023.10.18_SLAxAreaErrors_scaledtobulkleafnr.png")

# From these visuals, we can estimate reasonable values
error.SLAxArea <- error.sla |>
  # Flag possible errors
  mutate(flag_SLA = case_when(
    species == "Calluna vulgaris" & (SLA < 25 | SLA > 150) ~ "CV SLA error",
    species == "Empetrum nigrum" & (SLA < 40 | SLA > 200) ~ "EN SLA error",
    species == "Vaccinium myrtillus" & (SLA < 125 | SLA > 400) ~ "VM SLA error",
    species == "Vaccinium vitis-idaea" & (SLA < 40 | SLA > 300) ~ "VV SLA error",
    TRUE ~ "okay"
  ),
  flag_area = case_when(
    species == "Calluna vulgaris" & (leaf_area_scaled < 0 | leaf_area_scaled > 0.3) ~ "CV area error",
    species == "Empetrum nigrum" & (leaf_area_scaled < 0 | leaf_area_scaled > 0.2) ~ "EN area error",
    species == "Vaccinium myrtillus" & (leaf_area_scaled < 0 | leaf_area_scaled > 3.5) ~ "VM area error",
    species == "Vaccinium vitis-idaea" & (leaf_area_scaled < 0 | leaf_area_scaled > 3.5) ~ "VV area error",
    TRUE ~ "okay"
  )) |>
  filter(flag_SLA != "okay" | flag_area != "okay") |>
  select(envelope_ID, flag_SLA, flag_area, wet_mass_g, dry_mass_g, leaf_area, SLA) |>
  distinct()

write.csv(error.SLAxArea, "output/2023.10.12_SLAxAreaErrors.csv")

## Are all values sensible in relation to the mean values? ----
### Calculate means
library(rstatix)
durin.means <- durin |>
  select(plant_height:leaf_thickness_3_mm) |>
  get_summary_stats(type = "mean_ci")

### Thickness errors ----
error.durin.thickness <- durin |>
  # Values pulled from durin.means object
  filter(leaf_thickness_1_mm > 0.290*10 |
           leaf_thickness_2_mm > 0.412*10 |
           leaf_thickness_3_mm > 0.321*10 |
           leaf_thickness_1_mm < 0.290/10 |
           leaf_thickness_2_mm < 0.412/10 |
           leaf_thickness_3_mm < 0.321/10) |>
  relocate(c(leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm), .after = envelope_ID)

### Weight errors ----
error.durin.weight <- durin |>
  # Values pulled from durin.means object
  filter(wet_mass_g > 0.021 *10 |
           wet_mass_g < 0.021/10) |>
  relocate(wet_mass_g, .after = envelope_ID)

### Height errors ----
error.durin.height <- durin |>
  # Values pulled from durin.means object
  filter(plant_height > 19.487 * 5 |
           plant_height < 19.487/10) |>
  relocate(plant_height, .after = envelope_ID)

# Dry mass error checks ----
## Which barcodes are duplicates? ----
# Check for duplicates with different dry_mass_g entries
error.drymass.duplicate <- durin |>
  left_join(durin.drymass) |>
  # We don't care about exact duplicates so distinct() merges those
  select(envelope_ID, dry_mass_g) |>
  distinct() |>
  # Then this filters to only duplicates
  group_by(envelope_ID) %>%
  filter(n() > 1) |>
  # Mark as duplicates
  mutate(flag = "one barcode, two dry_mass_g")

# write.csv(error.drymass.duplicate, "output/2023.10.06_errorcheck_drymass.csv")

## Which barcodes are in the main datasheet but don't have dry mass? ----
error.drymass.missing <- durin |>
  # Filter to missing ones
  filter(is.na(dry_mass_g_original)) |>
  # Mark as missing
  mutate(flag = "missing dry_mass_g")

# Double-check these aren't in the Lygra discarded barcodes
error.drymass.missing.double <- error.drymass.missing |>
  right_join(read.csv("raw_data/Lygra barcodes to discard - Sheet2.csv"))

write.csv(error.drymass.missing, "output/2023.10.17_errorcheck_drymass_missing.csv")

## Which barcodes are in the DryMassCheck sheet but not the main spreadsheet? ----
error.drymass.extra <- durin |>
  # Bring together both the leaf barcodes and cutout barcodes
  # This is weird but probably works
  # Select the smaller list of barcodes
  select(cutout_barcode) |>
  # Remove the NAs
  drop_na() |>
  # Rename so the bind works
  rename(envelope_ID = cutout_barcode) |>
  # Bind in the larger list of barcodes
  bind_rows(durin) |>
  # Select the only relevant column
  select(envelope_ID) |>
  # Make a column to know this barcode is in the big datasheet
  mutate(present = "x") |>
  # Join DryMass list
  right_join(durin.drymass) |>
  # Filter to barcodes that weren't in the main datasheet
  filter(is.na(present)) |>
  arrange(envelope_ID) |>
  # Mark as extra
  mutate(flag = "extra (in DryMassCheck but not main datasheet")

## Double-check it's working with the cutouts
error.drymass.double <- durin |>
  select(cutout_barcode) |>
  rename(envelope_ID = cutout_barcode) |>
  # Use a filtering join
  inner_join(error.drymass.extra)

# write.csv(error.drymass.extra, "output/2023.10.10_errorcheck_drymass_extra.csv")

## Make large object with all the different DryMassCheck errors ----
error.drymass <- error.drymass.duplicate |>
  bind_rows(error.drymass.extra, error.drymass.missing) |>
  # This object is from `barcode positions.R`
  left_join(barcodes) |>
  # Select relevant columns
  select(envelope_ID, dry_mass_g, barcode_below, barcode_above, flag) |>
  distinct() |>
  arrange(envelope_ID) |>
  write.csv("output/2023.10.11_error.drymass_with.barcodes.csv")

# Scanning error checks ----
## Does SLA x area look right?

# Look up past attempts to fix errors ----
# Curious if someone has already tried to fix the error you found?
# This object and function will check if an individual envelope has already been inspected
# and tell you the outcome of that inspection.
# You need to download the data check tabs from the big Google Sheet
# NOTE that Round 3 checks have NOT been completed as of 2023.09.11

# Compile check object
envelope.checks <- read.csv("raw_data/DURIN Plant Functional Traits - 2023.07.31_Rd1_MeasurementChecks.csv") |>
  mutate(round = "1") |>
  bind_rows(read.csv("raw_data/DURIN Plant Functional Traits - Rd 2 measurement checks.csv")) |>
  mutate(round = replace_na(round, "2")) |>
  bind_rows(read.csv("raw_data/DURIN Plant Functional Traits - Rd 3 measurement checks.csv")) |>
  mutate(round = replace_na(round, "3")) |>
  select(-c(Variable_data, Explanation, Variable.to.fix_data, X)) |>
  relocate(envelope_ID, Variable.with.error, Supporting.Text.Comment, Fix)

# Function to look up fixes
check.fix <- function(barcode){
  data = envelope.checks |>
    filter(envelope_ID == barcode)
  data
}
