# DURIN checks ----
## Visualize data to see probable errors ----
durin = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                             na.strings=c("","NA")) |>
  # Change plant height to numeric
  mutate(plant_height = as.numeric(plant_height))

# Plot all variables against each other
library(GGally)

ggpairs(durin |> select(siteID, species, habitat, plant_nr, leaf_nr, leaf_age, plant_height, bulk_nr_leaves, wet_mass_g, leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm))

png("visualizations/durin_allvariables.png", width = 24, height = 24, units = "in", res = 300)
dev.off()

## Hunt down errors ----
### Calculate means ----
library(rstatix)
durin.means = durin |>
  select(plant_height:leaf_thickness_3_mm) |>
  get_summary_stats(type = "mean_ci")

### Thickness errors ----
error.durin.thickness = durin |>
  # Values pulled from durin.means object
  filter(leaf_thickness_1_mm > 0.278*10 |
           leaf_thickness_2_mm > 0.280*10 |
           leaf_thickness_3_mm > 0.321*10 |
           leaf_thickness_1_mm < 0.278/10 |
           leaf_thickness_2_mm < 0.280/10 |
           leaf_thickness_3_mm < 0.321/10) |>
  relocate(c(leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm), .after = envelope_ID)

### Weight errors ----
error.durin.weight = durin |>
  # Values pulled from durin.means object
  filter(wet_mass_g > 0.025 *10 |
           wet_mass_g < 0.025/10) |>
  relocate(wet_mass_g, .after = envelope_ID)

### Height errors ----
error.durin.height = durin |>
  # Values pulled from durin.means object
  filter(plant_height > 17.258 * 5 |
           plant_height < 17.258/10) |>
  relocate(plant_height, .after = envelope_ID)

### Missing bulk numbers (to be counted on scan) ----
error.durin.bulk = durin |>
  # Values pulled from durin.means object
  filter(is.na(bulk_nr_leaves) &
           species %in% c("Empetrum nigrum", "Calluna vulgaris")) |>
  relocate(bulk_nr_leaves, .after = envelope_ID)

write.csv(error.durin.bulk, "output/error.durin.bulk.csv")

### Incorrect age for myrtillus and Calluna ----
error.durin.age_vm.cv = durin |>
  filter(species %in% c("Vaccinium myrtillus", "Calluna vulgaris") & leaf_age == "old") |>
  relocate(leaf_age, .after = envelope_ID)

error.list.vmcv.age = error.durin.age_vm.cv$envelope_ID

### Incorrect number of plants per plot ----
# Cross reference with the list of plant numbers
durin.max.plant_nr = durin |>
  # Select the columns needed
  select(DURIN_plot, DroughNet_plotID, species, plant_nr) |>
  # Remove duplicate data
  distinct() |>
  # Sort by plant number and slice the maximum
  group_by(DURIN_plot, DroughNet_plotID, species) |>
  slice_max(plant_nr) |>
  rename(max.plant.n = plant_nr)

error.durin.plants = durin |>
  # # Filter out known issues (wrong species)
  # filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
  #                            "CMH5663", "DAI1197")) |>
  # Select relevant columns
  select(DURIN_plot, DroughNet_plotID, species, plant_nr) |>
  distinct() |>
  # Count plants per plot
  group_by(DURIN_plot, DroughNet_plotID, species) |>
  summarize(n = length(plant_nr)) |>
  # Join in reference data
  left_join(durin.max.plant_nr) |>
  # Filter to incorrect counts
  filter(n > max.plant.n)

### Incorrect number of leaves per age ----
#### Find missing data ----
# Missing leaf age
error.durin.age.missing = durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter out known issues (wrong species)
  # filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
  #                            "CMH5663", "DAI1197")) |>
  # Filter to NA values
  filter(is.na(leaf_age))

age.missing.list = as.list(error.durin.age.missing$envelope_ID)

# Missing plant number
error.durin.plantnr.missing = durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter out known issues (wrong species)
  filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
                             "CMH5663", "DAI1197")) |>
  # Filter to NA values
  filter(is.na(plant_nr))

nr.missing.list = as.list(error.durin.plantnr.missing$envelope_ID)

#### Find mismatched ages ----
# 2023.07.25 I'm setting this aside for further thought.
# I'm not sure how to correct these errors because none of them match (total = 6)
# This could be caused by the missing NAs above though.
error.durin.age = durin |>
  # Filter to species with young and old leaves
  filter(species %in% c("Vaccinium vitis-idaea", "Empetrum nigrum")) |>
  # Filter out known issues (wrong species)
  filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
                             "CMH5663", "DAI1197")) |>
  # Filter out known issues (NA age and plant number)
  filter(!envelope_ID %in% age.missing.list) |>
  filter(!envelope_ID %in% nr.missing.list) |>
  group_by(DURIN_plot, DroughNet_plotID, species, plant_nr, leaf_age) |>
  summarize(n = length(envelope_ID)) |>
  ungroup() |>
  # Filter to the incorrect number of leaves
  filter(n != 3)

## Incorrect plant heights ----
# 2023.07.25 Something is going wrong with this
# There are two ways to get wrong height: wrong treatment/plant nr OR incorrectly entered height
# The solution may be to enter all the field sheets and check against those...
# error.durin.height = durin |>
#   # Select relevant columns
#   select(DURIN_plot, DroughNet_plotID, species, plant_nr, plant_height) |>
#   # Remove duplicates
#   distinct() |>
#   # Summarize to find single plants with multiple measurements
#   group_by(DURIN_plot, DroughNet_plotID, species, plant_nr) |>
#   summarise(n = length(plant_height)) |>
#   # Filter to erroneous ones
#   filter(n > 1) |>
#   # Drop variable with count
#   select(-n) |>
#   # Inner join with main dataset
#   inner_join(durin) |>
#   select(envelope_ID, DURIN_plot, DroughNet_plotID, species, plant_nr, plant_height) |>
#   # Make dataset of heights
#   group_by(DURIN_plot, DroughNet_plotID, species, plant_nr, plant_height) |>
#   summarize(n = length(envelope_ID)) |>
#   ungroup() |>
#   # Remove heights that are likely to be correct
#   filter(!n %in% c(3, 6))
#   # Select the least probable height
#   group_by(DURIN_plot, DroughNet_plotID, species, plant_nr) |>
#   slice_min(n) |>
#   # Inner join for list of envelope IDs
#   inner_join(durin)
#
# write.csv(error.durin.height, "output/error.durin.height.csv")

## Incorrect plant heights using field sheets ----
error.durin.height.W.fielddata = durin |>
  # # Select relevant columns
  # select(siteID, DURIN_plot, DroughNet_plotID, species, plant_nr, plant_height, leaf_age, leaf_nr) |>
  # # Remove duplicates
  # distinct() |>
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

## Incorrect heights without field sheet ----
error.durin.height.WO.fielddata = durin |>
  # Filter to plants that don't have field sheet data
  anti_join(read.csv("raw_data/Plant height from field sheet.csv",
                     na.strings=c("","NA"))) |>
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
write.csv(error.durin.height.WO.fielddata, "output/error.durin.height.withoutFieldData.csv")

# Make temporary object without erroneous leaves ----
library(tidylog)

list.nobulk = as.list(error.durin.bulk$envelope_ID)

durin.noerrors = durin |>
  # Filter out measurement errors
  filter(!envelope_ID %in% c("AUX7373", "BSD3874", "CVP9320", "DDZ3156", "DEN0101",
                             "AFH1727", "ARK3594", "ERV2714", "AYX2273", "BPF4529",
                             "CSP7326", "DSD6681", "AOQ0411")) |>
  # Filter out treatment errors
  filter(!envelope_ID %in% c("AYN9607", "AST3380", "BBM8747", "BLM2549", "CMX4054",
                             "CMH5663", "DAI1197", "BHR0925", "AUZ1311", "BOW7206",
                             "DBV0943", "CWZ4784", "EDV5508", "EDR6459", "AEG7270",
                             "EEN3300")) |>
  # Filter out missing bulk leaves
  filter(!envelope_ID %in% list.nobulk) |>
  # Filter out mis-aged VM and CV
  filter(!envelope_ID %in% error.list.vmcv.age) |>
  # Correct the spelling of Senja
  mutate(siteID = replace(siteID, siteID == "Senje", "Senja")) |>
  # Scale for bulk leaves
  mutate(wet_mass_g.avg = case_when(
    species == "Calluna vulgaris" ~ wet_mass_g/bulk_nr_leaves,
    species == "Empetrum nigrum" ~ wet_mass_g/bulk_nr_leaves,
    TRUE ~ wet_mass_g
  )) |>
  relocate(wet_mass_g.avg, .after = wet_mass_g)
