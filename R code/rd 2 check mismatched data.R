library(tidyverse)

# Code to locate a specific specimen ----
check = function(barcode) {
  data = durin |>
    select(envelope_ID, siteID,
           DURIN_plot, ageClass, DroughtTrt, DroughNet_plotID,
           plotNR, habitat, leaf_age,
           species, plant_nr, leaf_nr, plant_height, wet_mass_g,
           leaf_thickness_1_mm, leaf_thickness_2_mm, leaf_thickness_3_mm) |>
    filter(envelope_ID == barcode)
  data
}

# DURIN ----
# Checking that DURIN species are in the correct plots
spp.durin = durin |>
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

# Check that DURIN habitats have been correctly assigned
habitat.durin = durin |>
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

write.csv(habitat.durin, "output/2023.07.31_checks_habitat.durin.csv")

# DroughtNet ----
DN.metadata = read.csv("output/DroughtNet plot metadata.csv") |>
  select(-c(X)) |>
  rename(DroughtTrt.correct = DroughtTrt)

age.DN = durin |>
  #Filter to just DroughtNet
  drop_na(DroughNet_plotID) |>
  # Manually create verified age class
  # NOTE this is incorrect for a handful of recollected plants:
  # PIO 3.2 CV, PIO 3.1 CV (x2)
  mutate(ageClass.correct = case_when(
    day == 5 ~ "Pioneer",
    day == 3 ~ "Mature"
  )) |>
  # Add verified metadata
  left_join(DN.metadata) |>
  filter(ageClass != ageClass.correct) |>
  relocate(c(day, month, DroughNet_plotID, species,
             ageClass, ageClass.correct, DroughtTrt, DroughtTrt.correct), .after = envelope_ID)

trt.DN = durin |>
  #Filter to just DroughtNet
  drop_na(DroughNet_plotID) |>
  # Manually create verified age class
  # NOTE this is incorrect for a handful of recollected plants:
  # PIO 3.2 CV, PIO 3.1 CV (x2)
  mutate(ageClass.correct = case_when(
    day == 5 ~ "Pioneer",
    day == 3 ~ "Mature"
  )) |>
  # Add verified metadata
  left_join(DN.metadata) |>
  filter(DroughtTrt != DroughtTrt.correct) |>
  relocate(c(day, month, DroughNet_plotID, species,
             ageClass, ageClass.correct, DroughtTrt, DroughtTrt.correct), .after = envelope_ID)

write.csv(trt.DN, "output/2023.07.24_checks_trt.DroughtNet.csv")

# Check number of plants per plot ----
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

# Function to list all of a given plot + species
check.plant = function(plot) {
  data = durin |>
    select(envelope_ID, siteID,
           DURIN_plot,
           plotNR, habitat,
           species, plant_nr, leaf_nr, leaf_age, plant_height) |>
    filter(DURIN_plot == plot) |>
    arrange(plant_nr, leaf_age, leaf_nr)
  data
}

error.DN.plots = durin |>
  # Remove Phys Team plants
  filter(leaf_nr > 0) |>
  # Bring to just the level of unique plants
  select(siteID, ageClass, DroughNet_plotID, species, plant_nr, plant_height) |>
  drop_na(DroughNet_plotID) |>
  distinct() |>
  # Summarize
  group_by(siteID, ageClass, DroughNet_plotID, species) |>
  summarize(n = length(plant_height)) |>
  ungroup() |>
  # Add in max plant nr data
  left_join(durin.max.plant_nr) |>
  # Filter to the errors
  filter(n > max.plant.n)

# Function to list all of a given plot + species
check.plant = function(site, age, plot, spp) {
  data = durin |>
    select(envelope_ID, siteID,
           ageClass, DroughNet_plotID,
           species, plant_nr, leaf_nr, leaf_age, plant_height) |>
    filter(siteID == site & ageClass == age & DroughNet_plotID == plot & species == spp) |>
    arrange(plant_nr, leaf_age, leaf_nr) |>
    drop_na(plant_height)
  data
}

# Check number of leaves per age group ----
## Again, this isn't working due to the case_when
# Does case_when not do maths?
error.durin.plots = durin |>
  # Bring to just the level of unique plants
  select(siteID, DURIN_plot, species, plant_nr, plant_height, leaf_nr, leaf_age) |>
  drop_na(DURIN_plot) |>
  distinct() |>
  # Summarize
  group_by(siteID, DURIN_plot, species, plant_nr, leaf_age) |>
  summarize(n = length(leaf_nr)) |>
  ungroup() |>
  # Add in max plant nr data
  left_join(durin.max.plant_nr) |>
  mutate(max.leaves =
           case_when(
    species == "Vaccinium vitis-idaea" | species == "Empetrum nigrum" ~ (max.plant.n * 6),
    species == "Vaccinium myrtillus" | species == "Calluna vulgaris" ~ (max.plant.n * 3),
    TRUE ~ "Unknown")
  )
  # Filter to the errors
  filter(n > max.leaves)

# Function to list all of a given plot + species
check.species = function(plot) {
  data = durin |>
    select(envelope_ID, siteID,
           DURIN_plot,
           plotNR, habitat,
           species, plant_nr, leaf_nr, leaf_age, plant_height) |>
    filter(DURIN_plot == plot) |>
    arrange(plant_nr, leaf_age, leaf_nr)
  data
}

# Figure out which plant heights are assigned NA ----
error.height.na = durin |>
  filter(is.na(plant_height)) |>
  filter(project == "Field - Traits") |>
  select(envelope_ID) |>
  left_join(read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                     na.strings=c("","NA"))) |>
  relocate(plant_height, .after = envelope_ID)

# Check number of leaves per plant ----
# Function to list all of a given plant (DURIN)
check.plant = function(plot, plantnr) {
  data = durin |>
    select(envelope_ID, siteID,
           DURIN_plot,
           plotNR, habitat,
           species, plant_nr, leaf_nr, leaf_age, plant_height) |>
    filter(DURIN_plot == plot & plant_nr == plantnr) |>
    arrange(plant_nr, leaf_age, leaf_nr)
  data
}


# Function to list all of a given plant (DroughtNet)
check.plant = function(site, plot, speciesID, plantnr) {
  data = durin |>
    select(envelope_ID, siteID,
           DroughNet_plotID, ageClass,
           species, plant_nr, leaf_nr, leaf_age, plant_height) |>
    filter(siteID == site & DroughNet_plotID == plot & species == speciesID & plant_nr == plantnr) |>
    arrange(ageClass, plant_nr, leaf_age, leaf_nr)
  data
}
## DURIN only ----
error.durin.leaves = durin %>%
  # Select just the relevant data
  drop_na(DURIN_plot) |>
  filter(project == "Field - Traits") |>
  dplyr::group_by(siteID, DURIN_plot, plant_nr, leaf_age, leaf_nr) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

## DroughtNet only ----
error.DN.leaves = durin %>%
  # Select just the relevant data
  drop_na(DroughNet_plotID) |>
  filter(project == "Field - Traits") |>
  dplyr::group_by(project, siteID, ageClass, DroughNet_plotID, species,
                  plant_nr, leaf_age, leaf_nr) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)


# Run check.plant() for each one listed in the resulting object to find barcodes with errors

### Manually inspect all with lists
