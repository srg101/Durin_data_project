complete.droughtnet = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv") %>%
  #Filter to just DroughtNet
  drop_na(DroughNet_plotID) |>
  #Make list of only individual plants, not leaves
  select(siteID, DroughNet_plotID, species, plant_nr) |>
  distinct() |>
  # Filter out ones we don't need to worry about
  filter(species != "Calluna vulgaris") |>
  filter(DroughNet_plotID != 0.0) |>
  # Summarize
  group_by(siteID, DroughNet_plotID, species) |>
  summarise(n = length(plant_nr)) |>
  # List plots with too few plants
  filter(n < 3)

complete.durin = read.csv("raw_data/2023.07.20_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv") %>%
  #Filter to just DroughtNet
  drop_na(DURIN_plot) |>
  #Make list of only individual plants, not leaves
  select(siteID, DURIN_plot, species, plant_nr) |>
  distinct() |>
  # Filter out ones we don't need to worry about
  filter(species != "Calluna vulgaris") |>
  filter(DURIN_plot != 0.0) |>
  # Summarize
  group_by(siteID, DURIN_plot, species) |>
  summarise(n = length(plant_nr)) |>
  # List plots with too few plants
  filter(n < 3)
