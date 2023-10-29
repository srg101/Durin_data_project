droughtnet.plots = read.csv("raw_data/DroughtNet plot metadata_Lygra.csv") |>
  distinct() |>
  drop_na() |>
  mutate(ageClass = case_when(
    ageClass == "PIO" ~ "Pioneer",
    ageClass == "MAT" ~ "Mature",
    ageClass == "BUI" ~ "Building",
    TRUE ~ "Unknown"
  ),
  DroughtTrt = case_when(
    DroughtTrt == "Amb (0%)" ~ "Amb (0)",
    DroughtTrt == "Extr (90%)" ~ "Ext (90)",
    DroughtTrt == "Mod (60%)" ~ "Mod (60)",
    DroughtTrt == "Mod" ~ "Mod (60)",
    TRUE ~ "Unknown"
  )) |>
  bind_rows(read.csv("raw_data/DroughtNet plot metadata_Tjøtta.csv"))

write.csv(droughtnet.plots, "output/DroughtNet plot metadata.csv")
