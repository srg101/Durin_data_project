# Visualize the basic variables against each other

durin.viz = durin.noerrors |>
  # DURIN plots only
  drop_na(DURIN_plot) |>
  select(-c(bulk_nr_leaves, wet_mass_g)) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") %>%
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Factor the sites
  mutate(siteID = factor(siteID, levels = c("Lygra", "Sogndal", "Senja", "Kautokeino")))

# visualize data ----
library(ggh4x)

# Traits not height
ggplot(durin.viz %>% filter(leaf_age != "" & habitat != "") |> filter(trait != "plant_height"),
       aes(interaction(habitat, siteID), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN leaf thickness and leaf mass") +
  theme_bw()

ggsave("visualizations/2023.07.25_DURIN_thickness.mass.png",
       width = 12, height = 8, units = "in")

# Plant height
ggplot(durin.viz %>% filter(leaf_age != "" & habitat != "") |> filter(trait == "plant_height"),
       aes(interaction(habitat, siteID), y = value)) +
  geom_boxplot() +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN plant height") +
  theme_bw()

ggsave("visualizations/2023.07.25_DURIN_height.png",
       width = 6, height = 8, units = "in")

# DroughtNet----
DN.viz = durin.noerrors |>
  # DURIN plots only
  drop_na(DroughNet_plotID) |>
  # Select DroughtNet only columns
  select(envelope_ID:project, ageClass:leaf_thickness_3_mm) %>%
  select(-bulk_nr_leaves) |>
  # Tidy in long form
  pivot_longer(cols = plant_height:leaf_thickness_3_mm, names_to = "trait", values_to = "value") %>%
  # Standardize traits
  mutate(trait = replace(trait,
                         trait == "leaf_thickness_1_mm" | trait == "leaf_thickness_2_mm" | trait == "leaf_thickness_3_mm",
                         "leaf_thickness")) |>
  # Factor the sites
  mutate(siteID = factor(siteID, levels = c("Lygra", "Sogndal","Tjøtta", "Senja", "Kautokeino")))

# visualize data ----
library(ggh4x)

## Leaf thickness ----
ggplot(DN.viz %>% filter(leaf_age != "" & ageClass != "" & DroughtTrt != "") |> filter(trait == "leaf_thickness"),
       aes(interaction(DroughtTrt, ageClass), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ siteID) +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DroughtNet leaf thickness") +
  theme_bw()

ggsave("visualizations/2023.07.25_DroughtNet_thickness.png",
       width = 10, height = 8, units = "in")

## Weight ----
ggplot(DN.viz %>% filter(leaf_age != "" & ageClass != "" & DroughtTrt != "") |> filter(trait == "wet_mass_g"),
       aes(interaction(DroughtTrt, ageClass), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ siteID) +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DroughtNet leaf mass") +
  theme_bw()

ggsave("visualizations/2023.07.25_DroughtNet_mass.png",
       width = 10, height = 8, units = "in")

## Height ----
ggplot(DN.viz %>% filter(leaf_age != "" & ageClass != "" & DroughtTrt != "") |> filter(trait == "plant_height"),
       aes(interaction(DroughtTrt, ageClass), y = value)) +
  geom_boxplot() +
  facet_wrap(~ siteID) +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DroughtNet plant height") +
  theme_bw()

ggsave("visualizations/2023.07.25_DroughtNet_height.png",
       width = 6, height = 8, units = "in")

# By species ----
ggplot(durin.viz %>% filter(leaf_age != "" & habitat != "") |>
         mutate(siteID = factor(siteID,
                                levels = c("Lygra", "Sogndal", "Senja", "Kautokeino"),
                                labels = c("LY", "SO", "SE", "KA"))) |>
         filter(trait != "plant_height"),
       aes(interaction(siteID, species), y = value, fill = leaf_age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey40", "grey80")) +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  # labs(x = "", title = "DURIN leaf thickness and leaf mass") +
  theme_bw()

ggsave("visualizations/2023.07.28_DURIN_species_thickness.mass.png",
       width = 12, height = 8, units = "in")

# Plant height
ggplot(durin.viz %>% filter(leaf_age != "" & habitat != "") |>
         mutate(siteID = factor(siteID,
                                levels = c("Lygra", "Sogndal", "Senja", "Kautokeino"),
                                labels = c("LY", "SO", "SE", "KA")))
       |> filter(trait == "plant_height"),
       aes(interaction(siteID, species), y = value)) +
  geom_boxplot() +
  facet_wrap(~ trait, scales = "free") +
  scale_y_log10() +
  scale_x_discrete(guide = "axis_nested") +
  labs(x = "", title = "DURIN plant height") +
  theme_bw()

ggsave("visualizations/2023.07.25_DURIN_species_height.png",
       width = 6, height = 8, units = "in")
