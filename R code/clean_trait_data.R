# Manually clean errors from leaf area scanning datasheet ----
durin.area = read.csv("output/2023.09.27_LeafScanData_Raw.csv") |>
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
  select(-c(cutting, X))

# Manually clean errors from dry mass datasheet ----
durin.drymass = read.csv("raw_data/2023.10.10_DryMassChecks.csv", na.strings=c("","NA")) |>
  # Drop unused columns
  select(-c(X, order.entered)) |>
  # Remove example and non-data
  filter(!envelope_ID %in% c("AAA0000", "Metal tins:")) |>
  # Clean other errors with mutate
  mutate(
    # Replace erroneous 0s with NA
    dry_mass_g = case_when(
    envelope_ID %in% c("CTI1429", "AZL3403") ~ NA,
    TRUE ~ dry_mass_g
  ),
  # Make dry mass a number
  dry_mass_g = as.numeric(dry_mass_g),
  # Add missing (known) values
  dry_mass_g = case_when(
    envelope_ID == "BED6774" ~ 0.01332,
    TRUE ~ dry_mass_g
  )) |>
  # Filter out known bad values/typos
  filter(!(envelope_ID == "EPP7266" & dry_mass_g == 0.00558)) |>
  filter(is.na(flag_DryMass))

# Manually clean errors from main datasheet ----
durin = read.csv("raw_data/2023.10.10_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                 na.strings=c("","NA")) |>
  # Correct Senja
  mutate(siteID = str_replace(siteID, "Senje", "Senja")) |>
  # Set all barcodes to uppercase
  mutate(envelope_ID = str_to_upper(envelope_ID)) |>
  # Change plant height to numeric
  mutate(plant_height = as.numeric(plant_height)) |>
  # Remove erroneous dry_mass_g column
  select(-dry_mass_g) |>
  mutate(
    # Correct envelope_ID typos
    envelope_ID = case_when(
      envelope_ID=="JAN2188"~"JAN3188",
      envelope_ID=="HAO2716"~"JAO2716",
      envelope_ID=="JAW7909"~"JAQ7909",
      envelope_ID=="JAU0506"~"JAU0406",
      envelope_ID=="JAY5700"~"JAY4700",
      envelope_ID=="JBT3257"~"JBT3247",
      envelope_ID=="JEK8175"~"JEK6175",
      envelope_ID=="JER2751"~"JER4751",
      envelope_ID=="JEO2375"~"JEU2375",
      envelope_ID=="JHA7525"~"JHX7525",
      envelope_ID=="JHZ731"~"JHZ4732",
      envelope_ID=="JIG7181"~"JIG7191",
      envelope_ID=="JTP1340"~"JIP1340",
      envelope_ID=="FIU6785"~"JIU6785",
      envelope_ID=="JIBV7267"~"JIV7267",
      envelope_ID=="JTX1029"~"JIZ1029",
      envelope_ID=="JSB3462"~"JSE3462",
      envelope_ID=="JSZ0260"~"JSZ0360",
      envelope_ID=="JTM2490"~"JTM3490",
      envelope_ID=="JOO6233"~"JUU6233",
      envelope_ID=="JOY1879"~"JUY1879",
      envelope_ID=="JVK2226"~"JVK2228",
      envelope_ID=="JUW4653"~"JVW4653",
      envelope_ID=="JUX1381"~"JVX1381",
      envelope_ID=="HWY3758"~"JWY3758",
      envelope_ID=="JXI9810"~"JXI9801",
      envelope_ID=="HJYL5525"~"JYL5525",
      envelope_ID=="AZG5994" ~ "AYG5994",
      envelope_ID=="BJY3407" ~ "BJZ3407",
      envelope_ID=="BMZ3737" ~ "BMY3737",
      envelope_ID=="ECT1801" ~ "ECT1701",
      envelope_ID=="EPO1348" ~ "EPO1378",
      envelope_ID=="JSC1626" ~ "JSC1636",
      envelope_ID=="BIY5546" ~ "BIZ5546",
      TRUE ~ envelope_ID),
    # Correct plot names
    DURIN_plot = case_when(
      envelope_ID =="DAP6500"~ "KA_F_VV_2",
      envelope_ID =="EXQ8322"~ "KA_O_EN_1",
      envelope_ID =="EYR2971"~ "KA_O_EN_2",
      envelope_ID =="EYV3590"~ "KA_O_EN_2",
      envelope_ID =="EZA6532"~ "KA_O_EN_3",
      envelope_ID =="DFW2204"~ "KA_O_EN_4",
      envelope_ID =="EYG4750"~ "KA_O_EN_4",
      envelope_ID =="EYK4044"~ "KA_O_EN_4",
      envelope_ID =="DHD0172"~ "KA_O_VM_2",
      envelope_ID =="DHH3325"~ "KA_O_VM_2",
      envelope_ID =="DFM4890"~ "KA_O_VM_4",
      envelope_ID =="BMB6959"~ "LY_F_EN_5",
      envelope_ID =="BIL0759"~ "LY_F_VM_2",
      envelope_ID =="AYB7940"~ "LY_O_EN_1",
      envelope_ID =="AYF1252"~ "LY_O_EN_1",
      envelope_ID =="AYN9607"~ "LY_O_EN_1",
      envelope_ID =="AUZ1311"~ "LY_O_EN_3",
      envelope_ID =="AWC3830"~ "LY_O_EN_3",
      envelope_ID =="AVY7377"~ "LY_O_EN_4",
      envelope_ID =="BHR0925"~ "LY_O_EN_5",
      envelope_ID =="AST3380"~ "LY_O_VM_4",
      envelope_ID =="AYP7221"~ "LY_O_VV_2",
      envelope_ID =="AZB3929"~ "LY_O_VV_2",
      envelope_ID =="BFY4922"~ "LY_O_VV_2",
      envelope_ID =="AYS6617"~ "LY_O_VM_3",
      envelope_ID =="AZJ4306"~ "LY_O_VV_5",
      envelope_ID =="EVA9626"~ "SE_F_VV_4",
      envelope_ID =="CWZ4784"~ "SE_O_VM_5",
      envelope_ID =="DBV0943"~ "SE_O_VV_1",
      envelope_ID =="DBV0943"~ "SE_O_VV_1",
      envelope_ID =="CYR2242"~ "SE_O_VV_3",
      envelope_ID =="CZD0880"~ "SE_O_VV_4",
      envelope_ID =="CMH5663"~ "SO_F_VM_3",
      envelope_ID =="CMX4054"~ "SO_F_VM_4",
      # From cross-checking number of plants per plot
      envelope_ID =="CKI5874"~ "SO_F_VV_1",
      envelope_ID =="BZJ1412"~ "KA_F_VV_1",
      envelope_ID =="BZN2057"~ "KA_F_VV_1",
      envelope_ID =="DBF6720"~ "KA_F_VV_1",
      envelope_ID =="BZA7321"~ "KA_O_VV_1",
      envelope_ID =="CCH4321"~ "KA_F_EN_1",
      envelope_ID =="BYB4115"~ "KA_F_VV_5",
      envelope_ID =="AYS6617"~ "LY_O_VM_3",
      envelope_ID =="EUK3873"~ "SE_F_VV_3",
      envelope_ID =="EGE6339"~ "SE_F_VV_3",
      envelope_ID =="CYM4429"~ "SE_F_VV_3",
      envelope_ID =="ETB0915"~ "SE_O_VM_1",
      envelope_ID =="EVI4590"~ "SE_F_VV_4",
      envelope_ID =="DAM1823"~ "SE_F_VV_4",
      envelope_ID =="BWU3342"~ "SE_F_VV_4",
      envelope_ID =="EUJ5068"~ "SE_F_VV_4",
      # Round two corrections
      envelope_ID == "EUJ5068" ~ "SE_O_VV_4",
      envelope_ID == "EUS4572" ~ "SE_F_VV_3",
      # Missing plots to be entered
      envelope_ID == "ARB1083" ~ "LY_O_EN_4",
      envelope_ID == "ATE1699" ~ "LY_O_VM_3",
      envelope_ID == "ATI1569" ~ "LY_O_VM_2",
      envelope_ID == "ATX5549" ~ "LY_O_EN_2",
      envelope_ID == "AYB7940" ~ "LY_O_EN_1",
      envelope_ID == "AYP7221" ~ "LY_O_VV_2",
      envelope_ID == "AYS6617" ~ "LY_O_VM_3",
      envelope_ID == "AZE4205" ~ "LY_O_VM_1",
      envelope_ID == "AZJ4306" ~ "LY_O_VV_5",
      envelope_ID == "EGE6339" ~ "SE_F_VV_2",
      # Species corrections
      envelope_ID %in% c("AVI9865", "ATG1962", "AVY7377") ~ "LY_O_VV_4",
      TRUE ~ DURIN_plot
    ),
    # Correct habitats
    habitat = case_when(
      envelope_ID =="EXQ8322"~"Open",
      envelope_ID =="EYR2971"~"Open",
      envelope_ID =="EYV3590"~"Open",
      envelope_ID =="EZA6532"~"Open",
      envelope_ID =="DFW2204"~"Open",
      envelope_ID =="EYG4750"~"Open",
      envelope_ID =="EYK4044"~"Open",
      envelope_ID =="DHD0172"~"Open",
      envelope_ID =="DHH3325"~"Open",
      envelope_ID =="DFM4890"~"Open",
      envelope_ID =="BIL0759"~"Forested",
      envelope_ID =="AZB3929"~"Open",
      envelope_ID =="EVA9626"~"Forested",
      envelope_ID =="CYR2242"~"Open",
      envelope_ID =="CZD0880"~"Open",
      envelope_ID =="BOW7206"~"Forested",
      # From round 2 corrections
      envelope_ID == "CLA4537" ~ "Open",
      envelope_ID == "CLE7064" ~ "Open",
      # Missing plots to be entered
      envelope_ID == "ARB1083" ~ "Open",
      envelope_ID == "ATE1699" ~ "Open",
      envelope_ID == "ATI1569" ~ "Open",
      envelope_ID == "ATX5549" ~ "Open",
      envelope_ID == "AYB7940" ~ "Open",
      envelope_ID == "AYP7221" ~ "Open",
      envelope_ID == "AYS6617" ~ "Open",
      envelope_ID == "AZE4205" ~ "Open",
      envelope_ID == "AZJ4306" ~ "Open",
      envelope_ID == "EGE6339" ~ "Forested",
      TRUE ~ habitat
    ),
    # Correct plot numbers
    plotNR = case_when(
      envelope_ID == "BFY4922" ~ 2,
      # Missing plots to be entered
      envelope_ID == "ARB1083" ~ 4,
      envelope_ID == "ATE1699" ~ 3,
      envelope_ID == "ATI1569" ~ 2,
      envelope_ID == "ATX5549" ~ 2,
      envelope_ID == "AYB7940" ~ 1,
      envelope_ID == "AYP7221" ~ 2,
      envelope_ID == "AYS6617" ~ 3,
      envelope_ID == "AZE4205" ~ 1,
      envelope_ID == "AZJ4306" ~ 5,
      envelope_ID == "EGE6339" ~ 2,
      TRUE ~ plotNR
    ),
    # Correct leaf numbers
    leaf_nr = case_when(
      envelope_ID %in% c("EEF0649", "AHC6687") ~ 1,
      envelope_ID %in% c("AHD4843") ~ 2,
      envelope_ID %in% c("AHE7194","AGT6887", "BMS6362","DDP2497") ~ 3,
      TRUE ~ leaf_nr
    ),
    # Correct leaf ages
    leaf_age = case_when(
      envelope_ID =="AZG5994"~"old",
      envelope_ID =="EVO1776"~"young",
      envelope_ID =="EVS4445"~"young",
      envelope_ID =="ASS9832"~"old",
      envelope_ID =="BBM8747"~"old",
      DroughNet_plotID == 7.3 & species == "Vaccinium myrtillus" ~ "young",
      # Round 3 corrections
      envelope_ID == "BMC0045" ~ "old",
      envelope_ID == "CST5790" ~ "young",
      TRUE ~ leaf_age
    ),
    DroughNet_plotID = case_when(
      envelope_ID == "EFN3557" ~ 2.3,
      envelope_ID == "EFR2422" ~ 2.3,
      TRUE ~ DroughNet_plotID
    ),
    # Correct thicknesses
    leaf_thickness_1_mm = case_when(
      envelope_ID =="DSD6681"~0.0107,
      envelope_ID =="CSP7326"~0.259,
      # From round 2 corrections
      envelope_ID == "ASM6249" ~ 0.114,
      # Round 3 corrections
      envelope_ID == "AYX2273" ~ 0.248,
      TRUE ~ leaf_thickness_1_mm
    ),
    leaf_thickness_2_mm = case_when(
      envelope_ID =="BPF4529"~0.221,
      # From round 2 corrections
      envelope_ID == "ASM6249" ~ 0.114,
      TRUE ~ leaf_thickness_2_mm
    ),
    # Correct plant heights
    plant_height = case_when(
      envelope_ID =="AWH7022"~11.6,
      envelope_ID =="DUX5951"~14.8,
      envelope_ID =="DNA2455"~15.4,
      envelope_ID =="ERV2714"~15.5,
      envelope_ID =="DFX0733"~17.0,
      envelope_ID =="DGF1762"~17.0,
      envelope_ID =="ANO6821"~17.5,
      envelope_ID =="CHP1113"~41.2,
      envelope_ID =="APH3193"~49.2,
      envelope_ID =="AJA7257"~49.2,
      envelope_ID =="APC6542"~23.3,
      envelope_ID =="AIO8720"~23.3,
      envelope_ID =="AGX3707"~8.1,
      envelope_ID =="ARR7615"~11.6,

      TRUE ~ plant_height
    ),
    # Correct species
    species = case_when(
      envelope_ID %in% c("ALL1763", "AWF5086", "BBM8747",
                         "DAI1197", "DZX9994", "BLM2549",
                         "AVI9865", "ATG1962", "AVY7377") ~ "Vaccinium vitis-idaea",

      TRUE ~ species
    ),
    # Correct wet mass
    wet_mass_g = case_when(
    envelope_ID =="DDZ3156"~0.0138,
    envelope_ID =="DEN0101"~0.0254,
    envelope_ID =="ARK3594"~0.0282,
    envelope_ID =="AFH1727"~0.0333,
    envelope_ID =="BSD3874"~0.0468,
    TRUE ~ wet_mass_g),
    # Correct Tjøtta plot switch
    DroughNet_plotID = case_when(
      siteID == "Tjøtta" & DroughNet_plotID == 1.1 ~ 1.2,
      TRUE ~ DroughNet_plotID
    ),
    # Correct ageClass typos
    ageClass = case_when(
      envelope_ID == "EDV5508" ~ "Pioneer",
      envelope_ID == "EDR6459" ~ "Pioneer",
      TRUE ~ ageClass
    ),
    # Correct swapped plant numbers and plant heights
    plant_nr = as.numeric(plant_nr),
    plant_nr = case_when(
      DURIN_plot == "KA_F_EN_3" & plant_nr == 1 ~ 3,
      DURIN_plot == "KA_F_EN_3" & plant_nr == 3 ~ 1,
      envelope_ID == "CQU5714" ~ 1,
      envelope_ID == "BBM8747" ~ 3,
      envelope_ID == "CWT0272" ~ 1,
      DURIN_plot == "SO_O_VV_5" & plant_height == 13.5 ~ 2,
      DURIN_plot == "SO_O_VV_5" & plant_height == 10.4 ~ 3,
      envelope_ID =="AAO4783"~3,
      envelope_ID =="AAT2449"~3,
      envelope_ID =="AFN5166"~3,
      envelope_ID =="EFN3557"~3,
      DroughNet_plotID == 7.3 & plant_height == 17.6 ~ 2,
      TRUE ~ plant_nr
    ),
    plant_height = case_when(
      DURIN_plot == "KA_F_EN_3" & plant_nr == 1 ~ 17.2,
      DURIN_plot == "KA_F_EN_3" & plant_nr == 3 ~ 19.0,
      DURIN_plot == "LY_F_EN_2" & plant_nr == 3 ~ 26.8,
      DURIN_plot == "LY_F_EN_5" & plant_nr == 1 ~ 28.0,
      DURIN_plot == "LY_O_EN_1" & plant_nr == 1 ~ 10.2,
      DURIN_plot == "LY_O_EN_3" & plant_nr == 3 ~ 14.2,
      # DURIN_plot == "LY_O_EN_4" & plant_nr == 2 ~ 16.4,
      DURIN_plot == "SE_O_VM_3" & plant_nr == 2 ~ 15.5,
      DURIN_plot == "SE_O_VM_3" & plant_nr == 3 ~ 17.2,
      DURIN_plot == "SE_O_VV_3" & plant_nr == 3 ~ 18.4,
      envelope_ID == "ESC1744" ~ 18.5,
      TRUE ~ plant_height
    )
  ) |>
  # Filter out known bad values/typos
  filter(!(envelope_ID == "CVP9320" & wet_mass_g == 0.4470)) |>
  filter(!(envelope_ID == "BKI4712" & species == "Calluna vulgaris")) |>
  filter(is.na(flag)) |>
  # Add dry mass and area
  left_join(durin.drymass) |>
  left_join(durin.area) |>
  # Filter out any complete duplicates
  distinct()


# write.csv(durin, "output/2023.09.11_cleanDURIN.csv")
