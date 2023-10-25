remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")

# make bardoces
library(PFTCFunctions)
# create unique IDs
IDs2023 <- PFTCFunctions::get_PFTC_envelope_codes(seed = 2023)

# Export to work magic in excel
## Namely, assign columns (1-4), rows (1-12), and pages(n+1) in Excel
# write.csv(IDs2023, "raw_data/DURIN_barcodes_all.csv")

barcodes = as_tibble(read.csv("raw_data/DURIN_barcodes_all.csv")) |>
  # Assign lag and leads
  # From https://community.rstudio.com/t/using-preceding-following-rows-to-code-a-new-column/150699/2
  group_by(page, column) |>
  mutate(barcode_above = lag(envelope_ID),
         barcode_below = lead(envelope_ID)) |>
  relocate(envelope_ID, barcode_above, barcode_below)

write.csv(barcodes, "output/barcodes above and below.csv")

#### ARCHIVED ####
# Includes workarounds from before I got the full list of barcodes from Aud

barcodes = read.csv("raw_data/Durin_RangeX_barcodes_2023_transposed.csv") |>
  # Trim white space
  mutate(envelope_ID = str_trim(envelope_ID)) |>
  # Some barcodes got mashed together
  mutate(barcode1 = str_sub(envelope_ID, 1,7),
         barcode2 = str_sub(envelope_ID, 8,14)) |>
  mutate_all(na_if,"") |>
  # Bind the rows
  select(-envelope_ID) |>
  pivot_longer(barcode1:barcode2, values_to = "envelope_ID") |>
  select(envelope_ID) |>
  drop_na() |>
  # Make sure it's alphabetical
  arrange(envelope_ID)

# Get permutations
# from https://github.com/richardjtelford/PFTCFunctions/blob/master/R/get_PFTC_envelope_code.R
get_PFTC_envelope_codes_letters_only <- function(seed){
  suppressWarnings(set.seed(seed = seed, sample.kind = "Rounding"))
  all_codes <- crossing(A = LETTERS, B = LETTERS, C = LETTERS) %>%
    mutate(code = paste0(A, B, C)) %>%
    select(code)
  return(all_codes)
}

codes = get_PFTC_envelope_codes_letters_only(seed = 1)

write.csv(codes, "raw_data/permutations.csv")

# Make sure all barcodes are present
barcode.check = barcodes |>
  # Select just the letters
  mutate(code = str_sub(envelope_ID, 1,3),
         present = "x") |>
  # Join complete permutations
  full_join(codes) |>
  # Filter to missing ones
  filter(is.na(present))


# Export to work magic in excel
## Namely, assign columns (1-4), rows (1-12), and pages(n+1) in Excel
# write.csv(barcodes, "raw_data/DURIN_barcodes_formatted.csv")

envelopeIDs = read.csv("raw_data/Durin_barcodes_formatted.csv") |>
  select(envelope_ID) |>
  mutate(code = str_sub(envelope_ID, 1,3))

# barcodes = as_tibble(read.csv("raw_data/Durin_barcodes_formatted.csv")) |>
  barcodes = as_tibble(read.csv("raw_data/permutations.csv")) |>
    # rename(envelope_ID = code) |>
  # Join list of known envelope IDs
  full_join(envelopeIDs) |>
  # Fill in missing envelope IDs with permutations
  mutate(envelope_ID = coalesce(envelope_ID, code)) |>
  # Assign lag and leads
  # From https://community.rstudio.com/t/using-preceding-following-rows-to-code-a-new-column/150699/2
  group_by(page, column) |>
  mutate(barcode_above = lag(envelope_ID),
         barcode_below = lead(envelope_ID)) |>
  relocate(envelope_ID, barcode_above, barcode_below)

write.csv(barcodes, "output/barcodes above and below.csv")
