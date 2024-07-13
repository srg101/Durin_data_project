# install.packages("remotes")
remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")

# Create barcode envelopes for DURIN
library(PFTCFunctions)
## create list with all envelope codes. And show the first five values
all_codes <- get_PFTC_envelope_codes(seed = 88, prefix = "")
all_codes$hashcode[1:5]


# Unique envelope codes
unique_PFTC_envelope_codes <- function(newseed, oldseeds) {
  oldhash <- purrr::map_df(oldseeds, get_PFTC_envelope_codes)
  uniqueIDs <- get_PFTC_envelope_codes(newseed) %>%
    anti_join(oldhash, by = "hashcode")

  return(uniqueIDs)
}

check_IDs <- unique_PFTC_envelope_codes(newseed = 88, oldseeds = 49)
library(tidyverse)
install.packages("baRcodeR")
library(baRcodeR)
make_barcode_labels(all_codes, filename = "DURIN_Thermal_Tolerance_2024.pdf")
