# make bardoces

library(PFTCFunctions)

# create unique IDs
IDs2023 <- PFTCFunctions::get_PFTC_envelope_codes(seed = 2023)



library(baRcodeR)
# 45.7 x 21.2 mm labels
custom_create_PDF(Labels = IDs2023$hashcode,
                  name = "Durin_RangeX_barcodes_2023",
                  type = "linear",
                  Fsz = 14, # font size
                  Across = TRUE, # prints labels across rows
                  trunc = TRUE, # text broken into multiple lines
                  numrow = 12,
                  numcol = 4,
                  # page width and height in inches
                  page_width = 8.3,
                  page_height = 11.7,
                  # margin width and height in inches (top. 2.15 and left/right: 1 cm)
                  width_margin = 0.3937008,
                  height_margin = 0.8464567,
                  # label width and height in inches
                  label_width = 1.799213,
                  label_height = 0.8346457)
