# title: "DURIN Leaf Area Calculations 2024"
# author: "Sonya R. Geange"
# output: html_notebook


### CALCULATE LEAF AREA ###

#### LOAD LIBRARY
devtools::install_github("richardjtelford/LeafArea")
library(LeafArea)
library(plyr)
library(dplyr)


#### Function to calculate leaf area
loop.files <-  function(files){

  file.copy(files, new.folder)
  if(grepl("-NA$", files)){
    newfile <- basename(files)
    file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
                                                         "/", gsub("-NA$", "", newfile)))
  }
  print(files)
  # Change the file path to your local directory for ImageJ
  area <- try(run.ij(path.imagej = "C:/Users/sge010/OneDrive - University of Bergen/Documents/ImageJ",
                     set.directory = new.folder,
                     distance.pixel = 237, known.distance = 2,
                     log = TRUE,
                     # Pick a minimum size threshold (removes dust spects etc.)
                     low.size = 0.005,
                     # Trim areas on the sides which represent rulers etc.
                     # Trim pixel is all-round trimming of image
                     # Trim pixel right is for the rulers normally
                     # Trim pixel top removes the envelope and blue tap on scanner
                     trim.pixel = 60, trim.pixel.right = 150, trim.pixel.top = 1500,
                     save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}


# If required
# test run.ij on a subfolder (change directories if required)
run.ij(path.imagej = "C:/Users/sge010/OneDrive - University of Bergen/Documents/ImageJ",
       set.directory = "C:/Users/sge010/OneDrive - University of Bergen/Documents/DURIN/Durin_data_project/data/raw_data/Leaf_Scans",
       distance.pixel = 237, known.distance = 2,
       log = TRUE,
       # Pick a minimum size threshold (removes dust spects etc.)
       low.size = 0.005,
       # Trim areas on the sides which represent rulers etc.
       # Trim pixel is all-round trimming of image
       # Trim pixel right is for the rulers normally
       # Trim pixel top removes the envelope and blue tap on scanner
       trim.pixel = 60, trim.pixel.right = 150, trim.pixel.top = 1500,
       save.image = TRUE)



###########################################################################
#### Calculate leaf area for 2024 DURIN and DroughtNet Collected data
# Make a list of files, temporary folder and output folder
# Be sure to adjust the file path directory if required
# These need to be downloaded from the OSF account.
# Note, each site has a folder and several subfolders.
# TOP TIP: Because of how often errors occur, run each folder seperately, i.e. by dates, or sites.
# Some images also required editing, i.e. painting overlapping leaves etc.
# A default address could be 'raw_data/leaf_scans/DURIN_leaf_scans/

# Change directories
# List of the image files we want to bring in
list.of.files <- dir(path = paste0("C:/Users/sge010/OneDrive - University of Bergen/Documents/DURIN/Durin_data_project/data/raw_data/Leaf_Scans"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
# A new folder (temporary as the script runs, files aren't left here at the end of the process)
# And an output folder for the final thresholded images.
# If these don't run via script, then just create them manually..
new.folder <- "C:/Users/sge010/OneDrive - University of Bergen/Documents/DURIN/Durin_data_project/data/raw_data/Leaf_Scans/Temp" # "raw_data/Temp/"
output.folder <- "C:/Users/sge010/OneDrive - University of Bergen/Documents/DURIN/Durin_data_project/data/raw_data/Leaf_Scans/output/" # raw_data/output

# Run function
LeafArea.raw_total <- plyr::ldply(list.of.files, loop.files)

# Calculate Area Sums
leaf_area <- LeafArea.raw_total |>
  mutate(ID = substr(ID, 1, 7)) |>
  group_by(ID) |>
  dplyr::summarise(n = n(),
                   leaf_area = sum(LeafArea))

# Save data as csv, change directories
dim(LeafArea.raw)
write.csv(leaf_area, file = "C:/Users/sge010/OneDrive - University of Bergen/Documents/DURIN/Durin_data_project/data/raw_data/Leaf_Scans/output/DURIN_Calculated_Leaf_Area_2024_07_19.csv")
