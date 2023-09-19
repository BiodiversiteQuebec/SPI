###############################################################################
# Utils script to extracts range maps produced by Vicent Belavance from their folders
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

# Extract the map_range.tif files from each folder within the terra_converted_maps folder and rename them to the folder name
folders <- list.dirs("terra_converted_maps", recursive = FALSE, full.names=F)
for (i in 1:length(folders)){
    # move the file named maps_range.tif from its folder to the data folder
    file.rename(paste0("./terra_converted_maps/", folders[i],"/maps_range.tif"), paste0("./data/",folders[i],".tif"))
    # remove the folder
    unlink(paste0("./terra_converted_maps/", folders[i]), recursive = TRUE)
}

