###############################################################################
# Utils script to extracts range maps
#
# Files have different column order and different column names. This script
# combines all range maps into one file.
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################
library(sf)

# 1. Amphibiens
amph <- sf::st_read("./data_raw/Aires_repartition_amphibiens.gpkg")
names <- names(amph)

# 2. Mammifères terrestres
mam <- sf::st_read("./data_raw/Aires_repartition_MT.gpkg")
names(mam)[names(mam) == "GRAND_GROU"] <- "GRAND_GROUPE"

# 3. Poisson eau douce
poiss <- sf::st_read("./data_raw/Aires_repartition_poisson_eau_douce.gpkg")
names(poiss)[names(poiss) == "MODIF_DATE"] <- "DATE_MAJ"
poiss <- poiss[, names]

# 4. Reptiles
rept <- sf::st_read("./data_raw/Aires_repartition_reptiles.gpkg")
names(rept)[names(rept) == "GRAND_GROU"] <- "GRAND_GROUPE"

# Combine all range maps into one file
range_maps <- rbind(amph, mam, poiss, rept)

# Save range maps
sf::st_write(range_maps, "./data_clean/aires_repartition.gpkg", driver = "GPKG", append = FALSE)