###############################################################################
# This script computes the SPI indicator from range maps produced by Vicent
# Belavance. 
#
# The maps were produced using mapSpecies R package to generate distribution
# maps from occurences data.
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

#------------------------------------------------------------------------------
# 1. Load libraries
#------------------------------------------------------------------------------
library(terra)
library(sf)
library(dplyr)


#------------------------------------------------------------------------------
# 2. Load data
#------------------------------------------------------------------------------

# Read the first tif file and plot it
r <- rast("data/acanthis_flammea.tif")
qc_map <- read_sf("data/QUEBEC_CR_NIV_01.gpkg")
# Reproject the sf object to match the coordinate system of the SpatRaster
r_proj <- project(r, crs(qc_map))

# Plot the raster and the Quebec map
# terra::plot(qc_map$geom)
# terra::plot(r_proj$X2017, add = TRUE)

# Read the data/registre_aires_prot.gpkg file and glimpse it
sf::st_layers("data/registre_aires_prot.gpkg")
## Select the layer Aires protégées du registre
aires_prot <- read_sf("data/registre_aires_prot.gpkg", layer = "AP_REG_S")
dplyr::glimpse(aires_prot)


#------------------------------------------------------------------------------
# 3. Compute overlap between species distribution and protected areas
# 
# The overlap is computed in km2
# The overlap is computed for each protected area and for each year
# The overlap area is computed as the sum of fraction of cells that overlaps
# with the protected area multiplied by the mean area of cells
#------------------------------------------------------------------------------

# Compute the area of overlap between r and aires_prot
mean_cell_area <- mean(values(cellSize(r_proj)) * 1e-06, na.rm = TRUE) # km2
overlap_areas <- list()

for (i in 1:nrow(aires_prot)) {
    # Extract the overlap between r_proj and aires_prot in km2
    # Overlap is mesured in km2
    overlap <- extract(r_proj, aires_prot[i, ], fun = 'sum', exact = TRUE, ID = FALSE) * mean_cell_area
    overlap_areas[[i]] <- overlap
    
    # Calculate progress percentage
    progress_percentage <- (i / nrow(aires_prot)) * 100
    cat("Progress:", round(progress_percentage, 2), "%\r")
}

protected_habitats <- do.call(rbind, overlap_areas)

# Create new ./results directory
dir.create("results", showWarnings = FALSE)
saveRDS(protected_habitats, "results/overlap_areas.rds")


#------------------------------------------------------------------------------
# 4. Compute SPI
#
# SPI is computed as the proportion of protected area in the species range
#------------------------------------------------------------------------------

protected_habitat <- colSums(protected_habitats, na.rm = TRUE)

# Compute the area in km2 of the species range for each year
# It is computed as the number of cells with a value > 0 in the species range
# map multiplied by the area of each cell
range_areas <- c()
for(i in 1:nlyr(r_proj)) {
    cell_size <- cellSize(r_proj[[i]])
    range_area <- sum(cell_size[r_proj[[i]] > 0] * 1e-06, na.rm = TRUE)
    range_areas[i] <- range_area
}

(SPI <- protected_habitat / range_areas)

# Save SPI to results directory
saveRDS(SPI, "results/SPI.rds")
write.csv(SPI, "results/SPI.csv")


###############################################################################
# TODO
# - [ ] Fuse overlapping polygons within aires_prot