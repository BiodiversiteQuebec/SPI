# Get array number from command line
rm(list = ls())
array_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))


#------------------------------------------------------------------------------
# 1. Load libraries
#------------------------------------------------------------------------------
library(terra)
library(sf)
library(dplyr)
library(geos)
#source("scr/plot_range_map.r")
source("scr/utils-comp_SPI.r")
source("scr/utils-format_spatial.r")

#------------------------------------------------------------------------------
# 2. Load data
#------------------------------------------------------------------------------

# Get the list of files in the data/range_maps folder (species range maps)
# These maps were extracted from original results of Vincent Bellavance using the 'utils-extract_data.r' script
range_maps <- list.files("data/range_maps", pattern = ".tif$", full.names = TRUE)

# Get the protected areas polygons
if (!file.exists("data/aires_union.gpkg")) {
    # If the union of protected areas does not exist then create it
    aires_prot <- read_sf("data/registre_aires_prot.gpkg", layer = "AP_REG_S") |>
        union_polygons() |>
        st_cast("POLYGON")
    st_write(aires_prot, "data/aires_union.gpkg", driver = "GPKG")
} else {
    aires_prot <- st_read("data/aires_union.gpkg", promote_to_multi = FALSE)
}


#------------------------------------------------------------------------------
# 2. Compute SPI
#
# This code loops through species to compute SPI for each year
#------------------------------------------------------------------------------

species = range_maps[array_id]

sp_name <- sub(".*/(.*)(\\.tif)", "\\1", species)
which_sp <- which(species == range_maps)
cat("Computing SPI for", sp_name, " (", which_sp, "/", length(range_maps), ")\n")

r <- rast(species)

# Reproject the sf object to match the coordinate system of the SpatRaster
r_proj <- project(r, crs(aires_prot))

# Compute SPI as % of range within protected areas
SPI <- SPI_from_range_map(r_proj, aires_prot)


#------------------------------------------------------------------------------
# 2. Save SPI
#------------------------------------------------------------------------------

# Save SPI to results directory
write.csv(SPI, paste0("results/", sp_name, "_SPI.csv"))
