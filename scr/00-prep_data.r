###############################################################################
# Utils scripts to prep data for SPI computation
#
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################
library(sf)

###############################################################################
# Range maps
#
# Files have different column order and different column names. This script
# combines all range maps into one file.
###############################################################################
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


###############################################################################
# Range maps for the south and north regions
#
# SPLIT = 50 is the latitude where the range maps are split into two regions
# (S and N).
###############################################################################
# Params
## Qc bounding box
xmin = -80.1
ymin = 44
xmax = -57
ymax = 63
## Split latitude
SPLIT = 50

# Load data
range_maps <- sf::st_read("./data_clean/aires_repartition.gpkg", quiet = TRUE)
aires_prot <- suppressWarnings(st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE))


# Get bounding box
box_s <- st_bbox(
  st_transform(
    st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymax = SPLIT, ymin = ymin), crs = st_crs(4326))), 
    st_crs(range_maps)
  )
)
box_n <- st_bbox(
  st_transform(
    st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = SPLIT), crs = st_crs(4326))), 
    st_crs(range_maps)
  )
)

# Split range maps into two regions
range_maps_s <- range_maps |>
        st_make_valid() |>
        st_crop(box_s) |>
        suppressWarnings()
# mapview::mapview(range_maps_s[1:10,])
# plot(range_maps_s[1:10,"geom"])
range_maps_n <- range_maps |>
        st_make_valid() |>
        st_crop(box_n) |>
        suppressWarnings()
# mapview::mapview(range_maps_n[1:10, "geom"])
# plot(range_maps_n[1:10, "geom"])
st_write(range_maps_s, "./data_clean/aires_repartition_sud.gpkg", append = FALSE, quiet = TRUE)
st_write(range_maps_n, "./data_clean/aires_repartition_nord.gpkg", append = FALSE, quiet = TRUE)


###############################################################################
# Protected areas
#
# Union of protected areas
###############################################################################
library(sf)
library(dplyr)
library(geos)

print("Union in progress...")

    # If the union of protected areas does not exist then create it
    aires_prot <- read_sf("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S")
    aires_prot$annee_creation <- as.numeric(substr(aires_prot$DA_CREATIO, start = 1, stop = 4))
    years <- unique(unique(aires_prot$annee_creation)) |> sort()
    #years <- years[years >= YEAR_RANGE[1] & years <= YEAR_RANGE[2]] # select years
    for (i in years) {
        aires_prot_year <- aires_prot[aires_prot$annee_creation <= i, ]
        aires_prot_union <- aires_prot_year |>
            st_union() |> # st_cast("POLYGON")
            st_as_sf()

        if(i == years[1]) {
            x <- aires_prot_union
        } else {
            x <- rbind(x, aires_prot_union)
        }
    }
    x$year <- years
    st_write(x, paste0("data_clean/aires_union.gpkg"), driver = "GPKG")


##############################################################################
# Protected areas
#
# Split protected areas into two regions
# SPLIT = 50 is the latitude where the range maps are split into two regions
# (S and N).
##############################################################################
# Prep protected areas
aires_s <- aires_prot |>
        st_crop(box_s) |>
        suppressWarnings()
# mapview::mapview(aires_s |> st_simplify())
aires_n <- aires_prot |>
        st_crop(box_n) |>
        suppressWarnings()
# mapview::mapview(aires_n)
st_write(aires_s, "data_clean/aires_protegees_sud.gpkg", append = FALSE, quiet = TRUE)
st_write(aires_n, "data_clean/aires_protegees_nord.gpkg", append = FALSE, quiet = TRUE)
