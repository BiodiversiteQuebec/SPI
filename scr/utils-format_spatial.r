###############################################################################
# This script formats spatial data for the SPI indicator
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################


#------------------------------------------------------------------------------
# Function to union spatial polygons
#------------------------------------------------------------------------------
union_polygons <- function(x) {
    # Union all polygons in x
    x <- geos::as_geos_geometry(x) |>
        geos::geos_make_collection() |>
        geos::geos_unary_union() |>
        sf::st_as_sf()
    return(x)
}

