###############################################################################
# Plot species range map
#
# Arguments:
#   - range_map: SpatRaster object containing the range map of the species.
#                Multiple layers may be provided such as years
#   - layer: name of the layer to visualise. If NULL then all layer are
#            visualised
#   - base_map: sf object containing the basemap to plot the range map over
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_range_map <- function(range_map, layer = NULL, base_map = NULL) {
    #------------------------------------------------------------------------------
    # 1. Load data
    #------------------------------------------------------------------------------

    # Read the first tif file and plot it
    r <- terra::rast(range_map)


    #------------------------------------------------------------------------------
    # 2. Plot the raster and the Quebec map
    #------------------------------------------------------------------------------

    # Plot basemap
    if(!is.null(base_map)) {
        terra::plot(base_map$geom)
        add <- TRUE
    } else {
        add <- FALSE
    }

    # Plot the raster layer over the basemap with semi-transparent pixels
    if (is.null(layer)) {
        terra::plot(r_proj, add = add)
    } else {
        terra::plot(r_proj[[layer]], add = add)
    }
}