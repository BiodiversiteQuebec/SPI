###############################################################################
# This script computes the SPI indicator from range maps and multipolygons of
# protected areas.
#
# Arguments:
#   - range_map: SpatRaster object containing the range map of the species.
#                Multiple layers may be provided such as years
#   - protected_area: sf object containing the multipolygons of protected areas
#
# Output: Vector with SPI indicator value as a % of range within protected areas. 
#         If range_map contains multiple layers then SPI is computed for each
#         layer and returned as a vector.
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

SPI_from_range_map <- function(range_map, protected_area) {
    #------------------------------------------------------------------------------
    # 1. Compute overlap between species distribution and protected areas
    # 
    # The overlap is computed in m2
    # The overlap is computed for each protected area and for each year
    # The overlap area is computed as the sum of fraction of cells that overlaps
    # with the protected area multiplied by the mean area of cells
    #------------------------------------------------------------------------------

    # Compute the area of overlap between range_map and protected_area
    mean_cell_area <- mean(values(cellSize(range_map)), na.rm = TRUE) # m2
    overlap_areas <- list()

    for (i in 1:nrow(protected_area)) {
        # Extract the overlap between range_map and protected_area in km2
        # Overlap is mesured in km2
        overlap <- extract(range_map, protected_area[i, ], fun = 'sum', exact = TRUE, ID = FALSE) * mean_cell_area
        overlap_areas[[i]] <- overlap
        
        # Calculate progress percentage
        progress_percentage <- (i / nrow(protected_area)) * 100
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
        range_area <- sum(cell_size[r_proj[[i]] > 0], na.rm = TRUE)
        range_areas[i] <- range_area
    }

    SPI <- protected_habitat / range_areas

    return(SPI)
    # # Save SPI to results directory
    # saveRDS(SPI, "results/SPI.rds")
    # write.csv(SPI, "results/SPI.csv")

}