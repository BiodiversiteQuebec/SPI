# Function to run SPI computation
#
# Parameters:
# - SPECIES: A character vector specifying the species of interest.
# - YEAR: An integer specifying the year of interest.
# - PROTECTED_AREA_TYPE: A character vector specifying the type of protected area.
# - UNION: A logical value indicating whether to perform a union of protected areas.
#
# Returns:
# - A dataframe representing the SPI value.
#
run_SPI_computation <- function(SPECIES, YEAR, PROTECTED_AREA_TYPE = "", UNION = FALSE){
    #------------------------------------------------------------------------------
    # 1. Load libraries
    #------------------------------------------------------------------------------
    library(sf)
    
    #------------------------------------------------------------------------------
    # 2. Load range_maps
    #
    # Range maps were first extracted using the "scr/utils-extract_data.r" script
    #------------------------------------------------------------------------------
    # Species Range maps
    range_maps <- st_read("data_clean/aires_repartition.gpkg", quiet = TRUE)
    
    # Subset species
    range_maps <- range_maps[range_maps$NOM_SCIENT %in% SPECIES,]
    
    #------------------------------------------------------------------------------
    # 3. Load protected areas
    #------------------------------------------------------------------------------
    if(UNION & PROTECTED_AREA_TYPE == "" & file.exists("data_clean/aires_union.gpkg")) {
        # Select all protected areas
        aires_prot <- st_read("data_clean/aires_union.gpkg", promote_to_multi = FALSE, quiet = TRUE)
        
        # Subset years of interest
        aires_prot <- aires_prot[aires_prot$annee_creation <= YEAR,]
    } else {
        # Protected areas
        aires_prot <- st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE)
        
        # Select protected areas by year 
        aires_prot$annee_creation <- as.numeric(substr(aires_prot$DA_CREATIO, start = 1, stop = 4))
        aires_prot <- aires_prot[aires_prot$annee_creation <= YEAR,]
        
        # Select protected areas by type
        if (PROTECTED_AREA_TYPE != "") {
            aires_prot <- aires_prot[aires_prot$DESIG_GR %in% PROTECTED_AREA_TYPE,]
        }
        
        # Union of protected areas ?
        if (UNION) {
            aires_prot <- aires_prot |> st_union() |> st_as_sf() |> suppressWarnings()
        }
    }
    
    #------------------------------------------------------------------------------
    # 3. Compute SPI
    #
    # This code loops through species to compute SPI for each year
    #------------------------------------------------------------------------------
    cat("Computing SPI for", SPECIES, YEAR, "\n")
    
    # Compute SPI as a proportion of range within protected areas
    intersect <- suppressWarnings(sf::st_intersection(range_maps, aires_prot))
    SPA <- sf::st_area(intersect) |> as.numeric() |> sum() |> suppressWarnings()
    SPI <- SPA / sum(as.numeric(sf::st_area(range_maps)))
    
    cat("SPI is", SPI, "\n")
    
    return(data.frame(
        SPECIES = SPECIES,
        SPI = SPI,
        YEAR = YEAR,
        SELECTED_PROTECTED_AREA_TYPE = PROTECTED_AREA_TYPE,
        UNION = UNION))
}

# Test SPI computation
# SPECIES = "Anaxyrus americanus" # Species analyzed
# YEAR = 1990 # Years of creation of protected areas of interest (all years before this year will also be considered)
# PROTECTED_AREA_TYPE = c("Parc national du Québec") # Types of protected areas to consider (unique(aires_prot$DESIG_GR))
# UNION = TRUE # Union of protected areas ?
# SPI <- run_SPI_computation(SPECIES, YEAR, PROTECTED_AREA_TYPE, UNION)
