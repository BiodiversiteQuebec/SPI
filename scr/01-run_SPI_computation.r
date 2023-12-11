# Function to run SPI computation
#
# Parameters:
# - SPECIES: A character vector specifying the species of interest.
# - YEAR: An integer vector specifying the year(s) of interest.
# - PROTECTED_AREA_TYPE: A character vector specifying the type of protected area.
# - UNION: A logical value indicating whether to perform a union of protected areas.
#
# Returns:
# - A dataframe representing the SPI value.
#
run_SPI_computation <- function(SPECIES, YEAR, PROTECTED_AREA_TYPE = "", UNION = FALSE){
    cat("Computing SPI for", SPECIES, "\n")

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
    occurences <- st_read("data_raw/emvs_dq.gpkg", quiet = TRUE)
    
    # Subset species
    occurences_sp <- occurences[occurences$SNAME %in% SPECIES,]
    
    #------------------------------------------------------------------------------
    # 3. Load protected areas
    #------------------------------------------------------------------------------
    # if(UNION & PROTECTED_AREA_TYPE == "" & file.exists("data_clean/aires_union.gpkg")) {
    #     # Select all protected areas
    #     aires_prot <- st_read("data_clean/aires_union.gpkg", promote_to_multi = FALSE, quiet = TRUE)
        
    #     # Subset years of interest
    #     aires_prot <- aires_prot[aires_prot$year <= YEAR,]
    # } else {
    # Protected areas
    aires_prot <- suppressWarnings(st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE))
    
    # Select protected areas by year 
    aires_prot$year <- as.numeric(substr(aires_prot$DA_CREATIO, start = 1, stop = 4))
    
    # Select protected areas by type
    if (PROTECTED_AREA_TYPE != "") {
        aires_prot <- aires_prot[aires_prot$DESIG_GR %in% PROTECTED_AREA_TYPE,]
    }
    
    # Union of protected areas ?
    aires_org <- aires_prot
    spi_vect <- c()
    YEAR_sp <- YEAR[YEAR %in% aires_org$year] # Subset to years specifific to the sp.
    for (year in YEAR_sp){
        aires <- aires_org[aires_org$year <= year,]
        if (UNION) aires <- aires |> st_union() |> st_as_sf() |> suppressWarnings()
        spi_year <- spi(occurences_sp, aires)
        spi_vect <- c(spi_vect, spi_year)

        cat("SPI", year, " is", spi_year, "\n")
    }

    return(data.frame(
        SPECIES = SPECIES,
        SPI = spi_vect,
        YEAR = YEAR_sp,
        SELECTED_PROTECTED_AREA_TYPE = PROTECTED_AREA_TYPE,
        UNION = UNION))
}
    
#------------------------------------------------------------------------------
# Helper function to compute SPI
#------------------------------------------------------------------------------    
spi <- function(range_maps, aires_prot){    
    intersect <- suppressWarnings(sf::st_intersection(range_maps, aires_prot))
    SPA <- sf::st_area(intersect) |> as.numeric() |> suppressWarnings() |> sum()
    
    if (length(SPA) == 0) return(0)

    SPI <- SPA / sum(as.numeric(sf::st_area(range_maps)))
    
    return(SPI)
}

# Test SPI computation
# SPECIES = "Anaxyrus americanus" # Species analyzed
# YEAR = 1990 # Years of creation of protected areas of interest (all years before this year will also be considered)
# PROTECTED_AREA_TYPE = c("Parc national du Québec") # Types of protected areas to consider (unique(aires_prot$DESIG_GR))
# UNION = TRUE # Union of protected areas ?
# SPI <- run_SPI_computation(SPECIES, YEAR, PROTECTED_AREA_TYPE, UNION)
