# Function to run SPI computation
#
# Parameters:
# - SPECIES: A character vector specifying the species of interest.
# - YEAR: An integer vector specifying the year(s) of interest.
# - SPLIT: TRUE/FALSE specifying if the computations should also be ran for the north and south regions.
# - PROTECTED_AREA_TYPE: A character vector specifying the type of protected area.
# - UNION: A logical value indicating whether to perform a union of protected areas.
#
# Returns:
# - A dataframe representing the SPI value.
#
run_SPI_computation <- function(SPECIES, YEAR, SPLIT = FALSE, PROTECTED_AREA_TYPE = "", UNION = FALSE){
    cat("Computing SPI for", SPECIES, "...\n")

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
    range_maps <- range_maps[range_maps$NOM_SCIENT %in% SPECIES,]
    if(SPLIT){
        range_maps_n <- st_read("data_clean/aires_repartition_nord.gpkg", quiet = TRUE)
        range_maps_n <- range_maps_n[range_maps_n$NOM_SCIENT %in% SPECIES,]
        range_maps_s <- st_read("data_clean/aires_repartition_sud.gpkg", quiet = TRUE)
        range_maps_s <- range_maps_s[range_maps_s$NOM_SCIENT %in% SPECIES,]
    }
    
    #------------------------------------------------------------------------------
    # 3. Load protected areas
    #------------------------------------------------------------------------------
    # Protected areas
    aires_prot <- st_read("data_clean/aires_union.gpkg", promote_to_multi = FALSE, quiet = TRUE)
    if(SPLIT){
        aires_s <- st_read("data_clean/aires_protegees_sud.gpkg", quiet = TRUE)
        aires_n <- st_read("data_clean/aires_protegees_nord.gpkg", quiet = TRUE)
    }
    
    # Add year to protected areas
    if (SPLIT){
        aires_s$year <- as.numeric(substr(aires_s$DA_CREATIO, start = 1, stop = 4))
        aires_n$year <- as.numeric(substr(aires_n$DA_CREATIO, start = 1, stop = 4))
    }

    # Select protected areas by type
    if (PROTECTED_AREA_TYPE != "") {
        aires_prot <- aires_prot[aires_prot$DESIG_GR %in% PROTECTED_AREA_TYPE,]
        if(SPLIT){
            aires_s <- aires_s[aires_s$DESIG_GR %in% PROTECTED_AREA_TYPE,]
            aires_n <- aires_n[aires_n$DESIG_GR %in% PROTECTED_AREA_TYPE,]
        }
    }
    
    #------------------------------------------------------------------------------
    # 4. Compute SPI
    #------------------------------------------------------------------------------
    # Loop through years
    spi_vect <- c()
    spi_vect_s <- c()
    spi_vect_n <- c()
    YEAR_sp <- YEAR[YEAR %in% aires_prot$year] # Subset to years specifific to the sp.
    YEAR_s <- YEAR[YEAR %in% aires_s$year] # Subset to years specifific to the sp.
    YEAR_n <- YEAR[YEAR %in% aires_n$year] # Subset to years specifific to the sp.
    
    for (year in YEAR_sp){

        # Total
        aires <- aires_prot[aires_prot$year <= year,]
        if (UNION) aires <- aires |> st_union() |> st_as_sf() |> suppressWarnings()
        spi_year <- spi(range_maps, aires)
        spi_vect <- c(spi_vect, spi_year)

        if (SPLIT){
            # South
            aires_year_s <- aires_s[aires_s$year <= year,]
            if (UNION) aires <- aires_year_s |> st_union() |> st_as_sf() |> suppressWarnings()
            spi_year_s <- spi(range_maps_s, aires)
            spi_vect_s <- c(spi_vect_s, spi_year_s)

            # North
            aires_year_n <- aires_n[aires_n$year <= year,]
            if (UNION) aires <- aires_year_n |> st_union() |> st_as_sf() |> suppressWarnings()
            spi_year_n <- spi(range_maps_n, aires)
            spi_vect_n <- c(spi_vect_n, spi_year_n)
        }

        cat("SPI for", year, "is", spi_year, "(total),", spi_year_s, "(S),", spi_year_n, "(N)", "\n")
    }

    return(data.frame(
            SPECIES = SPECIES,
            YEAR = YEAR_sp,
            SPI = spi_vect,
            SPI_SOUTH = spi_vect_s,
            SPI_NORTH = spi_vect_n,
            SELECTED_PROTECTED_AREA_TYPE = PROTECTED_AREA_TYPE,
            UNION = UNION))
}
    
#------------------------------------------------------------------------------
# Helper function to compute SPI
#------------------------------------------------------------------------------    
spi <- function(range_maps, aires_prot){    
    intersect <- suppressWarnings(sf::st_intersection(range_maps, aires_prot))
    SPA <- sf::st_area(intersect) |> as.numeric() |> suppressWarnings()
    
    if (length(SPA) == 0) return(0)

    SPI <- SPA / sum(as.numeric(sf::st_area(range_maps)))
    
    return(SPI)
}

# Test SPI computation
# source("scr/01-run_SPI_computation.r")
# SPECIES = "Anaxyrus americanus" # Species analyzed
# YEAR = 1990 # Years of creation of protected areas of interest (all years before this year will also be considered)
# PROTECTED_AREA_TYPE = "" # Types of protected areas to consider (unique(aires_prot$DESIG_GR))
# UNION = TRUE # Union of protected areas ?
# SPLIT = TRUE # Split range maps into total, south and north regions
# SPI <- run_SPI_computation(SPECIES, YEAR, SPLIT, PROTECTED_AREA_TYPE, UNION)
