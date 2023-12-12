###############################################################################
# Plot species range map
#
# Arguments:
#   - range_map: SpatRaster object containing the range map of the species.
#                Multiple layers may be provided such as years
#   - base_map: sf object containing the basemap to plot the range map over
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_range_map <- function(range_map, base_map = NULL) {
    # Plot basemap
    if(!is.null(base_map)) {
        plot(base_map)
        add <- TRUE
    } else {
        add <- FALSE
    }

    # Plot the distribution over the basemap semi-transparent
    plot(range_map$geom, col = 4, add = add)
}


###############################################################################
# Plot SPI time series
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_SPI_time_series <- function(...) {
    SPI <- read.csv("results/SPI.csv")

    names <- as.character(unique(SPI$SPECIES))
    years <- as.numeric(unique(SPI$YEAR))

    plot(years, SPI$SPI[SPI$SPECIES == names[1]], 
        ylim = c(0,1), xlim = c(1880, 2025),
        type='l', col='lightgrey',
        xlab='Year', ylab='SPI', ...)
    for (i in names[-1]) {
        lines(years, SPI$SPI[SPI$SPECIES == i], type='l', col='lightgrey')
        # Sys.sleep(0.1)
    }

    # Stransform the data to a long format
    year_mean <- c()
    for (i in years) {
        sub_year <- SPI$SPI[SPI$YEAR == i]
        year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
    }

    # Add a treandline
    ## Mean
    lines(years, year_mean, type='l', col='black', lwd=2)
    ## Linear regression
    lm(SPI$SPI ~ SPI$YEAR) |> abline(lwd=2, col='red', lty=2)

}


###############################################################################
# Plot Species Protection Scores
#
# Arguments:
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_SPI_scores <- function() {
    SPI <- read.csv("results/SPI.csv")[,-1]

    years <- as.numeric(unique(SPI$YEAR)) |> sort()
    
    old_par <- par()
    par(mfrow = c(2,3))

    hist(SPI$SPI, breaks = 20, main = paste("All SPI scores"),
        xlab = "SPI score", ylab = "Number of species")

    for (year in years) {
        breaks <- c("1950", "1980", "1990", "2000", "2010", "2020")
        if(!(year %in% breaks)) {
            next
        } else {
            hist(SPI$SPI[SPI$YEAR == year], breaks = seq(0,1, by = 0.05), 
                main = paste("SPI scores in", year),
                xlab = "SPI score", ylab = "Number of species",
                xlim = c(0, 0.4))
        }
    }   
    suppressWarnings(par(old_par) )
}


###############################################################################
# Plot Species Protection Scores by group
#
# Arguments:
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_SPI_by_group <- function() {
    library(sf)
    SPI <- read.csv("results/SPI.csv")
    occurences <- st_read("data_raw/emvs_dq.gpkg", quiet = TRUE)
    groups <- unique(occurences$GGROUPE)

    old_par <- par()
    par(mfrow = c(2,4))

    # Plot all species
    plot_SPI_time_series(main = "All species")

    # Plot species by group
    for (i in seq_along(groups)) {
        which_sp <- occurences$SNAME[occurences$GGROUPE == groups[i]] |> unique()
        sub <- SPI[SPI$SPECIES %in% which_sp,] 
        sub_sp <- unique(sub$SPECIES)

        # Plot first line
        plot(sub$YEAR[sub$SPECIES == sub_sp[1]], sub$SPI[sub$SPECIES == sub_sp[1]],
            ylim = c(0,1), xlim = c(1880, 2025),
            type='l', col='lightgrey',
            xlab='Year', ylab='SPI',
            main = groups[i])
        for (sp in sub_sp) {
            lines(sub$YEAR[sub$SPECIES == sp], sub$SPI[sub$SPECIES == sp], type='l', col='lightgrey')
        }

        # Add lines for each species
        years <- unique(sub$YEAR) |> sort()
        year_mean <- c()
        for (i in years) {
            sub_year <- sub$SPI[sub$YEAR == i]
            year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
        }

        # Add a treandline
        ## Mean
        lines(years, year_mean, type='l', col='black', lwd=2)
        ## Linear regression
        lm(sub$SPI ~ sub$YEAR) |> abline(lwd=2, col='red', lty=2)
    }

    # Plot protected area
    plot_protected_area()

    suppressWarnings(par(old_par))
}


###############################################################################
# Plot Species at risk (CDPNQ)
#
# Arguments:
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_SPI_at_risk <- function() {
    # cdpnq list of species at risk
    # https://www.quebec.ca/gouvernement/gouvernement-ouvert/transparence-performance/indicateurs-statistiques/donnees-especes-situation-precaire
    cdpnq_list <- c("Aquila chrysaetos", "Centronyx henslowii", "Falco peregrinus", "Melanerpes erythrocephalus", "Lanius ludovicianus")

    # Read the data
    SPI <- read.csv("results/SPI.csv")
    rownames(SPI) <- SPI[,1]
    SPI <- SPI[,-1]

    years <- as.numeric(1992:2017)
    names(SPI) <- years

    # Subset the data
    which_rows <- which(rownames(SPI) %in% cdpnq_list)
    sub <- SPI[which_rows,] 

    plot(years, sub[1,], ylim = c(0,0.2),
        type='l', col='lightgrey',
        xlab='Year', ylab='SPI',
        main = "Species at risk")
    for (i in 2:dim(sub)[1]) {
            lines(years, sub[i,], type='l', col='lightgrey')
    }

    # Stransform the data to a long format
    sub_long <- reshape2::melt(sub)
    names(sub_long) <- c("year", "SPI")
    sub_long$year <- sub("X", "", sub_long$year) |> as.numeric()

    # Add a trendline
    ## Mean
    lines(years, colMeans(sub, na.rm = TRUE), type='l', col='black', lwd=2)
    ## Linear regression
    lm(sub_long$SPI ~ sub_long$year) |> abline(lwd=2, col='red', lty=2)
}


###############################################################################
# Plot protected area time series
#
# Arguments:
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################

plot_protected_area <- function() {
    prot_area <- sf::read_sf("data_raw/registre_aires_prot.gpkg")

    time_series <- prot_area[,c("DA_CREATIO", "HA_LEGAL")] |> as.data.frame()
    time_series$annee <- as.numeric(substr(time_series$DA_CREATIO, start = 1, stop = 4))

    area <- c()
    for(i in unique(time_series$annee)) {
        sum <- sum(time_series$HA_LEGAL[time_series$annee == i])
        area <- c(area, sum)
    }

    dat <- data.frame(
        annee = unique(time_series$annee),
        area = area)
    
    dat <- dat[order(dat$annee),]
    dat$area_cum <- cumsum(area)
    names(dat) <- c("annee", "area", "area_cum")
    

    plot(dat$annee, dat$area_cum, col = "black",
        xlab = "Year", ylab = "Area (Ha)", main = "Protected area in Quebec")
}


# Represent SPI for northern and southern species
###############################################################################
# Plot SPI for northern and southern species
#
# Arguments:
#
# - SPI: SPI data frame
#
# Author: Victor Cameron
# Date: 2023-08-24
###############################################################################
plot_SPI_regions <- function(SPI, ...) {

    aires_prot <- suppressWarnings(st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE))
    range_maps <- st_read("data_clean/aires_repartition.gpkg", quiet = TRUE)
    SPI <- read.csv("results/SPI.csv")[,-1]

    # Which areas are in the north and south of the country?
    # We use the latitude of the center of the area to determine this. Then convert to degrees
    st_centroid(range_maps[1,]) |> st_coordinates() 
    centroid <- range_maps |> st_transform(4326) |> st_make_valid() |> st_centroid() |> st_coordinates() 
    southern_range <- which(centroid[,2] <= 50)
    southern_sp <- range_maps[southern_range,] |> st_drop_geometry() |> dplyr::select(NOM_SCIENT) |> unique() |> unlist()
    northern_range <- which(centroid[,2] > 50)
    northern_sp <- range_maps[northern_range,] |> st_drop_geometry() |> dplyr::select(NOM_SCIENT) |> unique() |> unlist()
    # Isolate the two first characters of the LATDMS column
    southern_aires <- which(substr(aires_prot$LATDMS, 1, 2) <= 50)
    northern_aires <- which(substr(aires_prot$LATDMS, 1, 2) > 50)

    names <- as.character(unique(SPI$SPECIES))
    years <- as.numeric(unique(SPI$YEAR)) |> sort()

    plot(SPI$YEAR[SPI$SPECIES == names[1]], SPI$SPI[SPI$SPECIES == names[1]], ylim = c(0,0.7),
        type='l', col='lightgrey',
        xlab='Year', ylab='SPI', ...)
    for (i in names[-1]) {
        lines(SPI$YEAR[SPI$SPECIES == i], SPI$SPI[SPI$SPECIES == i], type='l', col='lightgrey')
    }

    # Transform the data to a long format
    year_mean <- c()
    for (i in years) {
        sub_year <- SPI$SPI[SPI$YEAR == i]
        year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
    }

    # Add a treandline
    ## Mean
    lines(years, year_mean, type='l', col='black', lwd=2)
    ## Linear regression
    lm(SPI$SPI ~ SPI$YEAR) |> abline(lwd=2, col='red', lty=2)
}