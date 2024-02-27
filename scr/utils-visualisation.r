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
    SPI <- read.csv("results/SPI_OCC.csv")

    names <- as.character(unique(SPI$SPECIES))
    years <- as.numeric(unique(SPI$YEAR))

    plot(years, SPI$SPI[SPI$SPECIES == names[1]], 
        ylim = c(0,1), xlim = c(1880, 2025),
        type='l', col='lightgrey',
        xlab='Année', ylab='SPI', ...)
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

    # Add a legend
    legend("topleft", legend = c("Espèces", "Moyenne", "modèle linéaire"), 
        col = c("lightgrey", "black", "red"), 
        lty = c(1,1,2), lwd = c(1,2,2),
        bty = "n")
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
    SPI <- read.csv("results/SPI_OCC.csv")[,-1]

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
                ylim = c(0,400), xlim = c(0, 1))
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
    SPI <- read.csv("results/SPI_OCC.csv")
    occurences <- st_read("data_raw/emvs_dq.gpkg", quiet = TRUE)
    groups <- unique(occurences$GGROUPE)

    old_par <- par()
    par(mfrow = c(2,4))

    # Plot all species
    plot_SPI_time_series(main = "Toutes les espèces")

    # Plot species by group
    for (i in seq_along(groups)) {
        which_sp <- occurences$SNAME[occurences$GGROUPE == groups[i]] |> unique()
        sub <- SPI[SPI$SPECIES %in% which_sp,] 
        sub_sp <- unique(sub$SPECIES)

        # Plot first line
        plot(sub$YEAR[sub$SPECIES == sub_sp[1]], sub$SPI[sub$SPECIES == sub_sp[1]],
            ylim = c(0,1), xlim = c(1880, 2025),
            type='l', col='lightgrey',
            xlab='Année', ylab='SPI',
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
        if (length(sub$SPI) > 0)lm(sub$SPI ~ sub$YEAR) |> abline(lwd=2, col='red', lty=2)
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
    SPI <- read.csv("results/SPI_OCC.csv")
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
    dat$area_prop <- dat$area_cum / 166800000
    names(dat) <- c("annee", "area", "area_cum", "area_prop")
    

    plot(dat$annee, dat$area_prop, col = "black",
        xlab = "Année", ylab = "Proportion du territoire protégé", main = "Territoire portégé au Québec")
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

    # aires_prot <- suppressWarnings(st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE))
    # range_maps <- st_read("data_clean/aires_repartition.gpkg", quiet = TRUE)
    SPI <- read.csv("results/SPI_OCC.csv")

    names <- as.character(unique(SPI$SPECIES))
    years <- as.numeric(unique(SPI$YEAR)) |> sort()

    old_par <- par()
    par(mfrow = c(1, 2))
    # Plot Southern species
    plot(SPI$YEAR[SPI$SPECIES == names[1]], SPI$SPI_SOUTH[SPI$SPECIES == names[1]],
        ylim = c(0, 1),
        type = "l", col = "lightgrey",
        xlab = "Année", ylab = "SPI", main = "Territoire sous le 49e parallèle"
    )
    for (i in names[-1]) {
        lines(SPI$YEAR[SPI$SPECIES == i], SPI$SPI_SOUTH[SPI$SPECIES == i], type = "l", col = "lightgrey")
    }
    # Transform the data to a long format
    year_mean <- c()
    for (i in years) {
        sub_year <- SPI$SPI[SPI$YEAR == i]
        year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
    }
    # Add a treandline
    ## Mean
    lines(years, year_mean, type = "l", col = "black", lwd = 2)
    ## Linear regression
    lm(SPI$SPI_SOUTH ~ SPI$YEAR) |> abline(lwd = 2, col = "red", lty = 2)

    # Plot Northern species
    plot(SPI$YEAR[SPI$SPECIES == names[1]], SPI$SPI_NORTH[SPI$SPECIES == names[1]],
        ylim = c(0, 1),
        type = "l", col = "lightgrey",
        xlab = "Année", ylab = "SPI", main = "Territoire au delà du 49e parallèle"
    )
    for (i in names[-1]) {
        lines(SPI$YEAR[SPI$SPECIES == i], SPI$SPI_NORTH[SPI$SPECIES == i], type = "l", col = "lightgrey")
    }
    # Transform the data to a long format
    year_mean <- c()
    for (i in years) {
        sub_year <- SPI$SPI_NORTH[SPI$YEAR == i]
        year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
    }
    # Add a treandline
    ## Mean
    lines(years, year_mean, type = "l", col = "black", lwd = 2)
    ## Linear regression
    lm(SPI$SPI_NORTH ~ SPI$YEAR) |> abline(lwd = 2, col = "red", lty = 2)
}


###############################################################################
# Plot SPI by classes of number of occurences per species
#
# Arguments:
#
# Author: Victor Cameron
# Date: 2023-12-20
###############################################################################
plot_SPI_by_occurences <- function() {
    # Load libraries
    library(ggplot2)
    library(dplyr)
    library(sf)

    # Load data
    SPI <- read.csv("results/SPI_OCC.csv")
    occurences <- st_read("data_raw/emvs_dq.gpkg", quiet = TRUE)
    species <- unique(occurences$SNAME)

    # Remove "information masquée"
    if("Information masquée" %in% SPI$SPECIES) {
        SPI <- SPI[SPI$SPECIES != "Information masquée",]
    }
    if("Information masquée" %in% occurences$SNAME) {
        occurences <- occurences[occurences$SNAME != "Information masquée",]
    }

    # Compute the number of occurences per species
    occ_class <- occurences |>
        st_drop_geometry() |>
        group_by(SNAME) |>
        summarise(n = n()) |>
        mutate(occurence_class = case_when(
            n == 1 ~ "1",
            n > 1 & n <= 10 ~ "2-10",
            n > 10 & n <= 100 ~ "11-100",
            n > 100 ~ ">100")) |>
        # group_by(occurence_class) |>
        # summarise(n = n()) |>
        mutate(occurence_class = factor(occurence_class, levels = c("1", "2-10", "11-100", ">100"))) 
        # ggplot(aes(x = occurence_class, y = n)) +
        # geom_bar(stat = "identity") +
        # labs(x = "Number of occurences", y = "Number of species") +
        # theme_bw() +
        # theme(axis.text.x = element_text(angle = 45, hjust = 1))


    old_par <- par()
    par(mfrow = c(2,3))

    # Plot all species
    plot_SPI_time_series(main = "Toutes les espèces")

    # Plot species by group
    groups <- unique(occ_class$occurence_class) |> sort()
    for (i in seq_along(groups)) {
        sub_sp <- occ_class$SNAME[occ_class$occurence_class == groups[i]] |> unique()
        sub <- SPI[SPI$SPECIES %in% sub_sp,] 

        # Plot first line
        plot(sub$YEAR[sub$SPECIES == sub_sp[1]], sub$SPI[sub$SPECIES == sub_sp[1]],
            ylim = c(0,1), xlim = c(1880, 2025),
            type='l', col='lightgrey',
            xlab='Année', ylab='SPI',
            main = paste("Espèces avec", as.character(groups[i]), "occurence(s)"))
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