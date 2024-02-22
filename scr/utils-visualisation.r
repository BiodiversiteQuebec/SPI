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
    if (!is.null(base_map)) {
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
    SPI <- read.csv("results/SPI.csv")[, -1]

    names <- as.character(unique(SPI$SPECIES))
    years <- as.numeric(unique(SPI$YEAR))

    plot(years, SPI$SPI[SPI$SPECIES == names[1]],
        ylim = c(0, 0.7),
        type = "l", col = "lightgrey",
        xlab='Année', ylab='SPI', ...
    )
    for (i in names[-1]) {
        lines(years, SPI$SPI[SPI$SPECIES == i], type = "l", col = "lightgrey")
    }

    # Stransform the data to a long format
    year_mean <- c()
    for (i in years) {
        sub_year <- SPI$SPI[SPI$YEAR == i]
        year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
    }

    # Add a treandline
    ## Mean
    lines(years, year_mean, type = "l", col = "black", lwd = 2)
    ## Linear regression
    lm(SPI$SPI ~ SPI$YEAR) |> abline(lwd = 2, col = "red", lty = 2)
    ## GAM - all data
    g <- mgcv::gam(SPI ~ s(YEAR, k = 10), data = SPI)
    newdf <- data.frame(YEAR = years)
    pred <- mgcv::predict.gam(g, newdf, se.fit = F, type = "response")
    lines(years, pred, lwd = 2, col = "#00fff7", lty = 1)

    # Add a legend
    legend("topleft",
        legend = c("valeurs moyennes", "tendance linéaire", "gam"),
        col = c("black", "red", "#00fff7"),
        lty = c(1, 2, 1),
        bty = "n"
    )
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
    SPI <- read.csv("results/SPI.csv")[, -1]

    years <- as.numeric(unique(SPI$YEAR)) |> sort()

    old_par <- par()
    par(mfrow = c(2, 3))

    hist(SPI$SPI,
        breaks = 20, main = paste("Toutes les années"),
        xlab = "Score SPI", ylab = "Nombre d'espèces"
    )

    for (year in years) {
        breaks <- c("1950", "1980", "1990", "2000", "2010", "2020")
        if (!(year %in% breaks)) {
            next
        } else {
            hist(SPI$SPI[SPI$YEAR == year],
                breaks = seq(0, 1, by = 0.05),
                main = paste("Score SPI en", year),
                xlab = "Score SPI", ylab = "Nombre d'espèces",
                xlim = c(0, 0.4)
            )
        }
    }
    suppressWarnings(par(old_par))
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
    SPI <- read.csv("results/SPI.csv")[, -1]
    groups <- list(
        Amphibiens = "data_raw/Aires_repartition_amphibiens.gpkg",
        Mammifères_terrestres = "data_raw/Aires_repartition_MT.gpkg",
        Poissons_eau_douce = "data_raw/Aires_repartition_poisson_eau_douce.gpkg",
        Reptiles = "data_raw/Aires_repartition_reptiles.gpkg"
    )

    old_par <- par()
    par(mfrow = c(2, 3))

    # Plot all species
    plot_SPI_time_series(main = "Toutes les espèces")

    # Plot species by group
    for (i in seq_along(groups)) {
        group_members <- sf::read_sf(groups[i])$NOM_SCIENT
        which_rows <- which(SPI$SPECIES %in% group_members)
        sub <- SPI[which_rows, ]
        sub_sp <- unique(sub$SPECIES)

        # Plot first line
        plot(sub$YEAR[sub$SPECIES == sub_sp[1]], sub$SPI[sub$SPECIES == sub_sp[1]],
            ylim = c(0, 0.7),
            type = "l", col = "lightgrey",
            xlab = "Année", ylab = "SPI",
            main = names(groups)[i]
        )
        for (sp in sub_sp) {
            lines(sub$YEAR[sub$SPECIES == sp], sub$SPI[sub$SPECIES == sp], type = "l", col = "lightgrey")
        }

        # Add lines for each species
        years <- unique(sub$YEAR)
        year_mean <- c()
        for (i in years) {
            sub_year <- sub$SPI[sub$YEAR == i]
            year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
        }

        # Add a treandline
        ## Mean
        lines(years, year_mean, type = "l", col = "black", lwd = 2)
        ## Linear regression
        lm(sub$SPI ~ sub$YEAR) |> abline(lwd = 2, col = "red", lty = 2)
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
    rownames(SPI) <- SPI[, 1]
    SPI <- SPI[, -1]

    years <- as.numeric(1992:2017)
    names(SPI) <- years

    # Subset the data
    which_rows <- which(rownames(SPI) %in% cdpnq_list)
    sub <- SPI[which_rows, ]

    plot(years, sub[1, ],
        ylim = c(0, 0.2),
        type = "l", col = "lightgrey",
        xlab = "Year", ylab = "SPI",
        main = "Espèces à statut précaire au Québec"
    )
    for (i in 2:dim(sub)[1]) {
        lines(years, sub[i, ], type = "l", col = "lightgrey")
    }

    # Stransform the data to a long format
    sub_long <- reshape2::melt(sub)
    names(sub_long) <- c("Année", "SPI")
    sub_long$year <- sub("X", "", sub_long$year) |> as.numeric()

    # Add a trendline
    ## Mean
    lines(years, colMeans(sub, na.rm = TRUE), type = "l", col = "black", lwd = 2)
    ## Linear regression
    lm(sub_long$SPI ~ sub_long$year) |> abline(lwd = 2, col = "red", lty = 2)
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

    time_series <- prot_area[, c("DA_CREATIO", "HA_LEGAL")] |> as.data.frame()
    time_series$annee <- as.numeric(substr(time_series$DA_CREATIO, start = 1, stop = 4))

    area <- c()
    for (i in unique(time_series$annee)) {
        sum <- sum(time_series$HA_LEGAL[time_series$annee == i])
        area <- c(area, sum)
    }

    dat <- data.frame(
        annee = unique(time_series$annee),
        area = area
    )

    dat <- dat[order(dat$annee), ]
    dat$area_cum <- cumsum(area)
    dat$area_prop <- dat$area_cum / 166800000
    names(dat) <- c("annee", "area", "area_cum", "area_prop")


    plot(dat$annee, dat$area_prop,
        col = "black",
        xlab = "Année", ylab = "Proportion du territoire protégé", main = "Territoire portégé au Québec"
    )
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
    SPI <- read.csv("results/SPI_ranges.csv")

    names <- as.character(unique(SPI$SPECIES))
    years <- as.numeric(unique(SPI$YEAR)) |> sort()

    old_par <- par()
    par(mfrow = c(1, 2))
    # Plot Southern species
    plot(SPI$YEAR[SPI$SPECIES == names[1]], SPI$SPI_SOUTH[SPI$SPECIES == names[1]],
        ylim = c(0, 0.7),
        type = "l", col = "lightgrey",
        xlab = "Année", ylab = "SPI", main = "Territoire sous le 55e parallèle"
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
        ylim = c(0, 0.7),
        type = "l", col = "lightgrey",
        xlab = "Année", ylab = "SPI", main = "Territoire au delà du 55e parallèle"
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

# TO SAVE THE PLOTS
# png("results/figures/RANGES_SPI_regions.png", width = 3000, height = 2000, res = 300, bg = "transparent")
# plot_SPI_regions()
# dev.off()


###############################################################################
# Plot SPI by classes of range sizes
#
# Arguments:
#
# Author: Victor Cameron
# Date: 2023-12-20
###############################################################################
plot_SPI_range_size <- function() {
    SPI <- read.csv("results/SPI.csv")[, -1]

    # Read the range maps
    range_maps <- st_read("data_clean/aires_repartition.gpkg", quiet = TRUE)

    # Compute the range size
    range_maps$range_size <- st_area(range_maps) |> as.numeric()

    # Compute the range size classes
    range_maps$range_size_class <- cut(range_maps$range_size, breaks = c(0, 100, 1000, 10000, 100000, 1000000, Inf))

    # Compute the mean SPI for each species
    SPI$mean_SPI <- ave(SPI$SPI, SPI$SPECIES, FUN = mean)

    # Merge the two data frames
    SPI <- merge(SPI, range_maps, by = "NOM_SCIENT")

    # Plot the data
    plot(SPI$range_size, SPI$mean_SPI,
        xlab = "Taille de l'aire de répartition (km2)",
        ylab = "SPI moyen",
        log = "x"
    )
    abline(lm(SPI$mean_SPI ~ SPI$range_size), col = "red")
}


# TO SAVE THE PLOTS
# png("results/SPI_scores.png", width = 3000, height = 2000, res = 300)
# plot_SPI_scores()
# dev.off()
