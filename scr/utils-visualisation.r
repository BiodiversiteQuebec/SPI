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
    SPI <- read.csv("results/SPI.csv")[,-1]

    names <- as.character(1992:2017)
    years <- as.numeric(1992:2017)
    names(SPI) <- names

    plot(years, SPI[1,], ylim = c(0,0.2),
        type='l', col='lightgrey',
        xlab='Year', ylab='SPI',
        ...)
    for (i in 2:dim(SPI)[1]) {
        lines(years, SPI[i,], type='l', col='lightgrey')
    }

    # Stransform the data to a long format
    SPI_long <- reshape2::melt(SPI)
    names(SPI_long) <- c("year", "SPI")
    SPI_long$year <- sub("X", "", SPI_long$year) |> as.numeric()

    # Add a treandline
    ## Mean
    lines(years, colMeans(SPI, na.rm = TRUE), type='l', col='black', lwd=2)
    ## Linear regression
    lm(SPI_long$SPI ~ SPI_long$year) |> abline(lwd=2, col='red', lty=2)

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

    names <- as.character(1992:2017)
    names(SPI) <- names

    hist(SPI[,as.character(year)], breaks = 20, main = paste("SPI scores in", year),
        xlab = "SPI score", ylab = "Number of species")

    old_par <- par()
    par(mfrow = c(2,3))
    for (year in names) {
        breaks <- c("1992", "1998", "2003", "2008", "2012", "2017")
        if(!(year %in% breaks)) {
            next
        } else {
            hist(SPI[,as.character(year)], breaks = 20, 
            main = paste("SPI scores in", year),
            xlab = "SPI score", ylab = "Number of species",
            xlim = c(0, 0.2))
        }
    }   
    par(old_par) 
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
    SPI <- read.csv("results/SPI.csv")
    rownames(SPI) <- SPI[,1]
    SPI <- SPI[,-1]
    groups <- read.csv("data/sp_groups.csv", sep = ";", header = TRUE)
    groups_of_interest <- c("oiseau_proie", "milieux_humides", "forestiers", "prairie", "insectivores")
    names(groups_of_interest) <- c("Birds of prey", "Wetland species", "Forest species", "Prairie species", "Insectivores")

    names <- as.numeric(1992:2017)
    names(SPI) <- names

    old_par <- par()
    par(mfrow = c(2,3))

    # Plot all species
    plot_SPI_time_series(main = "All species")

    # Plot species by group
    for (i in seq_along(groups_of_interest)) {
        which_rows <- which(rownames(SPI) %in% groups[!is.na(groups[,groups_of_interest[i]]),"species"])
        sub <- SPI[which_rows,] 

        plot(years, sub[1,], ylim = c(0,0.2),
        type='l', col='lightgrey',
        xlab='Year', ylab='SPI',
        main = names(groups_of_interest)[i])
        for (i in 2:dim(sub)[1]) {
            lines(years, sub[i,], type='l', col='lightgrey')
        }

            # Stransform the data to a long format
            sub_long <- reshape2::melt(sub)
            names(sub_long) <- c("year", "SPI")
            sub_long$year <- sub("X", "", sub_long$year) |> as.numeric()

            # Add a treandline
            ## Mean
            lines(years, colMeans(sub, na.rm = TRUE), type='l', col='black', lwd=2)
            ## Linear regression
            lm(sub_long$SPI ~ sub_long$year) |> abline(lwd=2, col='red', lty=2)
    }
    par(old_par)
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