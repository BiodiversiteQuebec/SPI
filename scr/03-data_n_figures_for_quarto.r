library(plotly)
library(sf)
library(terra)
library(htmltools)
library(viridis)
library(smoothr)
library(rmapshaper)
library(leaflet)
library(ratlas)
library(dplyr)

# ------------------------------------------------------------ #
#### Function for drawing a vertical line on a plotly graph ####
# ------------------------------------------------------------ #
# fonction for drawing a vertical line
vline <- function(x = 0, color = "#238A8DFF") {
    list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = "dot")
    )
}


# ------------------------------------------ #
#### figure 1 - carte des aires protégées ####
# ------------------------------------------ #
aires <- st_read("data_clean/aires_protegees_simplifiees.gpkg")

# conversion en lat lon pour visualisation dans leaflet
aires_latlon <- st_transform(
                    aires,
                    crs = st_crs(4326))
# popup infos
aires_latlon$POPINFOS <- paste0(
        "<b>", aires_latlon$NOM,
        "</b><br> date de création: <b>", aires_latlon$DA_CREATIO,
        "</b><br> superficie: <b>", round(aires_latlon$HA_LEGAL, digit = 1), " ha </b>")

# ----------------------------------------------------------------------- #
#### figure 2A - courbes de tendance de SPI à partir aire distribution ####
# --------------------------------------------------------------------- #

SPI <- read.csv("results/SPI.csv")[, -1]
# error in species names
SPI$SPECIES[SPI$SPECIES == "Lyn rufus"] <- "Lynx rufus"
species <- as.character(unique(SPI$SPECIES))
years <- as.numeric(unique(SPI$YEAR))

# retrieve informations about species from Atlas
# info_spe <- get_taxa(scientific_name = species)
# info_spe <- info_spe[, c(2, 3, 5, 6, 7, 8)] # need to treat the data !
# info_ls <- split(info_spe, info_spe$valid_scientific_name)
# infos <- lapply(info_ls, function(x) {
#     l <- x[1, ]
#     l
# })

# info_spe2 <- do.call("rbind", infos)
# write.csv(info_spe2, "data_clean/ATLAS_info_spe_distri.csv")
info_spe2 <- read.csv("data_clean/ATLAS_info_spe_distri.csv")[,-1]
# Complete the empty values
spe_NA <- read.csv("data_raw/spe_infos_supp.csv")
info_spe2 <- rbind(info_spe2, spe_NA)

# smoothing the curves
spi_ls <- split(SPI, SPI$SPECIES)

spi_smoo <- lapply(spi_ls, function(x){
    m <- cbind(x$YEAR, x$SPI)
    sm <- smooth_ksmooth(m, smoothness = 4)
    df <- as.data.frame(sm)
    names(df) <- c("YEAR", "SPI")
    df$SPECIES <- unique(x$SPECIES)
    df$POPINFOS <- paste0(
        "<b>", df$SPECIES,
        "</b><br>Année <b>", floor(df$YEAR),
        "</b><br>SPI = <b>", round(df$SPI, digits = 3), "</b>")
    df
})

sspi_df <- do.call("rbind", spi_smoo)

# computation of mean SPI
year_mean <- c()
for (i in years) {
    sub_year <- SPI$SPI[SPI$YEAR == i]
    year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
}
s_mean_spi <- smooth_ksmooth(as.matrix(cbind(years, year_mean)))
    df <- as.data.frame(s_mean_spi)
    names(df) <- c("YEAR", "SPI")
    df$SPECIES <- "Valeur moyenne"
    df$POPINFOS <- paste0(
        "<b>", df$SPECIES,
        "</b><br>Année <b>", floor(df$YEAR),
        "</b><br>SPI = <b>", round(df$SPI, digits = 3), "</b>")

sspi_df <- rbind(sspi_df, df)

# Association d'un groupe par catégories - max, min & mean 
spi2023 <- SPI[SPI$YEAR == 2023,]
range(spi2023$SPI)
max_spe <- spi2023$SPECIES[spi2023$SPI == max(spi2023$SPI)]
min_spe <- spi2023$SPECIES[spi2023$SPI == min(spi2023$SPI)]

sspi_df$GROUPE[sspi_df$SPECIES == "Valeur moyenne"] <- "mean"
sspi_df$GROUPE[sspi_df$SPECIES == max_spe] <- "max"
sspi_df$GROUPE[sspi_df$SPECIES == min_spe] <- "min"
sspi_df$GROUPE[is.na(sspi_df$GROUPE)] <- "other"

# adding species info from Atlas (et al) with a left_join
sspi_df <- left_join(sspi_df, info_spe2, by = join_by("SPECIES" == "observed_scientific_name"))

# table(sspi_df$GROUPE, useNA = "always")

# g <- plot_ly(
#     type = "scatter",
#     x = sspi_df$YEAR[sspi_df$GROUPE == "mean"],
#     y = sspi_df$SPI[sspi_df$GROUPE == "mean"],
#     text = sspi_df$POPINFOS[sspi_df$GROUPE == "mean"],
#     hoverinfo = "text",
#     mode = "lines")
#     ,
#     transforms = list(
#         list(
#             type = "groupby",
#             groups = sspi_df$GROUPE,
#             styles = list(
#                 list(target = "min", value = list(marker = list(color = "#B8DE29FF"))),
#                 list(target = "max", value = list(marker = list(color = "#404788FF"))),
#                 list(target = "mean", value = list(marker = list(color = "#238A8DFF"))),
#                 list(target = "other", value = list(marker = list(color = "grey")))
#             )
#         )
#     )
# ) 

# ----------------------------------------------------------------------- #
#### figure 2B - courbes de tendance de SPI à partir aire distribution ####
# --------------------------------------------------------------------- #

SOCC <- read.csv("results/SPI_OCC.csv")
SOCC <- SOCC[!(SOCC$SPECIES == "Information masquée"),]
species <- as.character(unique(SOCC$SPECIES))
years <- as.numeric(unique(SOCC$YEAR))

# smoothing the curves
spi_ls <- split(SOCC, SOCC$SPECIES)

spi_smoo <- lapply(spi_ls, function(x){
    m <- cbind(x$YEAR, x$SPI)
    sm <- smooth_ksmooth(m, smoothness = 4)
    df <- as.data.frame(sm)
    names(df) <- c("YEAR", "SPI")
    df$SPECIES <- unique(x$SPECIES)
    df$POPINFOS <- paste0(
        "<b>", df$SPECIES,
        "</b><br>Année <b>", floor(df$YEAR),
        "</b><br>SPI = <b>", round(df$SPI, digits = 3), "</b>")
    df
})

sspi_df_occ <- do.call("rbind", spi_smoo)

# computation of mean SPI
year_mean <- c()
for (i in years) {
    sub_year <- SOCC$SPI[SOCC$YEAR == i]
    year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
}
s_mean_spi_occ <- smooth_ksmooth(as.matrix(cbind(years, year_mean)))
    df <- as.data.frame(s_mean_spi_occ)
    names(df) <- c("YEAR", "SPI")
    df$SPECIES <- "Valeur moyenne"
    df$POPINFOS <- paste0(
        "<b>", df$SPECIES,
        "</b><br>Année <b>", floor(df$YEAR),
        "</b><br>SPI = <b>", round(df$SPI, digits = 3), "</b>")

sspi_df_occ <- rbind(sspi_df_occ, df)

# Association d'un groupe par catégories - max, min & mean 
spi2023 <- SOCC[SOCC$YEAR == 2023,]
range(spi2023$SPI)
max_spe <- spi2023$SPECIES[spi2023$SPI == max(spi2023$SPI)]
min_spe <- spi2023$SPECIES[spi2023$SPI == min(spi2023$SPI)]

sspi_df_occ$GROUPE[sspi_df_occ$SPECIES == "Valeur moyenne"] <- "mean"
sspi_df_occ$GROUPE[sspi_df_occ$SPECIES == max_spe] <- "max"
sspi_df_occ$GROUPE[sspi_df_occ$SPECIES %in% min_spe] <- "min"
sspi_df_occ$GROUPE[is.na(sspi_df_occ$GROUPE)] <- "other"

# ---------------------------------------------------------- #
#### figure 3A - barchat SPI 2023 par groupe taxonomique ####
# -------------------------------------------------------- #
# select data
last_spi <- SPI[SPI$YEAR == 2023,]
# fill informations about species
last_spi <- left_join(last_spi, info_spe2, by = join_by("SPECIES" == "observed_scientific_name"))
