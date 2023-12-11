## Packages

library(mgcv)
library(dplyr)
library(plotrix)
library(sf)

## Data

SPI <- read.csv("results/SPI.csv")[, -1]

names <- as.character(unique(SPI$SPECIES))
years <- as.numeric(unique(SPI$YEAR))

#### FIGURE 1 ####
# --> trend of min, max and mean SPI smoothed with gam #

## SPI visualisation for all species
# ----------------------------------

plot(years, SPI$SPI[SPI$SPECIES == names[1]],
    ylim = c(0, 0.7),
    type = "l", col = "lightgrey",
    xlab = "Year", ylab = "SPI", ...
)
for (i in names[-1]) {
    lines(years, SPI$SPI[SPI$SPECIES == i], type = "l", col = "lightgrey")
}

# mean SPI
# --------
year_mean <- c()
for (i in years) {
    sub_year <- SPI$SPI[SPI$YEAR == i]
    year_mean <- c(year_mean, mean(sub_year, na.rm = TRUE))
}

# gam on mean values
# ------------------
g_mean <- mgcv::gam(year_mean ~ s(years, k = 10))
pred_mean <- mgcv::predict.gam(g_mean, se.fit = F, type = "response")

# looking for min and max species
# -------------------------------
synth <- SPI |>
    group_by(SPECIES) |>
    summarise(sum = sum(SPI))
synth[synth$sum == max(synth$sum) | synth$sum == min(synth$sum), ]

# max species => Pseudacris maculata
# min species => Desmognathus ochrophaeus

# gam for min and max species
# ---------------------------
df_max <- SPI[SPI$SPECIES == "Pseudacris maculata", ]
df_min <- SPI[SPI$SPECIES == "Desmognathus ochrophaeus", ]

g_max <- gam(SPI ~ s(YEAR, k = 5), data = df_max, family = "quasibinomial")
pred_max <- predict.gam(g_max, type = "response", se.fit = F)

g_min <- gam(SPI ~ s(YEAR, k = 4), data = df_min, family = "quasibinomial")
pred_min <- predict.gam(g_min, type = "response", se.fit = F)

# figure production
# -----------------

# define the gap for the y axis
from <- 0.2
to <- 0.5

gap.plot(years,
    pred_max,
    gap = c(from, to),
    # axes = F,
    xtics = seq(1880, 2020, 10),
    type = "l",
    lwd = 3,
    xlab = "année",
    ylab = "SPI"
) # with a gapped y axis
axis.break(2, from, breakcol = "snow", style = "gap")
axis.break(2, from * (1 + 0.02), breakcol = "black", style = "slash")
axis.break(4, from * (1 + 0.02), breakcol = "black", style = "slash")
grid(col = "grey")

lines(years,
    pred_min,
    lwd = 3
)
lines(years,
    pred_mean,
    lwd = 3,
    col = "#0b9241"
)

#### Figure 2 ####
# --> barplot of ordered SPI

# SPI in 2023 for all species
# ---------------------------

last_spi <- SPI[SPI$YEAR == 2023, ]
last_spi <- last_spi[order(last_spi$SPI), ]
head(last_spi)
tail(last_spi)

# north vs. south species
# -----------------------

range_maps <- st_read("data_clean/aires_repartition.gpkg", quiet = TRUE)
SPI <- read.csv("results/SPI.csv")[, -1]

# for segregation btw North and South
# We use the latitude of the center of the area to determine this. Then convert to degrees and compare with lat 50 (aprox of tree line)
centroid <- range_maps |>
    st_transform(4326) |>
    st_make_valid() |>
    st_centroid() |>
    st_coordinates()

southern_sp <- range_maps[which(centroid[, 2] <= 50), ] |>
    st_drop_geometry() |>
    dplyr::select(NOM_SCIENT) |>
    unique()
southern_sp$loc <- "South"

northern_sp <- range_maps[which(centroid[, 2] > 50), ] |>
    st_drop_geometry() |>
    dplyr::select(NOM_SCIENT) |>
    unique()
northern_sp$loc <- "North"

spe_loc <- rbind(northern_sp, southern_sp)
names(spe_loc) <- c("SPECIES", "LOC")

# left join between last_spi & spe_loc
last_spi <- left_join(last_spi, spe_loc, by = "SPECIES")

# barplot production
# ------------------

colors <- ifelse(last_spi$LOC == "North", "#37c9ae", "darkorange")
barplot(last_spi$SPI,
    horiz = T,
    col = colors,
    border = colors,
    # names.arg = last_spi$SPECIES,
    # las = 1,
    xlab = "SPI",
    ylab = "Espèce"
)
abline(v = 0.3, col = "darkgrey", lty = "dotted", lwd = 2)
legend("bottomright",
    legend = c("Espèce nordique", "Espèce sudiste"),
    fill = c("#37c9ae", "darkorange"),
    border = c("#37c9ae", "darkorange"),
    bty = "n"
)

#### Figure 3 ####
# --> map of protected areas
aires_prot <- suppressWarnings(st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE))
x11()
plot(st_geometry(aires_prot))

aires_prot_union <- aires_prot |>
    st_union() |> # st_cast("POLYGON")
    st_as_sf()

# st_write(aires_prot_union,
# "/home/claire/BDQC-GEOBON/GITHUB/SPI/data_clean/aires_protegees_union.gpkg")

aire_prot_union <- st_read("/home/claire/BDQC-GEOBON/GITHUB/SPI/data_clean/aires_protegees_union.gpkg")
mapview::mapview(aires_prot_union)
