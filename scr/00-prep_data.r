###############################################################################
# Utils scripts to prep data for SPI computation
#
#
# Author: Victor Cameron
# Date: 2024-01-19
###############################################################################
library(sf)


###############################################################################
# Range maps for the south and north regions
#
# SPLIT = 50 is the latitude where the range maps are split into two regions
# (S and N).
###############################################################################
# Params
## Qc bounding box
xmin = -80.1
ymin = 44
xmax = -57
ymax = 63
## Split latitude
SPLIT = 50

# Load occurences
occurences <- st_read("data_raw/emvs_dq.gpkg", quiet = TRUE)

# Get bounding box
box_s <- st_bbox(
  st_transform(
    st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymax = SPLIT, ymin = ymin), crs = st_crs(4326))), 
    st_crs(occurences)
  )
)
box_n <- st_bbox(
  st_transform(
    st_as_sfc(st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = SPLIT), crs = st_crs(4326))), 
    st_crs(occurences)
  )
)

# Split range maps into two regions
# Split range maps into two regions
occurences_s <- occurences |>
        st_make_valid() |>
        st_crop(box_s) |>
        suppressWarnings()
# mapview::mapview(range_maps_s[1:10,])
# plot(range_maps_s[1:10,"geom"])
occurences_n <- occurences |>
        st_make_valid() |>
        st_crop(box_n) |>
        suppressWarnings()

# occurences_s <- occurences[occurences$LATITUDE <= SPLIT,]
# occurences_n <- occurences[occurences$LATITUDE > SPLIT,]

# Save occurences
st_write(occurences_s, "data_clean/emvs_dq_s.gpkg", layer = "emvs_dq_s", quiet = TRUE)
st_write(occurences_n, "data_clean/emvs_dq_n.gpkg", layer = "emvs_dq_n", quiet = TRUE)


##############################################################################
# Protected areas
#
# Split protected areas into two regions
# SPLIT = 50 is the latitude where the range maps are split into two regions
# (S and N).
##############################################################################
aires_prot <- suppressWarnings(st_read("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S", quiet = TRUE))
# Prep protected areas
aires_s <- aires_prot |>
        st_crop(box_s) |>
        suppressWarnings()
# mapview::mapview(aires_s |> st_simplify())
aires_n <- aires_prot |>
        st_crop(box_n) |>
        suppressWarnings()
# mapview::mapview(aires_n)
st_write(aires_s, "data_clean/aires_protegees_sud.gpkg", append = FALSE, quiet = TRUE)
st_write(aires_n, "data_clean/aires_protegees_nord.gpkg", append = FALSE, quiet = TRUE)