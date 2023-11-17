library(sf)
library(dplyr)
library(geos)

print("Union in progress...")

    # If the union of protected areas does not exist then create it
    aires_prot <- read_sf("data_raw/registre_aires_prot.gpkg", layer = "AP_REG_S")
    aires_prot$annee_creation <- as.numeric(substr(aires_prot$DA_CREATIO, start = 1, stop = 4))
    years <- unique(unique(aires_prot$annee_creation)) |> sort()
    #years <- years[years >= YEAR_RANGE[1] & years <= YEAR_RANGE[2]] # select years
    for (i in years) {
        aires_prot_year <- aires_prot[aires_prot$annee_creation <= i, ]
        aires_prot_union <- aires_prot_year |>
            st_union() |> # st_cast("POLYGON")
            st_as_sf()

        if(i == years[1]) {
            x <- aires_prot_union
        } else {
            x <- rbind(x, aires_prot_union)
        }
    }
    x$year <- years
    st_write(x, paste0("data_clean/aires_union.gpkg"), driver = "GPKG")

