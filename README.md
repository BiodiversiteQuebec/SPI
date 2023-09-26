# Computation of the SPI indicator for the 1992-2017 period

The SPI biodiversity indicator measures the protected species habitat. It is computed as the ratio of the protected species habitat area to the total area of the species habitat. 


## Run computations

Computations may be ran from a personal computer with the `01-run_spi_computation.r` script in the `scr` folder. 

Alternatively, computations may be ran using clusters with the `start.sh` script in the `cluster` folder.


## Access results

SPI values per species are saved in dataframe `results/SPI.csv`.

Results are also saved in csv files per species in the `results` folder. They may be assembled as a single dataframe using the `cluster/02-combine_results.r`.

```r
# Combine results
source("cluster/02-combine_results.r")
```

The dataframe will be saved as `results/SPI.csv`.

```r
# View results
source("scr/utils-visualisation.r")
(SPI <- read.csv("results/SPI.csv"))

plot_SPI_time_series() # Time series of SPI values by species
plot_SPI_scores(year = 2000) # Histogram of SPI scores for a given year
plot_SPI_by_group() # Time series of SPI values devided by species groups
plot_SPI_at_risk() # Time series of SPI values for species at risk
```

## Data

All data needed for the analyses are available in the `data` folder.

As an initial proof of concept, the indicator is computed for the 1992-2018 period from whole range maps. These range maps were produced by Vencent Bellavance from occurence data from [Biodiversité Québec](https://biodiversite-quebec.ca/). The range maps are available in the `data` folder where each `.tif` file is named after a species and contains 26 layers, one for every year between 1992 and 2018. 

The protected areas are from the données ouvertes portal of the gouvernement du Québec and are also available in the `data` folder. Only the 'Aires protégées du registre' layer was used and **no distinction is made between the different types of protected areas** . The dataset was downloaded on 31-08-2023.

### Data treatment

Protected areas dataset contains multiple types of protected areas that may overlap. Overlap would duplicate the protected area and bias the SPI computation. To remove overlap between the different protected areas, the `geos_unary_union` function from the `geos` package was used to union overlapping polygons. The resulting layer is available in the `data` folder as `aires_union.gpkg`.


## TODO

- [ ] Select types of protected areas to include in the analyses
