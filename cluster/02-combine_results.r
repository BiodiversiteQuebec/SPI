# Combine all results into one dataframe

# Get all file names from /result folder
files <- list.files("results", pattern = "*.csv", full.names = TRUE)
files <- files[files != "results/SPI.csv"]
files <- files[files != "results/SPI_OCC.csv"]
files <- files[files != "results/SPI_ranges.csv"]
sp_names <- sub(".*/(.*)(\\_SPI.csv)", "\\1", files)
sp_names <- sub("_", " ", sp_names)
# First letter to uppercase
sp_names <- sub("^(.)", "\\U\\1", sp_names, perl = TRUE)


# Read all files into a list
SPI_list <- lapply(files, function(x) {
    dat <- read.csv(x)
    vect <- as.numeric(dat[,2])
    names(vect) <- as.character(dat[,1])
    return(vect)
    }
)

# Combine all files into one dataframe
# SPI <- do.call(rbind, SPI_list)
SPI <- do.call(rbind, lapply(SPI_list, function(x) x[match(names(SPI_list[[1]]), names(x))])) |>
    as.data.frame()

# Set rownames
rownames(SPI) <- sp_names


# Save SPI to results directory
# saveRDS(SPI, "results/SPI_ranges.rds")
write.csv(SPI, "results/SPI_ranges.csv", row.names = TRUE)
