# Combine all results into one dataframe

# Get all file names from /result folder
files <- list.files("results", pattern = "*.csv", full.names = TRUE)

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


# Save SPI to results directory
saveRDS(SPI, "results/SPI.rds")
write.csv(SPI, "results/SPI.csv")
