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

# SPI_list <- lapply(files, read.csv)

# Combine all files into one dataframe
# SPI <- do.call(rbind, SPI_list)
SPI <- do.call(rbind, lapply(SPI_list, function(x) SPI_list[match(names(l[[1]]), names(x))]))


# Save SPI to results directory
saveRDS(SPI, "results/SPI.rds")
write.csv(SPI, "results/SPI.csv")