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


# rbind all files
SPI <- read.csv(files[1])
for (file in files[-1]){
    dat <- read.csv(file)
    SPI <- rbind(SPI, dat)
}
SPI <- res[,-1]


# Save SPI to results directory
# saveRDS(SPI, "results/SPI_ranges.rds")
write.csv(SPI, "results/SPI_ranges.csv", row.names = FALSE)

