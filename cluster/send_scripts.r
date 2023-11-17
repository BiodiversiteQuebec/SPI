###########
# Save scripts in Beluga
# Victor Cameron
# November 16, 2023
##########

# Files in cluster folder
dirCluster <- paste(paste0('cluster/', dir('cluster/')), collapse=' ')
system(paste0('scp ', dirCluster, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/cluster'))

# Files in data folder
dirData <- paste(grep('.', paste0('data/', dir('data/')), value=TRUE, fixed=TRUE), collapse=' ')
system(paste0('scp ', dirData, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/data'))

# Files in data/range_maps folder
dirDataRaw <- paste(grep('.', paste0('data_raw/', dir('data_raw/')), value=TRUE, fixed=TRUE), collapse=' ')
system(paste0('scp ', dirDataRaw, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/data_raw'))

# Files in data/range_maps folder
dirDataClean <- paste(grep('.', paste0('data_clean/', dir('data_clean/')), value=TRUE, fixed=TRUE), collapse=' ')
system(paste0('scp ', dirDataClean, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/data_clean'))

# File in scr folder
dirScr <- paste(paste0('scr/', dir('scr/')), collapse=' ')
system(paste0('scp ', dirScr, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/scr'))
