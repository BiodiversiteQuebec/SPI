###########
# Save scripts in Beluga
# Victor Cameron
# September 18, 2023
##########

# Files in cluster folder
dirCluster <- paste(paste0('cluster/', dir('cluster/')), collapse=' ')
system(paste0('scp ', dirCluster, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/cluster'))

# Files in data folder
dirData <- paste(grep('.', paste0('data/', dir('data/')), value=TRUE, fixed=TRUE), collapse=' ')
system(paste0('scp ', dirData, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/data'))

# Files in data/range_maps folder
dirData <- paste(grep('.', paste0('data/range_maps/', dir('data/range_maps/')), value=TRUE, fixed=TRUE), collapse=' ')
system(paste0('scp ', dirData, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/data/range_maps'))

# File in scr folder
dirScr <- paste(paste0('scr/', dir('scr/')), collapse=' ')
system(paste0('scp ', dirScr, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI/scr'))
