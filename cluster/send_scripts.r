###########
# Save scripts in Beluga
# Victor Cameron
# November 16, 2023
##########

# Files in cluster folder
dirCluster <- paste(paste0('cluster/', dir('cluster/')), collapse=' ')
system(paste0('scp ', dirCluster, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI_OCC/cluster'))

# Files in data_raw/range_maps folder
# dirDataRaw <- paste(grep('.', paste0('data_raw/', dir('data_raw/')), value=TRUE, fixed=TRUE), collapse=' ')
dirDataRaw <- paste(c("data_raw/emvs_dq.gpkg"), collapse=' ')
system(paste0('scp ', dirDataRaw, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI_OCC/data_raw'))

# Files in data/range_maps folder
# dirDataClean <- paste(grep('.', paste0('data_clean/', dir('data_clean/')), value=TRUE, fixed=TRUE), collapse=' ')
dirDataClean <- paste(c("data_clean/aires_union.gpkg", "data_clean/aires_protegees_nord.gpkg", "data_clean/aires_protegees_sud.gpkg", "data_clean/emvs_dq_s.gpkg", "data_clean/emvs_dq_n.gpkg"), collapse=' ')
system(paste0('scp ', dirDataClean, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI_OCC/data_clean'))

# File in scr folder
dirScr <- paste(paste0('scr/', dir('scr/')), collapse=' ')
system(paste0('scp ', dirScr, ' vcameron@beluga.alliancecan.ca:projects/def-dgravel/vcameron/SPI_OCC/scr'))
