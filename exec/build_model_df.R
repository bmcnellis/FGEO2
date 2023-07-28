# BEM July 2023
library(terra)

data_dir <- '../data'

# get demography data
LAU <- read.csv(file.path(data_dir, 'LAU_processed.csv'))
# need to only use mainstems
LAU <- LAU[which(LAU$mstem == 0), ]
LAU <- LAU[which(LAU$CensusID == 5), ]
LAU <- LAU[, c('DFstatus', 'slp', 'sp', 'x', 'y', 'treeID', 'dbh')]
#LAU$ExactDate <- sapply(strsplit(LAU$ExactDate, ' '), \(xx) xx[1])
#LAU$ExactDate <- strptime(LAU$ExactDate, format = '%m/%d/%y')

# get DEM covariates
LAU_TRI <- terra::rast(file.path(data_dir, 'LAU_TRI.tif'))
LAU_TWI <- terra::rast(file.path(data_dir, 'LAU_TWI.tif'))
LAU_rad <- terra::rast(file.path(data_dir, 'LAU_rad.tif'))
LAU_wind <- terra::rast(file.path(data_dir, 'LAU_wind.tif'))
LAU_VDCN <- terra::rast(file.path(data_dir, 'VDCN.tif'))

# get & aggregate climate data
ppt_df <- data_dir |>
  file.path('PPT_Laupahoehoe.csv') |>
  read.csv(row.names = NULL) |>
  (\(.) .[which(.$year %in% c(2012:(2012-4))), ])() |>
  (\(.) mean(.[, 2]))() |>
  merge(LAU$treeID) |>
  (\(.) .[!duplicated(.), ])() |>
  setNames(c('ppt', 'treeID'))

# combine
LAU$TRI <- terra::extract(LAU_TRI, LAU[, c('x', 'y')])[, 2]
LAU$TWI <- terra::extract(LAU_TWI, LAU[, c('x', 'y')])[, 2]
LAU$rad <- terra::extract(LAU_rad, LAU[, c('x', 'y')])[, 2]
LAU$wind <- terra::extract(LAU_wind, LAU[, c('x', 'y')])[, 2]
LAU$VDCN <- terra::extract(LAU_VDCN, LAU[, c('x', 'y')])[, 2]

LAU <- dplyr::left_join(LAU, ppt_df)

write.csv(LAU, file.path(data_dir, 'LAU_df_for_model.csv'), row.names = F)

