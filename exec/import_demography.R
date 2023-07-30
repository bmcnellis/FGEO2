# BEM JULY 2023

# Purpose: Initial import script for demography data from Becky's census 1-6 data

data_dir <- ''

# QA/QC
LAU <- read.csv("../data/Census 1-6 CTFS formatted data/data/Laupahoehoe_master.csv")
# total lines 124278
# drop extra columns
LAU <- LAU[, c(1:33)]
colnames(LAU) <- c(colnames(LAU)[c(1:23)], 'SPACE', colnames(LAU)[24:(length(colnames(LAU)) - 1)])
# force coercion
LAU$treeID <- as.numeric(LAU$treeID)
LAU <- LAU[!is.na(LAU$treeID), ]
# 124246 stems, less than 1% drop
# drop weird columns
LAU <- LAU[, -c(1, 2, 11, 14, 17, 18, 24)]
# some columns are empty
LAU <- LAU[, -which(colnames(LAU) %in% c('StemTag', 'agb', 'codes'))]
# drop records which dont have a species
LAU <- LAU[!is.na(LAU$sp), ]
# drop records which dont have a status
LAU <- LAU[!is.na(LAU$status), ]
# drop records where the species is wrong
LAU <- LAU[which(LAU$sp %in% FGEO1::Laupahoehoe_sp_list), ]
# change mstem to ordered
LAU$mstem <- as.numeric(LAU$mstem)

# "prior" records dont need to exist
LAU <- LAU[-which(LAU$DFstatus == 'prior'), ]
LAU[which(LAU$DFstatus %in% c('gone', 'missing')), 'DFstatus'] <- 'dead'
sp_tbl <- table(LAU$sp[which(LAU$CensusID == 1)], LAU$DFstatus[which(LAU$CensusID == 1)])
# lump all Cibotium together
LAU$sp <- ifelse(LAU$sp %in% c('CIBCHA', 'CIBGLA', 'CIBMEN'), 'CIBSPP', LAU$sp)
# lump all uncommon understory shrubs/small trees together
# cutoff is to make sure ACAKOA is represented separately, all with less get lumped in
LAU$sp <- ifelse(LAU$sp %in% c('ANTPLA', 'CLEPAR', 'HEDHIL', 'LEPTAM', 'MELCLU', 'MYRSAN', 'PERSAN', 'PIPALB', 'PSYHAW', 'TREGRA'), 'UNCUND', LAU$sp)
#sp_tbl <- as.data.frame(sp_tbl)
#sp_tbl <- aggregate(sp_tbl$Freq, by = list(sp_tbl$Var1), FUN = sum)
#sp_tbl <- sp_tbl[which(sp_tbl$x > 300), ] # cutoff of 200 total stems
#LAU <- LAU[which(LAU$sp %in% sp_tbl$Group.1), ]

# create growth
# fix date/create year differential variable
LAU$ExactDate <- sapply(strsplit(LAU$ExactDate, ' '), \(xx) xx[1])
LAU$ExactDate <- strptime(LAU$ExactDate, format = '%m/%d/%y')
LAU$ExactDate <- as.POSIXct(LAU$ExactDate)
LAU$diff_days <- difftime(LAU$ExactDate, min(LAU$ExactDate, na.rm = T), units = 'days')
LAU$diff_years <- round(LAU$diff_days / 365, 2)
# drop multiple stems
LAU <- LAU[which(LAU$mstem == 0), ]
# coerce dbh
LAU$dbh <- as.numeric(LAU$dbh)
# only use census 1 and 5
LAU <- LAU[which(LAU$CensusID %in% c(1, 5)), ]

LAU_dbh <- LAU
#LAU_dbh$dDBH <- rep(NA, nrow(LAU_dbh))
#LAU_dbh$dDBH_days <- rep(NA, nrow(LAU_dbh))

# create growth increment
for (i in seq_along(unique(LAU$CensusID))) {
  if (i == 1) next

  i0 <- unique(LAU$CensusID)[i - 1]
  ii <- unique(LAU$CensusID)[i]

  idf_1 <- LAU[which(LAU$CensusID == i0), ]
  idf_1 <- idf_1[!is.na(idf_1$dbh), ]
  idf_1 <- idf_1[, c('treeID', 'dbh', 'diff_days')]
  idf_2 <- LAU[which(LAU$CensusID == ii), ]
  idf_2 <- idf_2[!is.na(idf_2$dbh), ]
  idf_2 <- idf_2[, c('CensusID', 'treeID', 'dbh', 'diff_days')]
  colnames(idf_2)[c(3, 4)] <- c('dbh_2', 'diff_2')

  idf0 <- dplyr::left_join(idf_2, idf_1)
  idf0$dDBH_days <- as.numeric(idf0$diff_2 - idf0$diff_days)
  idf0$dDBH <- (idf0$dbh_2 - idf0$dbh) / (idf0$dDBH_days / 365)

  #if (i == 3) break
  idf0 <- idf0[, c('CensusID', 'treeID', 'dDBH', 'dDBH_days')]

  # join the new data back to the original dataframe by CensusID and treeID
  LAU_dbh <- dplyr::left_join(LAU_dbh, idf0, by = c('CensusID', 'treeID'))
  #LAU_dbh <- dplyr::full_join(LAU_dbh, idf0, by = c('CensusID', 'treeID'))

}

# fix growth increment
# remove growth increment of 10+
LAU_dbh <- LAU_dbh[-which(LAU_dbh$dDBH > 10), ]
# using dtruncnorm strategy
LAU_dbh$dDBH_adj <- FGEO2::fix_DBH(LAU_dbh$dDBH, LAU_dbh$dbh, 'median')

# add height
LAU_dbh$height <- FGEO2::dbh_to_height(LAU_dbh$sp, LAU_dbh$dbh)
# fix CIB* height using dtruncnorm strategy
max_CIB_height <- 7
LAU_dbh$height[which(LAU_dbh$sp == 'CIBSPP')] <- FGEO2::fix_height(LAU_dbh$height[which(LAU_dbh$sp == 'CIBSPP')], max_CIB_height)

# get competition index
# coerce x and y
LAU_dbh$gx <- as.numeric(LAU_dbh$gx)
LAU_dbh$gy <- as.numeric(LAU_dbh$gy)
# needs adjusting
comp <- FGEO2::calc_light_index(LAU_dbh$gx, LAU_dbh$gy, LAU_dbh$height, 5)

write.csv(LAU_dbh, '../data/LAU_processed.csv', row.names = F)
