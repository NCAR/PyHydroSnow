###################################################
##             MODEL OUTPUT READS                ##
###################################################

library(plyr)
library(data.table)

# Multi-core
parallelFlag <- FALSE
if (ncores>1) {
        library(doParallel)
        cl <- makeForkCluster(ncores)
        registerDoParallel(cl)
        parallelFlag <- TRUE
}

################## General Setup ##################

# Setup temp file
saveList <- c()
tmpRimg <- tempfile(fileext=".Rdata", tmpdir=tmpDir)
message(paste0("Temp output file:", tmpRimg))

# Setup lookups
if (exists("stid2gageList")) {
	stid2gage <- data.frame(st_id=names(stid2gageList), STAID=unlist(stid2gageList), stringsAsFactors=FALSE)
	stid2gage$st_id <- as.integer(stid2gage$st_id)
}

# Get needed geo info
ncid <- ncdf4::nc_open(geoFile)
geoDX <- ncdf4::ncatt_get(ncid,varid=0,'DX')$value
ncdf4::nc_close(ncid)
hydDX <- geoDX/aggfact

if (readEnsemble) {
	numEns <- length(ensembleList)
	# Expand modPathList to number of ensembles
	modPathList <- expandModPathList(numEns,ensembleList,length(modPathList),modPathList)
	modTagList <- expandModTagList(numEns,length(modPathList),modTagList)
	ensTagList <- expandEnsTagList(numEns,length(modPathList),modPathList,ensembleList)
} else {
	numEns <- 1
}

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# INDEX PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if ((readMod & readBasinLdasout) | (readForc & readBasinLdasin)) {
 	# Basin means
 	basgeoIndex_Lev0 <- list()
 	basgeoIndex_Lev1 <- list()
 	basgeoIndex_Lev2 <- list()
 	basgeoIndex_Lev3 <- list()
 	basgeoIndex_Lev4 <- list()
        basgeoIndex_sweSum <- list()
 	for (i in 1:length(mskgeo.nameList)) {
   		basgeoIndex_Lev0[[as.character(mskgeo.nameList[[i]])]] <- list(start=c(mskgeo.minInds$x[i], mskgeo.minInds$y[i], 1),
                                               end=c(mskgeo.maxInds$x[i], mskgeo.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskgeo.List[[i]]))
   		basgeoIndex_Lev1[[as.character(mskgeo.nameList[[i]])]] <- list(start=c(mskgeo.minInds$x[i], 1, mskgeo.minInds$y[i], 1),
                                               end=c(mskgeo.maxInds$x[i], 1, mskgeo.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskgeo.List[[i]]))
   		basgeoIndex_Lev2[[as.character(mskgeo.nameList[[i]])]] <- list(start=c(mskgeo.minInds$x[i], 2, mskgeo.minInds$y[i], 1),
                                               end=c(mskgeo.maxInds$x[i], 2, mskgeo.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskgeo.List[[i]]))
   		basgeoIndex_Lev3[[as.character(mskgeo.nameList[[i]])]] <- list(start=c(mskgeo.minInds$x[i], 3, mskgeo.minInds$y[i], 1),
                                               end=c(mskgeo.maxInds$x[i], 3, mskgeo.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskgeo.List[[i]]))
   		basgeoIndex_Lev4[[as.character(mskgeo.nameList[[i]])]] <- list(start=c(mskgeo.minInds$x[i], 4, mskgeo.minInds$y[i], 1),
                                               end=c(mskgeo.maxInds$x[i], 4, mskgeo.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskgeo.List[[i]]))
 		basgeoIndex_sweSum[[as.character(mskgeo.nameList[[i]])]] <- list(start=c(mskgeo.minInds$x[i], mskgeo.minInds$y[i], 1),
                                               end=c(mskgeo.maxInds$x[i], mskgeo.maxInds$y[i], 1), stat='basin_sum_swe', arg=list(mskvar=mskgeo.List[[i]],res=resMod))
		}
 	}

if (readMod & readBasinRtout) {
         # Basin means
         bashydIndex_Lev0 <- list()
         bashydIndex_Lev1 <- list()
         bashydIndex_Lev2 <- list()
         bashydIndex_Lev3 <- list()
         bashydIndex_Lev4 <- list()
         for (i in 1:length(mskhyd.nameList)) {
                 bashydIndex_Lev0[[as.character(mskhyd.nameList[[i]])]] <- list(start=c(mskhyd.minInds$x[i], mskhyd.minInds$y[i], 1),
                                               end=c(mskhyd.maxInds$x[i], mskhyd.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskhyd.List[[i]]))
                 bashydIndex_Lev1[[as.character(mskhyd.nameList[[i]])]] <- list(start=c(mskhyd.minInds$x[i], 1, mskhyd.minInds$y[i], 1),
                                               end=c(mskhyd.maxInds$x[i], 1, mskhyd.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskhyd.List[[i]]))
                 bashydIndex_Lev2[[as.character(mskhyd.nameList[[i]])]] <- list(start=c(mskhyd.minInds$x[i], 2, mskhyd.minInds$y[i], 1),
                                               end=c(mskhyd.maxInds$x[i], 2, mskhyd.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskhyd.List[[i]]))
                 bashydIndex_Lev3[[as.character(mskhyd.nameList[[i]])]] <- list(start=c(mskhyd.minInds$x[i], 3, mskhyd.minInds$y[i], 1),
                                               end=c(mskhyd.maxInds$x[i], 3, mskhyd.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskhyd.List[[i]]))
                 bashydIndex_Lev4[[as.character(mskhyd.nameList[[i]])]] <- list(start=c(mskhyd.minInds$x[i], 4, mskhyd.minInds$y[i], 1),
                                               end=c(mskhyd.maxInds$x[i], 4, mskhyd.maxInds$y[i], 1), stat='basin_avg', arg=list(mskvar=mskhyd.List[[i]]))
                 }
         }

if ((readMod & readAmfLdasout) | (readForc & readAmfLdasin)) {
        # Ameriflux stations
        amfIndex_Lev0 <- list()
        amfIndex_Lev1 <- list()
        amfIndex_Lev2 <- list()
        amfIndex_Lev3 <- list()
        amfIndex_Lev4 <- list()
        amfIndex_sweSum <- list()
        # Ameriflux sites
        for (i in 1:length(ptgeo.amf$id)) {
                if (!is.na(ptgeo.amf$we[i]) & !is.na(ptgeo.amf$sn[i])) {
                        amfIndex_Lev0[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], ptgeo.amf$sn[i], 1),
                                               end=c(ptgeo.amf$we[i], ptgeo.amf$sn[i], 1), stat="mean")
                        amfIndex_Lev1[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], 1, ptgeo.amf$sn[i], 1),
                                          end=c(ptgeo.amf$we[i], 1, ptgeo.amf$sn[i], 1), stat="mean")
                        amfIndex_Lev2[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], 2, ptgeo.amf$sn[i], 1),
                                               end=c(ptgeo.amf$we[i], 2, ptgeo.amf$sn[i], 1), stat="mean")
                        amfIndex_Lev3[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], 3, ptgeo.amf$sn[i], 1),
                                               end=c(ptgeo.amf$we[i], 3, ptgeo.amf$sn[i], 1), stat="mean")
                        amfIndex_Lev4[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], 4, ptgeo.amf$sn[i], 1),
                                               end=c(ptgeo.amf$we[i], 4, ptgeo.amf$sn[i], 1), stat="mean")
			amfIndex_sweSum[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], ptgeo.amf$sn[i], 1),
                                               end=c(ptgeo.amf$we[i], ptgeo.amf$sn[i], 1), stat="mean")
                        }
                }
        }

if ((readMod & readSnoLdasout) | (readForc & readSnoLdasin)) {
 	# SNOTEL points
 	snoIndex_Lev0 <- list()
 	snoIndex_Lev1 <- list()
 	snoIndex_Lev2 <- list()
 	snoIndex_Lev3 <- list()
 	snoIndex_Lev4 <- list()
	snoIndex_sweSum <- list()
 	for (i in 1:length(ptgeo.sno$id)) {
   		if (!is.na(ptgeo.sno$we[i]) & !is.na(ptgeo.sno$sn[i])) {
   			snoIndex_Lev0[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1),
                                               end=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1), stat="mean")
   			snoIndex_Lev1[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], 1, ptgeo.sno$sn[i], 1),
                                          end=c(ptgeo.sno$we[i], 1, ptgeo.sno$sn[i], 1), stat="mean")
   			snoIndex_Lev2[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], 2, ptgeo.sno$sn[i], 1),
                                               end=c(ptgeo.sno$we[i], 2, ptgeo.sno$sn[i], 1), stat="mean")
   			snoIndex_Lev3[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], 3, ptgeo.sno$sn[i], 1),
                                               end=c(ptgeo.sno$we[i], 3, ptgeo.sno$sn[i], 1), stat="mean")
   			snoIndex_Lev4[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], 4, ptgeo.sno$sn[i], 1),
                                               end=c(ptgeo.sno$we[i], 4, ptgeo.sno$sn[i], 1), stat="mean")
			snoIndex_sweSum[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1),
                                               end=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1), stat="mean")
   			}
 		}
	}

if ((readMod & readMetLdasout) | (readForc & readMetLdasin)) {
	# Other met stations
        metIndex_Lev0 <- list()
        metIndex_Lev1 <- list()
        metIndex_Lev2 <- list()
        metIndex_Lev3 <- list()
        metIndex_Lev4 <- list()
	metIndex_sweSum <- list()
        for (i in 1:length(ptgeo.met$id)) {
                if (!is.na(ptgeo.met$we[i]) & !is.na(ptgeo.met$sn[i])) {
                        metIndex_Lev0[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], ptgeo.met$sn[i], 1),
                                               end=c(ptgeo.met$we[i], ptgeo.met$sn[i], 1), stat="mean")
                        metIndex_Lev1[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], 1, ptgeo.met$sn[i], 1),
                                          end=c(ptgeo.met$we[i], 1, ptgeo.met$sn[i], 1), stat="mean")
                        metIndex_Lev2[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], 2, ptgeo.met$sn[i], 1),
                                               end=c(ptgeo.met$we[i], 2, ptgeo.met$sn[i], 1), stat="mean")
                        metIndex_Lev3[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], 3, ptgeo.met$sn[i], 1),
                                               end=c(ptgeo.met$we[i], 3, ptgeo.met$sn[i], 1), stat="mean")
                        metIndex_Lev4[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], 4, ptgeo.met$sn[i], 1),
                                               end=c(ptgeo.met$we[i], 4, ptgeo.met$sn[i], 1), stat="mean")
			metIndex_sweSum[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], ptgeo.met$sn[i], 1),
                                               end=c(ptgeo.met$we[i], ptgeo.met$sn[i], 1), stat="mean")
                        }
                }
	}
	
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# RTOUT PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readMod & readBasinRtout) {
 
         ## ------------------------------------------------------------------------
         # Setup variables
 
         varNames <- c('QSTRMVOLRT','SFCHEADSUBRT','QBDRYRT')
         rtoutVars <- as.list( varNames )
         names(rtoutVars) <- varNames
         rtoutVariableList <- list( rtout = rtoutVars )
 
         ## ------------------------------------------------------------------------
         # Setup indexes
 
         level0 <- bashydIndex_Lev0
         rtoutInd <- list( level0, level0, level0 )
         names(rtoutInd) <- names(rtoutVars)
         rtoutIndexList <- list( rtout = rtoutInd )
 
         ## ------------------------------------------------------------------------
         # Get data and flatten files

	 ## Loop through model run output directories
	 modRtout_BAS_tmp <- data.frame()
	 for (i in 1:length(modPathList)) {
        	modoutPath <- modPathList[i]
        	modoutTag <- modTagList[i]
         	# Setup RTOUT files
         	filesList <- list.files(path=modoutPath, pattern=glob2rx('*.RTOUT_DOMAIN*'), full.names=TRUE)
		if (!is.null(readModStart) | !is.null(readModEnd)) {		
			filesList <- subDates(filesList, readModStart, readModEnd, rt2dt)
		}
         	rtoutFilesList <- list( rtout = filesList)
         	# Run basin means
		rtoutDF <- GetMultiNcdf(indexList=rtoutIndexList,
                	         variableList=rtoutVariableList,
                        	 filesList=rtoutFilesList, parallel=parallelFlag )
         	modRtout <- ReshapeMultiNcdf(rtoutDF)
         	modRtout <- modRtout[order(modRtout$statArg, modRtout$POSIXct),]
	 	modRtout$tag <- modoutTag
		modRtout$ensTag <- ensoutTag
	 	modRtout_BAS_tmp <- plyr::rbind.fill(modRtout_BAS_tmp, modRtout)
         	rm(rtoutDF, modRtout, filesList, rtoutFilesList)
 	 	gc()
	 }
	 rm(varNames, rtoutVars, rtoutVariableList, level0, rtoutInd, rtoutIndexList)
	 saveList <- c(saveList, "modRtout_BAS_tmp")
 	 save(list=saveList, file=tmpRimg)

 } # end rtout processing


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# LDASOUT PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readMod & (readBasinLdasout | readAmfLdasout | readSnoLdasout | readMetLdasout)) {
 
 	## ------------------------------------------------------------------------
 	# Setup variables

        if (varsLdasoutNFIE) {
                # SUBSET
                varNames <- c('SWFORC', 'LWFORC', 'ALBEDO', 'GRDFLX',
                        'LH', 'HFX', 'ACCETRAN', 'UGDRNOFF',
                        'SFCRNOFF', 'CANLIQ', 'CANICE', 'ACCPRCP',
			'ACCECAN', 'ACCEDIR', 'TRAD',
                         rep('SOIL_M',4),
                        'SNOWH', 'SNEQV','SNEQV')
                varLabels <- c('SWFORC', 'LWFORC', 'ALBEDO', 'GRDFLX',
                        'LH', 'HFX', 'ACCETRAN', 'UGDRNOFF',
                        'SFCRNOFF', 'CANLIQ', 'CANICE', 'ACCPRCP',
                        'ACCECAN', 'ACCEDIR', 'TRAD',
                         paste0('SOIL_M',1:4),
                        'SNOWH', 'SNEQV','SNEQV_SUM')
                ldasoutVars <- as.list( varNames )
                names(ldasoutVars) <- varLabels
                #ldasoutVariableList <- list( ldasout = ldasoutVars )
                # INDEXES
                genIndex_Ldasout <- function(pref, ldasoutVars.=ldasoutVars) {
                        level0 <- get(paste0(pref, "Index_Lev0"))
                        level1 <- get(paste0(pref, "Index_Lev1"))
                        level2 <- get(paste0(pref, "Index_Lev2"))
                        level3 <- get(paste0(pref, "Index_Lev3"))
                        level4 <- get(paste0(pref, "Index_Lev4"))
                        levelSum <- get(paste0(pref,"Index_sweSum"))
                        ldasoutInd <- list( level0, level0, level0, level0,
                                        level0, level0, level0, level0,
                                        level0, level0, level0, level0,
					level0, level0, level0,
                                        level1, level2, level3, level4,
                                        level0, level0, levelSum )
                        names(ldasoutInd) <- names(ldasoutVars.)
                        #ldasoutIndexList <- list( ldasout = ldasoutInd )
                        ldasoutInd
                }

        } else { 
 
 	if (varsLdasoutSUB) {
 		# SUBSET
 		varNames <- c('ALBEDO', 'GRDFLX', 'LH', 'HFX', 
 			'FIRA', 'FSA', 'TRAD', 'UGDRNOFF',
 			'SFCRNOFF','CANLIQ', 'CANICE', 'ACCPRCP',
			'ACCECAN', 'ACCEDIR', 'ACCETRAN', 'ACCET',
			'FVEG', 'LAI',
 			rep('SOIL_M',4),
 			rep('SOIL_T',4),
			'ACSNOM', 'ACSNOW', 'ISNOW',
			rep('SNLIQ', 3),
 			rep('SNOW_T', 3),
			rep('SOILICE', 4),
			'SOILSAT1', 'SOILSAT',
 			'FSNO', 'SNOWH', 'SNEQV','SNEQV')
 		varLabels <- c('ALBEDO', 'GRDFLX', 'LH', 'HFX',            
                        'FIRA', 'FSA', 'TRAD', 'UGDRNOFF',
                        'SFCRNOFF','CANLIQ', 'CANICE', 'ACCPRCP',
                        'ACCECAN', 'ACCEDIR', 'ACCETRAN', 'ACCET', 
			'FVEG', 'LAI',
                        paste0('SOIL_M',1:4),
                        paste0('SOIL_T',1:4),
                        'ACSNOM', 'ACSNOW', 'ISNOW',
			paste0('SNLIQ', 1:3), 
                        paste0('SNOW_T', 1:3),
			paste0('SOILICE', 1:4),
			'SOILSAT1', 'SOILSAT',
                        'FSNO', 'SNOWH', 'SNEQV','SNEQV_SUM')
 		ldasoutVars <- as.list( varNames )
 		names(ldasoutVars) <- varLabels
 		#ldasoutVariableList <- list( ldasout = ldasoutVars )
 		# INDEXES
		genIndex_Ldasout <- function(pref, ldasoutVars.=ldasoutVars) {
			level0 <- get(paste0(pref, "Index_Lev0"))
			level1 <- get(paste0(pref, "Index_Lev1"))
			level2 <- get(paste0(pref, "Index_Lev2"))
			level3 <- get(paste0(pref, "Index_Lev3"))
			level4 <- get(paste0(pref, "Index_Lev4"))
			levelSum <- get(paste0(pref,"Index_sweSum"))
			ldasoutInd <- list( level0, level0, level0, level0,
                                        level0, level0, level0, level0,
                                        level0, level0, level0, level0,
					level0, level0, level0, level0,
					level0, level0,
                                        level1, level2, level3, level4,
					level1, level2, level3, level4,                                        
					level0, level0, level0,
                                        level1, level2, level3,
					level1, level2, level3,
					level1, level2, level3, level4,
					level0, level0, 
                                        level0, level0, level0, levelSum )
                        names(ldasoutInd) <- names(ldasoutVars.)
                        #ldasoutIndexList <- list( ldasout = ldasoutInd )
			ldasoutInd
		}

	} else if (varsLdasoutSNOW) {
		varNames <- c('SNEQV','SNOWH','SNEQV','ACCPRCP')
		varLabels <- c('SNEQV','SNOWH','SNEQV_SUM','ACCPRCP')
		ldasoutVars <- as.list( varNames )
		names(ldasoutVars) <- varLabels
		genIndex_Ldasout <- function(pref, ldasoutVars.=ldasoutVars) {
                        level0 <- get(paste0(pref, "Index_Lev0"))
                        level1 <- get(paste0(pref, "Index_Lev1"))
                        level2 <- get(paste0(pref, "Index_Lev2"))
                        level3 <- get(paste0(pref, "Index_Lev3"))
                        level4 <- get(paste0(pref, "Index_Lev4"))
			levelSum <- get(paste0(pref,"Index_sweSum"))
                        ldasoutInd <- list( level0, level0, levelSum,level0)
		names(ldasoutInd) <- names(ldasoutVars.)
                #ldasoutIndexList <- list( ldasout = ldasoutInd )
                ldasoutInd
                }

        } else if (varsLdasoutIOC0) {
                # SUBSET
                varNames <- c('FSA', 'FIRA', 'GRDFLX', 'HFX',
                        'LH', 'UGDRNOFF', 'SFCRNOFF', 'ACCECAN',
                        'ACCEDIR','ACCETRAN', 'TRAD', 
			rep('SNLIQ', 3),
			rep('SOIL_T', 4),
			rep('SOIL_M', 4),
			'SNOWH', 'SNEQV', 'ISNOW', 'FSNO',
			'ACSNOM', 'ACCET', 'CANWAT', 'SOILICE',
			'SOILSAT_TOP', 'SOILSAT', 'SNOWT_AVG',
                        'SNEQV')
                varLabels <- c('FSA', 'FIRA', 'GRDFLX', 'HFX',
                        'LH', 'UGDRNOFF', 'SFCRNOFF', 'ACCECAN',
                        'ACCEDIR','ACCETRAN', 'TRAD', 
                        paste0('SNLIQ', 1:3),
                        paste0('SOIL_T', 1:4),
                        paste0('SOIL_M', 1:4),
                        'SNOWH', 'SNEQV', 'ISNOW', 'FSNO',
                        'ACSNOM', 'ACCET', 'CANWAT', 'SOILICE',
                        'SOILSAT_TOP', 'SOILSAT', 'SNOWT_AVG',
                        'SNEQV_SUM')
                ldasoutVars <- as.list( varNames )
                names(ldasoutVars) <- varLabels
                #ldasoutVariableList <- list( ldasout = ldasoutVars )
                # INDEXES
                genIndex_Ldasout <- function(pref, ldasoutVars.=ldasoutVars) {
                        level0 <- get(paste0(pref, "Index_Lev0"))
                        level1 <- get(paste0(pref, "Index_Lev1"))
                        level2 <- get(paste0(pref, "Index_Lev2"))
                        level3 <- get(paste0(pref, "Index_Lev3"))
                        level4 <- get(paste0(pref, "Index_Lev4"))
                        levelSum <- get(paste0(pref,"Index_sweSum"))
                        ldasoutInd <- list( level0, level0, level0, level0,
                                        level0, level0, level0, level0,
                                        level0, level0, level0,
                                        level1, level2, level3, 
                                        level1, level2, level3, level4,
                                        level1, level2, level3, level4,
					level0, level0, level0, level0,
					level0, level0, level0, level0,
					level0, level0, level0,
                                        levelSum)
                        names(ldasoutInd) <- names(ldasoutVars.)
                        #ldasoutIndexList <- list( ldasout = ldasoutInd )
                        ldasoutInd
		}

 	} else {
 		# ALL
 		varNames <- c('ACCECAN', 'ACCEDIR', 'ACCETRAN', 'ACCPRCP', 'ACSNOM', 'ACSNOW', 'ALBEDO', 'APAR', 'CANICE', 'CANLIQ',
                        'CH', 'CHB', 'CHB2', 'CHLEAF', 'CHUC', 'CHV', 'CHV2', 'CM', 'COSZ', 'EAH', 
                        'ECAN', 'EDIR', 'EMISS', 'ETRAN', 'EVB', 'EVC', 'EVG', 'FASTCP', 'FIRA', 'FSA', 
                        'FSNO', 'FVEG', 'FWET', 'GHB', 'GHV', 'GPP', 'GRDFLX', 'HFX', 'IRB', 'IRC', 
                        'IRG', 'ISLTYP', 'ISNOW', 'IVGTYP', 'LAI', 'LFMASS', 'LH', 'LWFORC', 'NEE', 'NPP', 
                        'PSN', 'Q2MB', 'Q2MV', 'QSNOW', 'RAINRATE', 'RTMASS', 'SAG', 'SAI', 'SAV', 'SFCRNOFF', 
                        'SHB', 'SHC', 'SHG', 'SNEQV',
                         rep('SNICE',3), 
                         rep('SNLIQ',3), 
                         'SNOWH', 
                         rep('SNOW_T',3),
                         rep('SOIL_M',4), 
                         rep('SOIL_T',4), 
                         rep('SOIL_W',4), 
                         'STBLCP', 'STMASS', 'SWFORC', 'T2MB', 'T2MV', 'TAH', 'TG', 'TGB', 'TGV', 'TR',
                         'TRAD', 'TV', 'UGDRNOFF', 'WA', 'WOOD', 'WT', 
                         rep('ZSNSO_SN',3), 
                         'ZWT', 'SNEQV')
 		varLabels <- c('ACCECAN', 'ACCEDIR', 'ACCETRAN', 'ACCPRCP', 'ACSNOM', 'ACSNOW', 'ALBEDO', 'APAR', 'CANICE', 'CANLIQ',
                         'CH', 'CHB', 'CHB2', 'CHLEAF', 'CHUC', 'CHV', 'CHV2', 'CM', 'COSZ', 'EAH', 
                         'ECAN', 'EDIR', 'EMISS', 'ETRAN', 'EVB', 'EVC', 'EVG', 'FASTCP', 'FIRA', 'FSA', 
                         'FSNO', 'FVEG', 'FWET', 'GHB', 'GHV', 'GPP', 'GRDFLX', 'HFX', 'IRB', 'IRC', 
                         'IRG', 'ISLTYP', 'ISNOW', 'IVGTYP', 'LAI', 'LFMASS', 'LH', 'LWFORC', 'NEE', 'NPP', 
                         'PSN', 'Q2MB', 'Q2MV', 'QSNOW', 'RAINRATE', 'RTMASS', 'SAG', 'SAI', 'SAV', 'SFCRNOFF', 
                         'SHB', 'SHC', 'SHG', 'SNEQV',
                         paste0('SNICE',1:3), 
                         paste0('SNLIQ',1:3), 
                         'SNOWH', 
                         paste0('SNOW_T',1:3), 
                         paste0('SOIL_M',1:4), 
                         paste0('SOIL_T',1:4), 
                         paste0('SOIL_W',1:4),
                         'STBLCP', 'STMASS', 'SWFORC', 'T2MB', 'T2MV', 'TAH', 'TG', 'TGB', 'TGV', 'TR',
                         'TRAD', 'TV', 'UGDRNOFF', 'WA', 'WOOD', 'WT',
                         paste0('ZSNSO_SN',1:3),
                         'ZWT', 'SNEQV_SUM')
         	ldasoutVars <- as.list( varNames )
         	names(ldasoutVars) <- varLabels
         	#ldasoutVariableList <- list( ldasout = ldasoutVars )
 		# INDEXES
	        genIndex_Ldasout <- function(pref, ldasoutVars.=ldasoutVars) {
                        level0 <- get(paste0(pref, "Index_Lev0"))
			level1 <- get(paste0(pref, "Index_Lev1"))
                        level2 <- get(paste0(pref, "Index_Lev2"))
                        level3 <- get(paste0(pref, "Index_Lev3"))
                        level4 <- get(paste0(pref, "Index_Lev4"))
                        levelSum <- get(paste0(pref,"Index_sweSum"))
                        ldasoutInd <- list( level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0,
                                     level1, level2, level3,
                                     level1, level2, level3,
                                     level0,
                                     level1, level2, level3,
                                     level1, level2, level3, level4,
                                     level1, level2, level3, level4,
                                     level1, level2, level3, level4,
                                     level0, level0, level0, level0, level0, level0, level0, level0, level0, level0,
                                     level0, level0, level0, level0, level0, level0,
                                     level1, level2, level3,
                                     level0, levelSum )
                        names(ldasoutInd) <- names(ldasoutVars.)
                        #ldasoutIndexList <- list( ldasout = ldasoutInd )
			ldasoutInd
		}

 	} # end ifelse vars subset
	} # end ifelse NFIE

        # Run through reads
        ldasoutIndexList <- list()
	ldasoutVariableList <- list()
        if (readBasinLdasout) {
                        ldasoutInd <- genIndex_Ldasout("basgeo")
                        ldasoutIndexList <- c(ldasoutIndexList, list( ldasout.basgeo = ldasoutInd ))
			ldasoutVariableList <- c(ldasoutVariableList, list( ldasout.basgeo = ldasoutVars ))
                        } # end runBasin
        if (readAmfLdasout) {
                        ldasoutInd <- genIndex_Ldasout("amf")
                        ldasoutIndexList <- c(ldasoutIndexList, list( ldasout.amf = ldasoutInd ))
			ldasoutVariableList <- c(ldasoutVariableList, list( ldasout.amf = ldasoutVars ))
                        } # end runAmf
        if (readSnoLdasout) {
                        ldasoutInd <- genIndex_Ldasout("sno")
                        ldasoutIndexList <- c(ldasoutIndexList, list( ldasout.sno = ldasoutInd ))
			ldasoutVariableList <- c(ldasoutVariableList, list( ldasout.sno = ldasoutVars ))
                        } # end runSno
        if (readMetLdasout) {
                        ldasoutInd <- genIndex_Ldasout("met")
                        ldasoutIndexList <- c(ldasoutIndexList, list( ldasout.met = ldasoutInd ))
			ldasoutVariableList <- c(ldasoutVariableList, list( ldasout.met = ldasoutVars ))
                        } # end runMet

 
 	## ------------------------------------------------------------------------
 	# Get data and flatten files
 	## ------------------------------------------------------------------------
        modLdasout_FINAL <- data.table()
	modLdasout.snoday_FINAL <- data.table()
	modLdasout.utcday_FINAL <- data.table()
	modLdasout.utcmonth_FINAL <- data.table()
        for (i in 1:length(modPathList)) {
        	modoutPath <- modPathList[i]
                modoutTag <- modTagList[i]
		if (numEns > 1) {
                        ensoutTag <- ensTagList[i]
                } else {
                        ensoutTag <- modoutTag
                }
                # Setup LDASOUT files
                filesList <- list.files(path=modoutPath, pattern=glob2rx('*.LDASOUT_DOMAIN*'), full.names=TRUE)
		if (!is.null(readModStart) | !is.null(readModEnd)) {
                        filesList <- subDates(filesList, readModStart, readModEnd, ldas2dt)
                }
                #filesList <- filesList[1:100]
		message(paste0("First: ", filesList[1], " Last: ", filesList[length(filesList)]))
		# Repeat vars and files for all of the index options
		indcnt <- length(names(ldasoutIndexList))
		ldasoutFilesList <- rep(list(filesList), indcnt)
		names(ldasoutFilesList) <- names(ldasoutIndexList)
		# Run basin means
                ldasoutDF <- GetMultiNcdf(indexList=ldasoutIndexList,
                                           variableList=ldasoutVariableList,
                                           filesList=ldasoutFilesList,
                                           parallel=parallelFlag )
                modLdasout_ALL <- ReshapeMultiNcdf(ldasoutDF)
		fileGroups <- unique(ldasoutDF$fileGroup)
		#if (!(is.list(modLdasout_ALL))) { modLdasout_ALL <- list(fileGroups = modLdasout_ALL) }
		modLdasoutList <- list()
		for (j in fileGroups) {
			if (length(fileGroups)==1) { modLdasout <- modLdasout_ALL } else { modLdasout <- modLdasout_ALL[[j]] }
                        modLdasout <- CalcNoahmpFluxes(modLdasout, "statArg")
			# Add derived variables
			if ( ("ACCEDIR" %in% names(modLdasout)) & ("ACCECAN" %in% names(modLdasout)) & ("ACCETRAN" %in% names(modLdasout)) ) {
				 modLdasout$ACCET <- with(modLdasout, ACCEDIR + ACCECAN + ACCETRAN) }
			if ( ("DEL_ACCEDIR" %in% names(modLdasout)) & ("DEL_ACCECAN" %in% names(modLdasout)) & ("DEL_ACCETRAN" %in% names(modLdasout)) ) {
				modLdasout$DEL_ET <- with(modLdasout, DEL_ACCEDIR + DEL_ACCECAN + DEL_ACCETRAN) }
			if ( ("FSA" %in% names(modLdasout)) & ("FIRA" %in% names(modLdasout)) ) {
				modLdasout$RadNet <- with(modLdasout, FSA-FIRA) }
			if ( ("LH" %in% names(modLdasout)) & ("HFX" %in% names(modLdasout)) ) {
				modLdasout$TurbNet <- with(modLdasout, LH+HFX) }
			# Data mgmt
                        modLdasout$tag <- modoutTag
			modLdasout$enstag <- ensoutTag
			modLdasout$fileGroup <- j
			modLdasout <- data.table(modLdasout)
			setkey(modLdasout, tag, enstag, fileGroup, statArg, POSIXct)
			message("LDASOUT: Starting daily aggregations")
                        ## ------------------------------------------------------------------------
                        # Calculate daily values
                        # Adjust to dates to match SNOTEL daily report.
                        # SNOTEL daily reports are derived from prior day's hourlies, and all 
                        # times are PST (no daylight savings).
                        # Adjust UTC to PST
                        modLdasout$PST_time <- modLdasout$POSIXct - 8*3600
                        # Calculate truncated date from PST time
                        modLdasout$PST_date <- CalcDateTrunc(modLdasout$PST_time)
                        # Shift by 1 day so aggregations match daily report
                        modLdasout$PST_dateP1 <- modLdasout$PST_date + 1
                        # Run daily aggs
			if (varsLdasoutIOC0) {
                        	modLdasout.snoday <- modLdasout[, list(SNEQV_last=tail(SNEQV,1), SNOWH_last=tail(SNOWH,1)), 
							by = "statArg,PST_dateP1"]
			} else if (varsLdasoutSNOW) {
				modLdasout.snoday <- modLdasout[, list(SNEQV_last=tail(SNEQV,1), SNOWH_last=tail(SNOWH,1)),
                                                        by = "statArg,PST_dateP1"]
			} else {
				modLdasout.snoday <- modLdasout[, list(DEL_ACCPRCP=sum(DEL_ACCPRCP), SNEQV_last=tail(SNEQV,1), SNOWH_last=tail(SNOWH,1)), 
								by = "statArg,PST_dateP1"]
                        }
			# Add dummy POSIXct for ease of plotting
                        modLdasout.snoday$POSIXct <- as.POSIXct(paste0(modLdasout.snoday$PST_dateP1, " 00:00"), tz="UTC")
                        # Calculate truncated date from UTC time
                        modLdasout$UTC_date <- CalcDateTrunc(modLdasout$POSIXct)
                        # Run daily aggs
			if (varsLdasoutNFIE) {
                        	modLdasout.utcday <- modLdasout[, list(DEL_ACCPRCP=sum(DEL_ACCPRCP), DEL_ACCEDIR=sum(DEL_ACCEDIR),
                                         DEL_ACCECAN=sum(DEL_ACCECAN), DEL_ACCETRAN=sum(DEL_ACCETRAN),
                                         SNEQV_mean=mean(SNEQV), SNOWH_mean=mean(SNOWH),
                                         SOIL_M1_mean=mean(SOIL_M1), SOIL_M2_mean=mean(SOIL_M2),
                                         LH_mean=mean(LH), HFX_mean=mean(HFX), GRDFLX_mean=mean(GRDFLX)),
                                         by = "statArg,UTC_date"]
                                mo <- as.integer(format(modLdasout.utcday$UTC_date, "%m"))
                                yr <- as.integer(format(modLdasout.utcday$UTC_date, "%Y"))
                                modLdasout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"), format="%Y-%m-%d")
                                modLdasout.utcmonth <- modLdasout.utcday[, list(DEL_ACCPRCP=sum(DEL_ACCPRCP), DEL_ACCEDIR=sum(DEL_ACCEDIR),
                                         DEL_ACCECAN=sum(DEL_ACCECAN), DEL_ACCETRAN=sum(DEL_ACCETRAN),
                                         SNEQV_mean=mean(SNEQV_mean), SNOWH_mean=mean(SNOWH_mean),
                                         SOIL_M1_mean=mean(SOIL_M1_mean), SOIL_M2_mean=mean(SOIL_M2_mean),
                                         LH_mean=mean(LH_mean), HFX_mean=mean(HFX_mean), GRDFLX_mean=mean(GRDFLX_mean)),
                                         by = "statArg,UTC_month"]
			} else if (varsLdasoutSUB) {
				modLdasout.utcday <- modLdasout[, list(DEL_ACCPRCP=sum(DEL_ACCPRCP), DEL_ACCEDIR=sum(DEL_ACCEDIR),
                                         DEL_ACCECAN=sum(DEL_ACCECAN), DEL_ACCETRAN=sum(DEL_ACCETRAN),
                                         SNEQV_mean=mean(SNEQV), SNOWH_mean=mean(SNOWH),
                                         SOIL_M1_mean=mean(SOIL_M1), SOIL_M2_mean=mean(SOIL_M2),
                                         SOIL_T1_mean=mean(SOIL_T1), SOIL_T2_mean=mean(SOIL_T2),
                                         LH_mean=mean(LH), HFX_mean=mean(HFX), GRDFLX_mean=mean(GRDFLX),
                                         FIRA_mean=mean(FIRA), FSA_mean=mean(FSA)),
					 by = "statArg,UTC_date"]
                                mo <- as.integer(format(modLdasout.utcday$UTC_date, "%m"))
                                yr <- as.integer(format(modLdasout.utcday$UTC_date, "%Y"))
                                modLdasout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"), format="%Y-%m-%d")
                                modLdasout.utcmonth <- modLdasout.utcday[, list(DEL_ACCPRCP=sum(DEL_ACCPRCP), DEL_ACCEDIR=sum(DEL_ACCEDIR),
                                         DEL_ACCECAN=sum(DEL_ACCECAN), DEL_ACCETRAN=sum(DEL_ACCETRAN),
                                         SNEQV_mean=mean(SNEQV_mean), SNOWH_mean=mean(SNOWH_mean),
                                         SOIL_M1_mean=mean(SOIL_M1_mean), SOIL_M2_mean=mean(SOIL_M2_mean),
                                         SOIL_T1_mean=mean(SOIL_T1_mean), SOIL_T2_mean=mean(SOIL_T2_mean),
                                         LH_mean=mean(LH_mean), HFX_mean=mean(HFX_mean), GRDFLX_mean=mean(GRDFLX_mean),
                                         FIRA_mean=mean(FIRA_mean), FSA_mean=mean(FSA_mean)),
                                         by = "statArg,UTC_month"]
			} else if (varsLdasoutSNOW) {
				modLdasout.utcday <- modLdasout[, list(SNEQV_mean=mean(SNEQV), SNOWH_mean=mean(SNOWH), 
								       ACCPRCP_mean=mean(ACCPRCP)),
					 by = "statArg,UTC_date"]
				mo <- as.integer(format(modLdasout.utcday$UTC_date, "%m"))
                                yr <- as.integer(format(modLdasout.utcday$UTC_date, "%Y"))
                                modLdasout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"), format="%Y-%m-%d")
                                modLdasout.utcmonth <- modLdasout.utcday[, list(SNEQV_mean=mean(SNEQV_mean), 
					SNOWH_mean=mean(SNOWH_mean),ACCPRCP_mean=mean(ACCPRCP_mean)),
					by = "statArg,UTC_month"]
			} else if (varsLdasoutIOC0) {
			        modLdasout.utcday <- modLdasout[, list(DEL_ACCEDIR=sum(DEL_ACCEDIR),
                                         DEL_ACCECAN=sum(DEL_ACCECAN), DEL_ACCETRAN=sum(DEL_ACCETRAN),
					 DEL_ET=sum(DEL_ET), ACCEDIR=tail(ACCEDIR, 1),
                                         ACCECAN=tail(ACCECAN, 1), ACCETRAN=tail(ACCETRAN, 1),
                                         ACCET=tail(ACCET, 1),
                                         SNEQV_mean=mean(SNEQV), SNOWH_mean=mean(SNOWH),
                                         SOIL_M1_mean=mean(SOIL_M1), SOIL_M2_mean=mean(SOIL_M2),
                                         SOIL_T1_mean=mean(SOIL_T1), SOIL_T2_mean=mean(SOIL_T2),
                                         LH_mean=mean(LH), HFX_mean=mean(HFX), GRDFLX_mean=mean(GRDFLX),
                                         FIRA_mean=mean(FIRA), FSA_mean=mean(FSA),
					 RadNet_mean=mean(RadNet), TurbNet_mean=mean(TurbNet)),
                                         by = "statArg,UTC_date"]
                        	mo <- as.integer(format(modLdasout.utcday$UTC_date, "%m"))
                        	yr <- as.integer(format(modLdasout.utcday$UTC_date, "%Y"))
                        	modLdasout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"), format="%Y-%m-%d")
                                modLdasout.utcmonth <- modLdasout.utcday[, list(DEL_ACCEDIR=sum(DEL_ACCEDIR),
                                         DEL_ACCECAN=sum(DEL_ACCECAN), DEL_ACCETRAN=sum(DEL_ACCETRAN),
                                         DEL_ET=sum(DEL_ET), ACCEDIR=tail(ACCEDIR, 1),
                                         ACCECAN=tail(ACCECAN, 1), ACCETRAN=tail(ACCETRAN, 1),
                                         ACCET=tail(ACCET, 1),
                                         SNEQV_mean=mean(SNEQV_mean), SNOWH_mean=mean(SNOWH_mean),
                                         SOIL_M1_mean=mean(SOIL_M1_mean), SOIL_M2_mean=mean(SOIL_M2_mean),
                                         SOIL_T1_mean=mean(SOIL_T1_mean), SOIL_T2_mean=mean(SOIL_T2_mean),
                                         LH_mean=mean(LH_mean), HFX_mean=mean(HFX_mean), GRDFLX_mean=mean(GRDFLX_mean),
                                         FIRA_mean=mean(FIRA_mean), FSA_mean=mean(FSA_mean),
					 RadNet_mean=mean(RadNet_mean), TurbNet_mean=mean(TurbNet_mean)),
                                         by = "statArg,UTC_month"]
			}
                        # Add dummy POSIXct for ease of plotting
                        modLdasout.utcday$POSIXct <- as.POSIXct(paste0(modLdasout.utcday$UTC_date, " 00:00"), tz="UTC")
			modLdasout.utcmonth$POSIXct <- as.POSIXct(paste0(modLdasout.utcmonth$UTC_month, " 00:00"), tz="UTC")
			# Add tags
			modLdasout.snoday$tag <- modoutTag
			modLdasout.utcday$tag <- modoutTag
			modLdasout.utcmonth$tag <- modoutTag
			modLdasout.snoday$enstag <- ensoutTag
			modLdasout.utcday$enstag <- ensoutTag
			modLdasout.utcmonth$enstag <- ensoutTag
			modLdasout.snoday$fileGroup <- j
			modLdasout.utcday$fileGroup <- j
			modLdasout.utcmonth$fileGroup <- j
			# Bind all
                	modLdasout_FINAL <- rbindlist(list(modLdasout_FINAL, modLdasout))
		        modLdasout.snoday_FINAL <- rbindlist(list(modLdasout.snoday_FINAL, modLdasout.snoday))
                        modLdasout.utcday_FINAL <- rbindlist(list(modLdasout.utcday_FINAL, modLdasout.utcday))
			modLdasout.utcmonth_FINAL <- rbindlist(list(modLdasout.utcmonth_FINAL, modLdasout.utcmonth))
			rm(modLdasout, modLdasout.snoday, modLdasout.utcday, modLdasout.utcmonth)
			gc()
                }
	}
	modLdasout_tmp <- list(modLdasout_FINAL, modLdasout.snoday_FINAL, modLdasout.utcday_FINAL, modLdasout.utcmonth_FINAL)
        names(modLdasout_tmp) <- c("native", "snoday", "utcday", "utcmonth")
	saveList <- c(saveList, "modLdasout_tmp")
	save(list=saveList, file=tmpRimg)

 } # end ldasout processing


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# GW PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readMod & readGwout) {

	## Loop through model run output directories
	modGwout_tmp <- data.frame()
	for (i in 1:length(modPathList)) {
        	modoutPath <- modPathList[i]
        	modoutTag <- modTagList[i]
		# Read GW
 		modGwout <- ReadGwOut(paste0(modoutPath, '/GW_outflow.txt'))
		# Filter out non-unique dates. Take values from latest run if dups.
 		modGwout <- modGwout[nrow(modGwout):1,]
 		modGwout$uni <- paste(modGwout$basin, modGwout$timest, sep=",")
 		modGwout <- modGwout[!(duplicated(modGwout$uni)),]
 		modGwout$uni <- NULL
 		modGwout <- modGwout[nrow(modGwout):1,]
		# Subset dates
		if (!is.null(readModStart) & !is.null(readModEnd)) {
			modGwout <- subset(modGwout, modGwout$POSIXct >= readModStart & modGwout$POSIXct <= readModEnd)
		}
		if (!is.null(readModStart) & is.null(readModEnd)) {
			modGwout <- subset(modGwout, modGwout$POSIXct >= readModStart)
		}
		if (is.null(readModStart) & !is.null(readModEnd)) {
			modGwout <- subset(modGwout, modGwout$POSIXct <= readModEnd)
		}
		# Tag and bind
		modGwout$tag <- modoutTag
                modGwout_tmp <- plyr::rbind.fill(modGwout_tmp, modGwout)
		rm(modGwout)
		gc()
	}
        saveList <- c(saveList, "modGwout_tmp")
        save(list=saveList, file=tmpRimg)

} # end gwout processing


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# FRXST PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readMod & readFrxstout) {

        ## Loop through model run output directories
        modFrxstout_tmp <- data.frame()
        for (i in 1:length(modPathList)) {
                modoutPath <- modPathList[i]
                modoutTag <- modTagList[i]
		if (numEns > 1) {
                        ensoutTag <- ensTagList[i]
                } else {
                        ensoutTag <- modoutTag 
                }
                # Read STR
		if (reachRting){ 
			modFrxstout <- ReadFrxstPts(paste0(modoutPath, '/frxst_pts_out.txt'))
		} else {
        		modFrxstout <- ReadFrxstPts(paste0(modoutPath, '/frxst_pts_out.txt'), stIdType='integer')
		}
        	# Filter out non-unique dates. Take values from latest run if dups.
        	modFrxstout <- modFrxstout[nrow(modFrxstout):1,]
        	modFrxstout$uni <- paste(modFrxstout$st_id, modFrxstout$timest, sep=",")
        	modFrxstout <- modFrxstout[!(duplicated(modFrxstout$uni)),]
        	modFrxstout$uni <- NULL
        	modFrxstout <- modFrxstout[nrow(modFrxstout):1,]
                modFrxstout <- modFrxstout[nrow(modFrxstout):1,]
                # Subset dates
                if (!is.null(readModStart) & !is.null(readModEnd)) {
                        modFrxstout <- subset(modFrxstout, modFrxstout$POSIXct >= readModStart & modFrxstout$POSIXct <= readModEnd)
                }
                if (!is.null(readModStart) & is.null(readModEnd)) {
                        modFrxstout <- subset(modFrxstout, modFrxstout$POSIXct >= readModStart)
                }
                if (is.null(readModStart) & !is.null(readModEnd)) {
                        modFrxstout <- subset(modFrxstout, modFrxstout$POSIXct <= readModEnd)
                }
		# Bring in basin IDs
		if (is.integer(modFrxstout$st_id[1])) {
  			modFrxstout <- plyr::join(modFrxstout, stid2gage, by="st_id")
		} else {
			modFrxstout$st_id <- stringr::str_trim(modFrxstout$st_id)
			modFrxstout$STAID <- modFrxstout$st_id
		}
		names(modFrxstout)[names(modFrxstout)=="STAID"] <- "site_no"
  		# Calculate accumulated flow
  		modFrxstout$q_mm <- NA
		modFrxstout$q_af <- NA
  		modFrxstout <- modFrxstout[order(modFrxstout$st_id, modFrxstout$POSIXct),]
  		modFrxstout$ACCFLOW <- NA
		modFrxstout$ACCFLOW_af <- NA
  		for (j in unique(modFrxstout$site_no)[!is.na(unique(modFrxstout$site_no))]) {
    			tmp <- subset(modFrxstout, modFrxstout$site_no==j)
			tmp$q_mm <- NA
			tmp$q_af <- NA
			for (k in 1:nrow(tmp)) {
				ts <- ifelse(k==1, as.integer(difftime(tmp$POSIXct[k+1],tmp$POSIXct[k], units="secs")), 
						as.integer(difftime(tmp$POSIXct[k],tmp$POSIXct[k-1], units="secs")))
                        	tmp$q_mm[k] <- tmp$q_cms[k]/
                                        (mskhyd.areaList[[j]]
                                        *hydDX*hydDX)*1000*ts
				tmp$q_af[k] <- (tmp$q_cms[k]*ts)/1233.48
			}
			modFrxstout$q_mm[modFrxstout$site_no==j & !is.na(modFrxstout$site_no)] <- tmp$q_mm
			modFrxstout$q_af[modFrxstout$site_no==j & !is.na(modFrxstout$site_no)] <- tmp$q_af
    			qaccum <- cumsum(tmp$q_mm)
			qaccum_af <- cumsum(tmp$q_af)
    			modFrxstout$ACCFLOW[modFrxstout$site_no==j & !is.na(modFrxstout$site_no)] <- qaccum
			modFrxstout$ACCFLOW_af[modFrxstout$site_no==j & !is.na(modFrxstout$site_no)] <- qaccum_af
  		}
		# Add model run tag and bind
                modFrxstout$tag <- modoutTag
		modFrxstout$enstag <- ensoutTag
                modFrxstout_tmp <- plyr::rbind.fill(modFrxstout_tmp, modFrxstout)
		# Remove NA values with subsetting
                modFrxstout_tmp <- subset(modFrxstout_tmp, !is.na(site_no))
                rm(modFrxstout)
                gc()
        }
        saveList <- c(saveList, "modFrxstout_tmp")
        save(list=saveList, file=tmpRimg)

} # end frxstout processing


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# CHRTOUT PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readMod & readChrtout) {

	# Call ReadChrtout to read in all streamflow data.
        modChrtout_tmp <- data.table()
	
	if (!is.null(readLink2gage)) {
        	idlist <- unique(readLink2gage$link)
        } else if (readChrtout_GAGES) {
                idlist <- unique(subset(rtLinks$link, !(rtLinks$site_no=="")))
        } else {
                idlist <- unique(rtLinks$link)
        }

	## Loop through model run output directories
	for (i in 1:length(modPathList)){
		modoutPath <- modPathList[i]
		modoutTag <- modTagList[i]
		if (numEns > 1) {
			ensoutTag <- ensTagList[i]
		} else {
			ensoutTag <- modoutTag
		}
		
        	modChrtout <- ReadChrtout(modoutPath,idList=idlist,rtlinkFile=routeLinkFile)
		# Subset based on datetime information
		modChrtout <- subset(modChrtout,(POSIXct <= readModEnd) & (POSIXct >= readModStart))
		# Add model run tag
		modChrtout$tag <- modoutTag
		# Add ensemble tags
		modChrtout$enstag <- ensoutTag

		# Calculate accumulated flow
		modChrtout$q_cfs <- NA
		modChrtout$q_af <- NA
		modChrtout$ACCFLOW_af <- NA
		for (j in unique(modChrtout$site_no)[!is.na(unique(modChrtout$site_no))]){
			tmp <- subset(modChrtout,modChrtout$site_no==j)
			tmp$q_af <- NA
			tmp$q_cfs <- NA
			for (k in 1:nrow(tmp)) {
				ts <- ifelse(k==1, as.integer(difftime(tmp$POSIXct[k+1],tmp$POSIXct[k], units="secs")), 
						as.integer(difftime(tmp$POSIXct[k],tmp$POSIXct[k-1], units="secs")))
		 		tmp$q_af[k] <- (tmp$q_cms[k]*ts)/1233.48
				tmp$q_cfs[k] <- tmp$q_cms[k]*35.3147
			}
			modChrtout$q_cfs[modChrtout$site_no==j & !is.na(modChrtout$site_no)] <- tmp$q_cfs
			modChrtout$q_af[modChrtout$site_no==j & !is.na(modChrtout$site_no)] <- tmp$q_af
			qaccum_af <- cumsum(tmp$q_af)
			modChrtout$ACCFLOW_af[modChrtout$site_no==j & !is.na(modChrtout$site_no)] <- qaccum_af
		}
		modChrtout_tmp <- rbindlist(list(modChrtout_tmp,modChrtout))
		# Remove NA values with subsetting
		modChrtout_tmp <- subset(modChrtout_tmp, !is.na(site_no))
		rm(modChrtout)
		gc()
	}
	saveList <- c(saveList, "modChrtout_tmp")
        save(list=saveList, file=tmpRimg)

} # end modchrtout processing


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# LDASIN PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readForc & (readBasinLdasin | readAmfLdasin | readSnoLdasin | readMetLdasin)) {

        ## ------------------------------------------------------------------------
        # Setup variables

        varNames <- c('T2D', 'Q2D', 'U2D', 'V2D', 'PSFC', 'SWDOWN', 'LWDOWN', 'RAINRATE')
        ldasinVars <- as.list( varNames )
        names(ldasinVars) <- varNames
        #ldasinVariableList <- list( ldasin = ldasinVars )
       
        # INDEXES
	# GENERAL FUNCTION
        genIndex_Ldasin <- function(pref, ldasinVars.=ldasinVars) {
        	level0 <- get(paste0(pref, "Index_Lev0"))
		ldasinInd <- list( level0, level0, level0, level0, level0, level0, level0, level0 )
                names(ldasinInd) <- names(ldasinVars.)
                #ldasinIndexList <- list( ldasin = ldasinInd )        
                ldasinInd
                }
        # Run through reads
        ldasinIndexList <- list()
        ldasinVariableList <- list()
        if (readBasinLdasin) {
                ldasinInd <- genIndex_Ldasin("basgeo")
                ldasinIndexList <- c(ldasinIndexList, list( ldasin.basgeo = ldasinInd ))
                ldasinVariableList <- c(ldasinVariableList, list( ldasin.basgeo = ldasinVars ))
                } # end runBasin
        if (readAmfLdasin) {
                ldasinInd <- genIndex_Ldasin("amf")
                ldasinIndexList <- c(ldasinIndexList, list( ldasin.amf = ldasinInd ))
                ldasinVariableList <- c(ldasinVariableList, list( ldasin.amf = ldasinVars ))
                } # end runAmf
        if (readSnoLdasin) {
                ldasinInd <- genIndex_Ldasin("sno")
                ldasinIndexList <- c(ldasinIndexList, list( ldasin.sno = ldasinInd ))
                ldasinVariableList <- c(ldasinVariableList, list( ldasin.sno = ldasinVars ))
                } # end runSno
        if (readMetLdasin) {
                ldasinInd <- genIndex_Ldasin("met")
                ldasinIndexList <- c(ldasinIndexList, list( ldasin.met = ldasinInd ))
                ldasinVariableList <- c(ldasinVariableList, list( ldasin.met = ldasinVars ))
                } # end runMet

        ## ------------------------------------------------------------------------
        # Get data and flatten files
        ## ------------------------------------------------------------------------
        modLdasin_FINAL <- data.table()
        modLdasin.snoday_FINAL <- data.table()
        modLdasin.utcday_FINAL <- data.table()
	modLdasin.utcmonth_FINAL <- data.table()
	message("Read LDASIN")
        for (i in 1:length(forcPathList)) {
                forcPath <- forcPathList[i]
                forcTag <- forcTagList[i]
                # Setup LDASIN files
                filesList <- list.files(path=forcPath, pattern=glob2rx('*.LDASIN_DOMAIN*'), full.names=TRUE)
                if (!is.null(readForcStart) | !is.null(readForcEnd)) {
                	filesList <- subDates(filesList, readForcStart, readForcEnd, ldas2dt)
                }
		#filesList <- filesList[1:100]       
		message(paste0("First: ", filesList[1], " Last: ", filesList[length(filesList)]))
                # Repeat vars and files for all of the index options
                indcnt <- length(names(ldasinIndexList))
                ldasinFilesList <- rep(list(filesList), indcnt)
                names(ldasinFilesList) <- names(ldasinIndexList)
                # Run basin means
		message("LDASIN: Starting GetMultiNcdf")
                ldasinDF <- GetMultiNcdf(indexList=ldasinIndexList,
                                           variableList=ldasinVariableList,
                                           filesList=ldasinFilesList,
                                           parallel=parallelFlag )
                modLdasin_ALL <- ReshapeMultiNcdf(ldasinDF)
		fileGroups <- unique(ldasinDF$fileGroup)
		modLdasinList <- list()
		for (j in fileGroups) {
                        if (length(fileGroups)==1) { modLdasin <- modLdasin_ALL } else { modLdasin <- modLdasin_ALL[[j]] }
                        modLdasin$tag <- forcTag
                        modLdasin$fileGroup <- j
                        modLdasin <- data.table(modLdasin)
                        setkey(modLdasin, tag, fileGroup, statArg, POSIXct)
  			message("LDASIN: Starting daily aggregations")
			## ------------------------------------------------------------------------
  			# Calculate daily forcings
  			# Adjust to dates to match SNOTEL daily report.
  			# SNOTEL daily reports are derived from prior day's hourlies, and all 
  			# times are PST (no daylight savings).
  			# Adjust UTC to PST
  			modLdasin$PST_time <- modLdasin$POSIXct - 8*3600
  			# Calculate truncated date from PST time
  			modLdasin$PST_date <- CalcDateTrunc(modLdasin$PST_time)
  			# Shift by 1 day so aggregations match daily report
  			modLdasin$PST_dateP1 <- modLdasin$PST_date + 1
  			# Unit conversions
  			modLdasin$RelHum <- 0.01 * with(modLdasin,
                        		    0.263*PSFC*Q2D*(exp((17.67*(T2D-273.16))/(T2D-29.65)))^(-1))
  			modLdasin$Wind <- with(modLdasin, sqrt(U2D^2 + V2D^2))
  			# Run daily aggs
			modLdasin.snoday <- modLdasin[, list(T2D_mean=mean(T2D), T2D_min=min(T2D), T2D_max=max(T2D),
                                 Q2D_mean=mean(Q2D), Q2D_min=min(Q2D), Q2D_max=max(Q2D),
                                 U2D_mean=mean(U2D), U2D_min=min(U2D), U2D_max=max(U2D),
                                 V2D_mean=mean(V2D), V2D_min=min(V2D), V2D_max=max(V2D),
                                 PSFC_mean=mean(PSFC), PSFC_min=min(PSFC), PSFC_max=max(PSFC),
                                 SWDOWN_mean=mean(SWDOWN), SWDOWN_min=min(SWDOWN), SWDOWN_max=max(SWDOWN),
                                 LWDOWN_mean=mean(LWDOWN), LWDOWN_min=min(LWDOWN), LWDOWN_max=max(LWDOWN),
                                 RelHum_mean=mean(RelHum), RelHum_min=min(RelHum), RelHum_max=max(RelHum),
                                 Wind_mean=mean(Wind), Wind_min=min(Wind), Wind_max=max(Wind)),
                                 by = "statArg,PST_dateP1"]
  			# Add dummy POSIXct for ease of plotting
  			modLdasin.snoday$POSIXct <- as.POSIXct(paste0(modLdasin.snoday$PST_dateP1, " 00:00"), tz="UTC")
  			# Calculate truncated date from UTC time
  			modLdasin$UTC_date <- CalcDateTrunc(modLdasin$POSIXct)
  			# Run daily aggs
  			modLdasin.utcday <- modLdasin[, list(T2D_mean=mean(T2D), T2D_min=min(T2D), T2D_max=max(T2D),
                                 Q2D_mean=mean(Q2D), Q2D_min=min(Q2D), Q2D_max=max(Q2D),
                                 U2D_mean=mean(U2D), U2D_min=min(U2D), U2D_max=max(U2D),
                                 V2D_mean=mean(V2D), V2D_min=min(V2D), V2D_max=max(V2D),
                                 PSFC_mean=mean(PSFC), PSFC_min=min(PSFC), PSFC_max=max(PSFC),
                                 SWDOWN_mean=mean(SWDOWN), SWDOWN_min=min(SWDOWN), SWDOWN_max=max(SWDOWN),
                                 LWDOWN_mean=mean(LWDOWN), LWDOWN_min=min(LWDOWN), LWDOWN_max=max(LWDOWN),
                                 RelHum_mean=mean(RelHum), RelHum_min=min(RelHum), RelHum_max=max(RelHum),
                                 Wind_mean=mean(Wind), Wind_min=min(Wind), Wind_max=max(Wind)),
                                 by = "statArg,UTC_date"] 
                        mo <- as.integer(format(modLdasin.utcday$UTC_date, "%m"))
                        yr <- as.integer(format(modLdasin.utcday$UTC_date, "%Y"))
                        modLdasin.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"), format="%Y-%m-%d")
                        modLdasin.utcmonth <- modLdasin.utcday[, list(T2D_mean=mean(T2D_mean), T2D_min=min(T2D_mean), T2D_max=max(T2D_mean),
                                 Q2D_mean=mean(Q2D_mean), Q2D_min=min(Q2D_mean), Q2D_max=max(Q2D_mean),
                                 U2D_mean=mean(U2D_mean), U2D_min=min(U2D_mean), U2D_max=max(U2D_mean),
                                 V2D_mean=mean(V2D_mean), V2D_min=min(V2D_mean), V2D_max=max(V2D_mean),
                                 PSFC_mean=mean(PSFC_mean), PSFC_min=min(PSFC_mean), PSFC_max=max(PSFC_mean),
                                 SWDOWN_mean=mean(SWDOWN_mean), SWDOWN_min=min(SWDOWN_mean), SWDOWN_max=max(SWDOWN_mean),
                                 LWDOWN_mean=mean(LWDOWN_mean), LWDOWN_min=min(LWDOWN_mean), LWDOWN_max=max(LWDOWN_mean),
                                 RelHum_mean=mean(RelHum_mean), RelHum_min=min(RelHum_mean), RelHum_max=max(RelHum_mean),
                                 Wind_mean=mean(Wind_mean), Wind_min=min(Wind_mean), Wind_max=max(Wind_mean)),
                                 by = "statArg,UTC_month"] 
  			# Add dummy POSIXct for ease of plotting
  			modLdasin.utcday$POSIXct <- as.POSIXct(paste0(modLdasin.utcday$UTC_date, " 00:00"), tz="UTC")
                        modLdasin.utcmonth$POSIXct <- as.POSIXct(paste0(modLdasin.utcmonth$UTC_month, " 00:00"), tz="UTC")
			# Add tags
                        modLdasin.snoday$tag <- forcTag
                        modLdasin.utcday$tag <- forcTag
			modLdasin.utcmonth$tag <- forcTag
                        modLdasin.snoday$fileGroup <- j
                        modLdasin.utcday$fileGroup <- j
			modLdasin.utcmonth$fileGroup <- j
                        # Bind all
                        modLdasin_FINAL <- rbindlist(list(modLdasin_FINAL, modLdasin))
                        modLdasin.snoday_FINAL <- rbindlist(list(modLdasin.snoday_FINAL, modLdasin.snoday))
                        modLdasin.utcday_FINAL <- rbindlist(list(modLdasin.utcday_FINAL, modLdasin.utcday))
			modLdasin.utcmonth_FINAL <- rbindlist(list(modLdasin.utcmonth_FINAL, modLdasin.utcmonth))
                        rm(modLdasin, modLdasin.snoday, modLdasin.utcday, modLdasin.utcmonth)
                        gc()
                }
	}
        modLdasin_tmp <- list(modLdasin_FINAL, modLdasin.snoday_FINAL, modLdasin.utcday_FINAL, modLdasin.utcmonth_FINAL)
        names(modLdasin_tmp) <- c("native", "snoday", "utcday", "utcmonth")
        saveList <- c(saveList, "modLdasin_tmp")
        save(list=saveList, file=tmpRimg)

 } # end LDASIN



## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# SAVE DATA
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

saveListMod <- c()
saveListForc <- c()

# Read in existing files if available and append=TRUE
if (readMod & modAppend) {
	tempEnv <- new.env()
	load(modReadFileOut, envir=tempEnv)
	objNew <- ls(tempEnv)
	for(n in ls(tempEnv, all.names=TRUE)) assign(n, get(n, tempEnv), .GlobalEnv)
	rm(tempEnv)
	saveListMod <- c(saveListMod, objNew)
	rm(objNew)
}
if (readForc & forcAppend) {
	message("Save append")
        tempEnv <- new.env()
        load(forcReadFileOut, envir=tempEnv)
        objNew <- ls(tempEnv)
        for(n in ls(tempEnv, all.names=TRUE)) assign(n, get(n, tempEnv), .GlobalEnv)
        rm(tempEnv)
        saveListForc <- c(saveListForc, objNew)
	rm(objNew)
}

# Model output reads
if (readMod) {
	if (readBasinLdasout | readAmfLdasout | readSnoLdasout | readMetLdasout) {
		if (modAppend & exists("modLdasout")) {
			for (i in names(modLdasout_tmp)) {
				modLdasout[[i]] <- rbindlist(list(modLdasout[[i]], modLdasout_tmp[[i]]))
			}
		} else {
			modLdasout <- modLdasout_tmp
			saveListMod <- c(saveListMod, "modLdasout")
		}
	}
        if (readBasinRtout) {
                if (modAppend & exists("modRtout_BAS")) {
                        modRtout_BAS <- plyr::rbind.fill(modRtout_BAS, modRtout_BAS_tmp)
                } else {
                        modRtout_BAS <- modRtout_BAS_tmp
                	saveListMod <- c(saveListMod, "modRtout_BAS")
		}
        }
        if (readGwout) {
                if (modAppend & exists("modGwout")) {
                        modGwout <- plyr::rbind.fill(modGwout, modGwout_tmp)
                } else {
                        modGwout <- modGwout_tmp
                	saveListMod <- c(saveListMod, "modGwout")
        	}
	}
        if (readFrxstout) {
                if (modAppend & exists("modFrxstout")) {
                        modFrxstout <- plyr::rbind.fill(modFrxstout, modFrxstout_tmp)
                } else {
                        modFrxstout <- modFrxstout_tmp
                	saveListMod <- c(saveListMod, "modFrxstout")
		}
        }
        if (readChrtout) {
                if (modAppend & exists("modChrtout")) {
                        modChrtout <- rbindlist(list(modChrtout, modChrtout_tmp))
                } else {
                        modChrtout <- modChrtout_tmp
                        saveListMod <- c(saveListMod, "modChrtout")
                }
                #if (modAppend & exists("modChrtout.d")) {
                #        modChrtout.d <- rbindlist(list(modChrtout.d, modChrtout_tmp.d))
                #} else {
                #        modChrtout.d <- modChrtout_tmp.d
                #        saveListMod <- c(saveListMod, "modChrtout.d")
                #}
        }
}

if (readForc) {
        if (readBasinLdasin | readAmfLdasin | readSnoLdasin | readMetLdasin) {
                if (forcAppend & exists("modLdasin")) {
                        for (i in names(modLdasin_tmp)) {
                                modLdasin[[i]] <- rbindlist(list(modLdasin[[i]], modLdasin_tmp[[i]]))
                        }
                } else {
                        modLdasin <- modLdasin_tmp
                        saveListForc <- c(saveListForc, "modLdasin")
                }
        }
}

saveListMod <- unique(saveListMod)
saveListForc <- unique(saveListForc)

#if ( (modReadFileOut == forcReadFileOut) & readMod & readForc ) {
#		saveListMod <- c(saveListMod, saveListForc)
#		save(list=saveListMod, file=modReadFileOut)
#} else {
	if (readMod) save(list=saveListMod, file=modReadFileOut)
	if (readForc) save(list=saveListForc, file=forcReadFileOut)
#}
file.remove(tmpRimg)

#stopCluster(cl)
proc.time()

