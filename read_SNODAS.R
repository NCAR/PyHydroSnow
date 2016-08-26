#################################
##        SNODAS READS         ##
#################################
                                
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
tmpRimg <- tempfile(fileext=".Rdata",tmpdir=tmpDir)
message(paste0("Temp output file:", tmpRimg))


## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# INDEX PROCESSING
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

if (readSnodas & readSnoSnodas) {
	# SNOTEL sites
	snoIndex_Lev0 <- list()
	for (i in 1:length(ptgeo.sno$id)) {
                if (!is.na(ptgeo.sno$we[i]) & !is.na(ptgeo.sno$sn[i])) {
                        snoIndex_Lev0[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1),
                                               end=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1), stat="mean")
		}
	} 

}

if (readSnodas & readAmfSnodas) {
        # Ameriflux sites
        amfIndex_Lev0 <- list()
        for (i in 1:length(ptgeo.amf$id)) {
                if (!is.na(ptgeo.amf$we[i]) & !is.na(ptgeo.amf$sn[i])) {
                        amfIndex_Lev0[[as.character(ptgeo.amf$id[i])]] <- list(start=c(ptgeo.amf$we[i], ptgeo.amf$sn[i], 1),
                                               end=c(ptgeo.amf$we[i], ptgeo.amf$sn[i], 1), stat="mean")
                }
        }

}

if (readSnodas & readMetSnodas) {
        # Other Met sites
        metIndex_Lev0 <- list()
        for (i in 1:length(ptgeo.met$id)) {
                if (!is.na(ptgeo.met$we[i]) & !is.na(ptgeo.met$sn[i])) {
                        metIndex_Lev0[[as.character(ptgeo.met$id[i])]] <- list(start=c(ptgeo.met$we[i], ptgeo.met$sn[i], 1),
                                               end=c(ptgeo.met$we[i], ptgeo.met$sn[i], 1), stat="mean")
                }
        }

}

# SNODAS point processing
if (readSnodas & (readSnoSnodas | readAmfSnodas | readMetSnodas)) {
	# Setup variables
	if (readMetSnodas) {
		varNames <- c('SNOWH')
        	varLabels <- c('SNOWH')
	} else {
		varNames <- c('SNEQV')
        	varLabels <- c('SNEQV')
	}
	snodasVars <- as.list(varNames)
        names(snodasVars) <- varLabels
        genIndex_Snodas <- function(pref,snodasVars.=snodasVars) {
		lev0 <- get(paste0(pref,"Index_Lev0"))
		snodasInd <- list(lev0)
		names(snodasInd) <- names(snodasVars.)
                snodasInd
        }

	# Run through reads
        snodasIndexList <- list()
        snodasVariableList <- list()
        if (readSnoSnodas) {
		snodasInd <- genIndex_Snodas("sno")
                snodasIndexList <- c(snodasIndexList,list(snodas.sno = snodasInd ))
                snodasVariableList <- c(snodasVariableList, list( snodas.sno = snodasVars ))
        }	
        if (readAmfSnodas) {
                snodasInd <- genIndex_Snodas("amf")
                snodasIndexList <- c(snodasIndexList,list(snodas.amf = snodasInd ))
                snodasVariableList <- c(snodasVariableList, list( snodas.amf = snodasVars ))
        }
	if (readMetSnodas) {
                snodasInd <- genIndex_Snodas("met")
                snodasIndexList <- c(snodasIndexList,list(snodas.met = snodasInd ))
                snodasVariableList <- c(snodasVariableList, list( snodas.met = snodasVars ))
        }

	# Get the data and flatten files
	snodas_FINAL <- data.table()
        snodas.utcday_FINAL <- data.table()
        snodas.utcmonth_FINAL <- data.table()
        for (i in 1:length(snodasPathList)) {
                snodasPath <- snodasPathList[i]
                snodasTag <- snodasTagList[i]
                filesList <- list.files(path=snodasPath,pattern=glob2rx('SNODAS_REGRIDDED_*.nc'),full.names=TRUE)
                if (!is.null(readSnodasStart) | !is.null(readSnodasEnd)) {
                        filesList <- subDates(filesList, readSnodasStart, readSnodasEnd, snodas2dt)
                }
                message(paste0("First: ", filesList[1], " Last: ", filesList[length(filesList)]))
                # Repeat vars and files for all of the index options
                indcnt <- length(names(snodasIndexList))
                snodasFilesList <- rep(list(filesList),indcnt)
                names(snodasFilesList) <- names(snodasIndexList)
                # Run point extraction
                snodasOutDF <- GetMultiNcdf(indexList=snodasIndexList,
                                            variableList=snodasVariableList,
                                            filesList=snodasFilesList,
                                            parallel=parallelFlag )
                snodasOutDF_ALL <- ReshapeMultiNcdf(snodasOutDF)
                fileGroups <- unique(snodasOutDF$fileGroup)
 
                snodasOutList <- list()
                for (j in fileGroups) {
                    if (length(fileGroups)==1) { snodasout <- snodasOutDF_ALL } else { snodasout <- snodasOutDF_ALL[[j]] }
                    # Data mgmt
		    snodasout$tag <- snodasTag
                    snodasout$fileGroup <- j
                    snodasout <- data.table(snodasout)
                    setkey(snodasout, tag, fileGroup, statArg, POSIXct)
                    # Calculate truncated date from UTC time
                    snodasout$UTC_date <- CalcDateTrunc(snodasout$POSIXct)
                    # List data by UTC day
		    if (readMetSnodas) {
			snodasout.utcday <- snodasout[,list(SNOWH=SNOWH),by = "statArg,UTC_date"]
		    } else {
                    	snodasout.utcday <- snodasout[,list(SNEQV=SNEQV),by = "statArg,UTC_date"]
		    }
                    mo <- as.integer(format(snodasout.utcday$UTC_date,"%m"))
                    yr <- as.integer(format(snodasout.utcday$UTC_date,"%Y"))
                    snodasout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"),format="%Y-%m-%d")
		    if (readMetSnodas) {
			snodasout.utcmonth <- snodasout.utcday[,list(SNOWH=mean(SNOWH)),
                                                  by = "statArg,UTC_month"]
		    } else {
                    	snodasout.utcmonth <- snodasout.utcday[,list(SNEQV=mean(SNEQV)),
                        	                  by = "statArg,UTC_month"]
		    }
                    snodasout.utcday$POSIXct <- as.POSIXct(paste0(snodasout.utcday$UTC_date, " 00:00"), tz="UTC")
                    snodasout.utcmonth$POSIXct <- as.POSIXct(paste0(snodasout.utcmonth$UTC_month, " 00:00"),tz="UTC")
                    # Add tags
                    snodas_FINAL <- rbindlist(list(snodas_FINAL, snodasout))
                    snodas.utcday_FINAL <- rbindlist(list(snodas.utcday_FINAL, snodasout.utcday))
                    snodas.utcmonth_FINAL <- rbindlist(list(snodas.utcmonth_FINAL, snodasout.utcmonth))
                    rm(snodasout, snodasout.utcday, snodasout.utcmonth)
                    gc()
                }

        }

	snodas_tmp <- list(snodas_FINAL,snodas.utcday_FINAL,snodas.utcmonth_FINAL)
        names(snodas_tmp) <- c("native","utcday","utcmonth")
        saveList <- c(saveList,"snodas_tmp")
        save(list=saveList,file=tmpRimg)

}
# Save the data

saveListSnodas <- c()

# Read in existing files if available and append=TRUE
if (readSnodas & snodasAppend) {
    tmpEnv <- new.env()
    load(snodasReadFileOut, envir=tempEnv)
    objNew <- ls(tempEnv)
    for(n in ls(tempEnv, all.names=TRUE)) assign(n, get(n, tempEnv), .GlobalEnv)
    rm(tempEnv)
    saveListSnodas <- c(saveListSnodas, objNew)
    rm(objNew)
}

if ( readSnodas ) {
    if (readBasinSnodas | readSnoSnodas | readAmfSnodas | readMetSnodas) {
        if (snodasAppend & exists("snodasout")) {
            for (i in names(snodas_tmp)){
                snodasout[[i]] <- rbindlist(list(snodasout[[i]],snodas_tmp[[i]]))
            }
        } else {
            snodasout <- snodas_tmp
            saveListSnodas <- c(saveListSnodas, "snodasout")
        }
    }
}

saveListSnodas <- unique(saveListSnodas)

save(list=saveListSnodas, file=snodasReadFileOut)

if (ncores>1) {
	stopCluster(cl)
}
proc.time()    
