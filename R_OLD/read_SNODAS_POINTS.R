###################################################
##             SNODAS Points Read                ##
###################################################

library(plyr)
library(data.table)

# First create temporary file paths for output. 
# They will be named according to their rank/size
# from the MPI information
tmpFilePath = paste0(tmpDir,'/SNODAS_PTS_',size,'_',rank,'.Rdata')

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# INDEX PROCESSING FOR SNODAS READS
## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

# Snow Sites
snoIndex_lev0 <- list()
for (i in 1:length(ptgeo.sno$id)) {
    if (!is.na(ptgeo.sno$we[i]) & !is.na(ptgeo.sno$sn[i]) {
        snoIndex_Lev0[[as.character(ptgeo.sno$id[i])]] <- list(start=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1),
            end=c(ptgeo.sno$we[i], ptgeo.sno$sn[i], 1), stat="mean")            
    }
}

varName <- c('SNEQV','SNOWH')
varLabels <- c('SNEQV','SNOWH')
snowVars <- as.list(varNames)
names(snowVars) <- varLabels

genIndex_Snow <- function(pref,snowVars.=snowVars) { 
    lev0 <- get(paste0(pref,"Index_Lev0"))
    snowInd <- list(lev0)
    names(snowInd) <- names(snowVars.)
    snowInd
}

# Run through reads
snowIndexList <- list()
snowVariableList <- list()

snowInd <- genIndex_Snow("sno")
snowIndexList <- c(snowIndexList,list(snow.sno = snowInd ))
snowVariableList <- c(snowVariableList, list( snow.sno = snowVars ))

# Get the data and flatten files
snow_FINAL <- data.table()
snow.utcday_FINAL <- data.table()
snow.utcmonth_FINAL <- data.table()

for (i in 1:length(snodasPathList)) {
    snodasPath <- snodasPathList[i]
    snodasTag <- 'SNODAS'
    # Setup SNODAS files
    filesList <- list.files(path=snodasPath, pattern=glob2rx('SNODAS_REGRIDDED_*.nc'), full.names=TRUE)
    if (!is.null(readSnodasStart) | !is.null(readSnodasEnd)) {
        filesList <- subDates(filesList, readSnodasStart, readSnodasEnd, snodas2dt)
    }
    message(paste0("First: ", filesList[1], " Last: ", filesList[length(filesList)]))
    indcnt <- length(names(snowIndexList))
    snowFilesList <- rep(list(filesList), indcnt)
    names(snowFilesList) <- names(snowIndexList)

    snodasDF <- GetMultiNcdf(indexList=snowIndexList,
                             variableList=snowVariableList,
                             filesList=snowFilesList,
                             parallel=parallelFlag )
    snodas_ALL <- ReshapeMultiNcdf(snodasDF)
    fileGroups <- unique(snodasDF$fileGroup)
    snodasoutList <- list()
    for (j in fileGroups) {
        if (length(fileGroups)==1) { snodasout <- snodasout_ALL } else { snodasout <- snodasout_ALL[[j]] }
        # Data mgmt
        snodasout$tag <- snodasTag
        snodasout$fileGroup <- j
        snodasout <- data.table(snodasout)
        setkey(snodasout,tag,fileGroup,statArg,POSIXct)
        # Calculate truncated date from UTC time
        snodasout$UTC_date <- CalcDateTrunc(snodasout$POSIXct)
        # List data by UTC day
        snodasout.utcday <- snodasout[,list(SNEQV=SNEQV,SNOWH=SNOWH),by = "statArg,UTC_date"]
        mo <- as.integer(format(snodasout.utcday$UTC_date,"%m"))
        yr <- as.integer(format(snodasout.utcday$UTC_date,"%Y"))
        snodasout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"),format="%Y-%m-%d")
        snodasout.utcmonth <- snodasout.utcday[,list(SNEQV_mean=mean(SNEQV),SNOWH_mean=mean(SNOWH)), by = 'statArg,UTC_month']
        snodasout.utcday$POSIXct <- as.POSIXct(paste0(snodasout.utcday$UTC_date, " 00:00"), tz="UTC")
        snodasout.utcmonth$POSIXct <- as.POSIXct(paste0(snodasout.utcmonth$UTC_month, " 00:00"),tz="UTC")
        snodasout.utcday.tag <- snodasTag
        snodasout.utcday.fileGroup <- j
        snodasout.utcmonth.tag <- snodasTag
        snodasout.utcmonth.fileGroup <- j
        # Add tags
        snodas_FINAL <- rbindlist(list(snodas_FINAL, snowout))
        snodas.utcday_FINAL <- rbindlist(list(snodas.utcday_FINAL, snodasout.utcday))
        snodas.utcmonth_FINAL <- rbindlist(list(snodas.utcmonth_FINAL, snodasout.utcmonth))
        rm(snodasout, snodasout.utcday, snodasout.utcmonth)
        gc()
    }

}

# Save data temporarily to output file
snow_tmp <- list(snow_FINAL,snow.utcday_FINAL,snow.utcmonth_FINAL)
names(snow_tmp) <- c("native","utcday","utcmonth")
saveList <- c(saveList,"snow_tmp")
save(list=saveList,file=tmpFilePath)

# Go back, read in data from all processors, and bind together
if (rank == 0){
    for (pId in 1:size){
        fileTmpIn <- paste0(tmpDir,'/SNOW_PTS_',size,'_',pId,'.Rdata')
        for (waitStep in 1:1000){
            if (!file.exists(fileTmpIn)){
                Sys.sleep(10)            
            } else {
                break
            }      
        }
        
        if (!file.exists){
            stop('ERROR: ',fileTmpIn,' Not Found.')
        }

        # Double check to make sure file isn't still being written to
        for (waitStep in 1:100){
            if (waitStep == 1){
                sizeCheck = file.info(fileTmpIn)    
            } else {
                sizeNew = file.info(fileTmpIn)
                if (sizeNew != sizeCheck){
                    continue
                } else {
                    break
                }
            }
        }

        if (waitStep == 100){
            stop('ERROR: ',fileTmpIn,' Did not finish writing.')        
        }

        # Read data in and append
        load(fileTmpIn)
        snowout[[pId]] <- rbindlist(list(snowout[[pId]],snow_tmp[[pId]]))
    }

    # Save output
    save(snowout,modReadFilePathSnow)
}