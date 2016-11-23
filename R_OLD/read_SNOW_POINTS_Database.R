###################################################
##             SNOW Points Read                  ##
###################################################

library(plyr)
library(data.table)

# First create temporary file paths for output. 
# They will be named according to their rank/size
# from the MPI information
tmpFilePath = paste0(tmpDir,'/SNOW_PTS_',size,'_',rank,'.Rdata')

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------
# INDEX PROCESSING FOR MODEL READS
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
    message(paste0("First: ", filesList[1], " Last: ", filesList[length(filesList)]))
    indcnt <- length(names(snowIndexList))
    snowFilesList <- rep(list(filesList), indcnt)
    names(snowFilesList) <- names(snowIndexList)

    snowDF <- GetMultiNcdf(indexList=snowIndexList,
                           variableList=snowVariableList,
                           filesList=snowFilesList,
                           parallel=parallelFlag )
    snowout_ALL <- ReshapeMultiNcdf(snowDF)
    fileGroups <- unique(snowDF$fileGroup)
    snowoutList <- list()
    for (j in fileGroups) {
        if (length(fileGroups)==1) { snowout <- snowout_ALL } else { snowout <- snowout_ALL[[j]] }
        # Data mgmt
        snowout$tag <- modoutTag
        snowout$fileGroup <- j
        snowout <- data.table(snowout)
        setkey(snowout,tag,fileGroup,statArg,POSIXct)
        # Calculate truncated date from UTC time
        snowout$UTC_date <- CalcDateTrunc(snowout$POSIXct)
        # List data by UTC day
        snowout.utcday <- snowout[,list(SNEQV=SNEQV,SNOWH=SNOWH),by = "statArg,UTC_date"]
        mo <- as.integer(format(snowout.utcday$UTC_date,"%m"))
        yr <- as.integer(format(snowout.utcday$UTC_date,"%Y"))
        snowout.utcday$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"),format="%Y-%m-%d")
        snowout.utcmonth <- snowout.utcday[,list(SNEQV_mean=mean(SNEQV),SNOWH_mean=mean(SNOWH)), by = 'statArg,UTC_month']
        snowout.utcday$POSIXct <- as.POSIXct(paste0(snowout.utcday$UTC_date, " 00:00"), tz="UTC")
        snowout.utcmonth$POSIXct <- as.POSIXct(paste0(snowout.utcmonth$UTC_month, " 00:00"),tz="UTC")
        snowout.utcday.tag <- modoutTag
        snowout.utcday.fileGroup <- j
        snowout.utcday.enstag <- enstag <- ensoutTag
        snowout.utcmonth.tag <- modoutTag
        snowout.utcmonth.fileGroup <- j
        snowout.utcmonth.enstag <- enstag <- ensoutTag
        # Add tags
        snow_FINAL <- rbindlist(list(snow_FINAL, snowout))
        snow.utcday_FINAL <- rbindlist(list(snow.utcday_FINAL, snowout.utcday))
        snow.utcmonth_FINAL <- rbindlist(list(snow.utcmonth_FINAL, snowout.utcmonth))
        rm(snowout, snowout.utcday, snowout.utcmonth)
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