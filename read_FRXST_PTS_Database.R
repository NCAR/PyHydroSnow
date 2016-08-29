###################################################
##             FRXST Points Read                 ##
###################################################

library(plyr)
library(data.table)

# First create temporary file paths for output. 
# They will be named according to their rank/size
# from the MPI information
tmpFilePath = paste0(tmpDir,'/FRXST_PTS_',size,'_',rank,'.Rdata')

# Setup lookups
if (exists("stid2gageList")) {
	stid2gage <- data.frame(st_id=names(stid2gageList), STAID=unlist(stid2gageList), stringsAsFactors=FALSE)
	stid2gage$st_id <- as.integer(stid2gage$st_id)
}

if (readEnsemble) {
	numEns <- length(ensembleList)
	# Expand modPathList to number of ensembles
	modPathList <- expandModPathList(numEns,ensembleList,length(modPathList),modPathList)
	modTagList <- expandModTagList(numEns,length(modPathList),modTagList)
	ensTagList <- expandEnsTagList(numEns,length(modPathList),modPathList,ensembleList)
} else {
	numEns <- 1
}

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
    modFrxstout <- ReadFrxstPts(paste0(modoutPath, '/frxst_pts_out.txt'), stIdType='integer')

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

# Gather files from other processors and write to a single output file from
# the master processor
if (rank == 0){
    # Loop through, read in data from each processor, delete temporary data,
    # append, then save to single output file at end.
    # NOTE: If temporary file is not found, it's assumed the processor is still
    # working on the dataset. In this case, the master processor will wait until
    # the fie is done. It will double check to make sure the file size isn't changing
    # anymore to ensure a complete dataset is read in.
    for (proc in 1:size){
        fileTemp <- paste0(tmpDir,'/FRXST_PTS_',size,'_',rank,'.Rdata')
        
    }
}