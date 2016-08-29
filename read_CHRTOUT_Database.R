###################################################
##             CHRTOUT OUTPUT READS              ##
###################################################

library(plyr)
library(data.table)

# First create temporary file paths for output. 
# They will be named according to their rank/size
# from the MPI information
tmpFilePath = paste0(tmpDir,'/CHRTOUT_',size,'_',rank,'.Rdata')

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

# Call ReadChrtout to read in all streamflow data.
modChrtout_tmp <- data.table()
	
if (!is.null(readLink2gage)) {
    idlist <- unique(readLink2gage$link)
} else {
    idlist <- unique(subset(rtLinks$link, !(rtLinks$site_no=="")))
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
        fileTemp <- paste0(tmpDir,'/CHRTOUT_',size,'_',rank,'.Rdata')
        
    }
}