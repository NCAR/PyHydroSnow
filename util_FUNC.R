# Expand model path list to account for ensembles
expandModPathList <- function(numEns,ensembleList,modPathLen,modPathList_tmp) {
   modPathList <- c()
   for (modPath in modPathList_tmp){
      for (ensembleName in ensembleList) {
         pthTmp <- paste0(modPath,"/",ensembleName)
         modPathList <- c(modPathList,pthTmp)
      }
   }
   modPathList
}

# Expand model tag list to account for ensembles
expandModTagList <- function(numEns,modPathLen,modTagList_tmp) {
  modTagList <- c()
  for (i in 1:modPathLen){
    modTagList <- c(modTagList,modTagList_tmp[ceiling(i/numEns)])
  }
  modTagList
}

# Expand ensemble tag list to account for various model groups
expandEnsTagList <- function(numEns,modPathLen,modPathList,ensembleList_tmp){
  ensembleList <- c()
  for (i in 1:length(modPathList)){
    if (i%%numEns == 0) {
      ensembleList <- c(ensembleList,ensembleList_tmp[numEns])
    } else {
      ensembleList <- c(ensembleList,ensembleList_tmp[i%%numEns])
    }
  }
  ensembleList
}

# Basin mean function
basin_avg <- function(myvar, mskvar, minValid=-1e+29) {
   myvar[which(myvar<minValid)]<-NA
   sum(mskvar*myvar, na.rm=TRUE)/sum(mskvar, na.rm=TRUE)
 }

# Basin sum function (mostly for snow)
basin_sum_swe <- function(myvar, mskvar, res, minValid=-1e+29) {
   myvar[which(myvar<minValid)]<-NA
   res = res*1000.0 # Convert resolution from km to meters
   resSquared <- res*res
   sum((mskvar/1000.0)*resSquared*myvar, na.rm=TRUE) #res needs to be in meters, convert SWE to meters.
   # Output is cubic meters.
} 

# Filename to date conversion functions
rt2dt <- function(x) {as.POSIXct(unlist(strsplit(x, "[.]"))[1], format="%Y%m%d%H%M", tz="UTC")}
ldas2dt <- function(x) {as.POSIXct(unlist(strsplit(x, "[.]"))[1], format="%Y%m%d%H", tz="UTC")}
snodas2dt <- function(x) {as.POSIXct(unlist(strsplit(unlist(strsplit(x,"_"))[3],"[.]"))[1], format="%Y%m%d", tz="UTC")}

# Subset file list by dates
subDates <- function(filesList, startDate, endDate, func) {
	tmp <- basename(filesList)
	tmp <- as.POSIXct(apply(as.data.frame(tmp), 1, func), origin='1970-01-01 00:00.00 UTC', tz="UTC")              
	if (!is.null(startDate) & !is.null(endDate)) {
		filesList <- filesList[tmp >= startDate & tmp <= endDate]
        } else if (!is.null(startDate) & is.null(endDate)) {
		filesList <- filesList[tmp >= startDate]
	} else if (is.null(startDate) & !is.null(endDate)) {
                filesList <- filesList[tmp <= endDate]
	}
	filesList
}

# Subset object by dates
subDf <- function(df, stdate=NULL, enddate=NULL) {
  # Subset
  if (!is.null(stdate) & !is.null(enddate)) {
    df <- subset(df, df$POSIXct >= stdate & df$POSIXct <= enddate)
  }
  if (!is.null(stdate) & is.null(enddate)) {
    df <- subset(df, df$POSIXct >= stdate)
  }
  if (is.null(stdate) & !is.null(enddate)) {
    df <- subset(df, df$POSIXct <= enddate)
  }
  df
}

# Remap data
remapData <- function(inObj, mapObj) {
first <- TRUE
for (i in names(mapObj)) {
	if ( mapObj[[i]] %in% names(inObj) ) {
		out <- data.frame(x=inObj[,mapObj[[i]]], stringsAsFactors=FALSE)
		names(out) <- i
		if(first) {outDf <- out} else {outDf <- cbind(outDf, out)}
		first <- FALSE
	}
}
outDf
}

# Calculate various basin snow metrics
basSnowMetrics <- function(sweVar,mskVar,basElev,runoff,res,runoffFlag) {
  # Establish constants
  minValid <- 0.0
  maxValid <- 5000.0
  minValidRo <- 0.0
  maxValidRo <- 99999999.0
  sweVar[which(sweVar < minValid)] <- NA
  sweVar[which(sweVar > maxValid)] <- NA
  resSquared = res*res
  resSquaredMeters = (res*1000.0)*(res*1000.0)
  # Establish output list containing metrics
  outList <- list()
  # First calculate total basin area, snow covered area, and fraction of basin covered by snow
  outList$totArea <- sum(mskVar*resSquared, na.rm=TRUE) # Squared km
  sweVarTmp <- sweVar
  sweVarTmp[which(sweVarTmp <= minValid)] <- 0.0
  sweVarTmp[which(sweVarTmp > 0.0)] <- 1.0
  sweVarTmp[which(sweVarTmp > maxValid)] <- 0.0
  outList$totSnoArea <- sum(mskVar*sweVarTmp*resSquared, na.rm=TRUE) # Squared km
  outList$snoFrac <- outList$totSnoArea/outList$totArea
  # Calculate mean snow line using a threshold of 25.4 mm of snow
  indLine <- which((sweVar <= 25.4) & (sweVar > 5.0) & (mskVar > 0.0))
  if (length(indLine) == 0) {
    outList$meanSnoElevMeters <- NA
    outList$meanSnoElevFeet <- NA
  } else {
    outList$meanSnoElevMeters <- sum(mskVar[indLine]*basElev[indLine], na.rm=TRUE)/length(indLine)
    outList$meanSnoElevFeet <- outList$meanSnoElevMeters*3.28084
  }
  # Calculate snow volume
  outList$sweVolCubMeters <- sum((mskVar/1000.0)*resSquaredMeters*sweVar, na.rm=TRUE)
  outList$sweVolAcreFeet <- outList$sweVolCubMeters/1233.48184
  # Calculate mean/max SWE
  indSWE <- which((sweVar > 0.0) & (mskVar == 1.0))
  if (length(indSWE) == 0) {
    outList$meanSweMM <- 0.0
    outList$maxSweMM <- 0.0
  } else {
    outList$meanSweMM <- sum(sweVar[indSWE], na.rm=TRUE)/length(indSWE)
    outList$maxSweMM <- max(sweVar[indSWE])
  }
  # Calculate basin runoff volume if runoff flag has been set to 1
  if (runoffFlag == 1) {
    runoff[which(runoff < minValidRo)] <- NA
    runoff[which(runoff > maxValidRo)] <- NA
    outList$roCubMeters <- sum((mskVar/1000.0)*resSquaredMeters*runoff, na.rm=TRUE)
    outList$roAcreFeet <- outList$roCubMeters/1233.48184
  } else {
    outList$roCubMeters <- 0.0
    outList$roAcreFeet <- 0.0
  } 
  # Calculate mean/max snow depth
  # TODO
  # Calculate min/mean/max rho of snow in kg/m3
  # TODO
  return(outList)
}

# Subset forecast points and associated basin mask information
subSetBasins <- function(mskgeo.nameList,
                         frxstPts,
                         basin2gageList,
                         gage2basinList,
                         mskgeo.areaList,
                         mskgeo.countInds,
                         mskgeo.List,
                         mskgeo.maxInds,
                         mskgeo.minInds,
                         mskhyd.areaList,
                         mskhyd.countInds,
                         mskhyd.List,
                         mskhyd.maxInds,
                         mskhyd.minInds,
                         mskhyd.nameList,
                         stid2gageList,
                         subSet) {

    # First determine which subset names are of type forecast points.
    frxstPtsSub <- subSet$id[which(subSet$type == 2)]
    ind <- c()
    for (frxstPt in frxstPtsSub){
        i <- which(mskgeo.nameList == frxstPt)
        ind <- append(ind,i)
    }
    # Subset lists/data frames
    mskgeo.nameList <- mskgeo.nameList[ind]
    frxstPts <- frxstPts[ind,]
    gage2basinList <- gage2basinList[ind]
    basin2gageList <- basin2gageList[ind]
    mskgeo.areaList <- mskgeo.areaList[ind]
    #mskgeo.countInds <- mskgeo.countInds$x[ind,]
    mskgeo.List <- mskgeo.List[ind]
    mskgeo.maxInds <- mskgeo.maxInds[ind,]
    mskgeo.minInds <- mskgeo.minInds[ind,]
    mskhyd.areaList <- mskhyd.areaList[ind]
    mskhyd.countInds <- mskhyd.countInds[ind,]
    mskhyd.List <- mskhyd.List[ind]
    mskhyd.maxInds <- mskhyd.maxInds[ind,]
    mskhyd.minInds <- mskhyd.minInds[ind,]
    mskhyd.nameList <- mskhyd.nameList[ind]
    stid2gageList <- stid2gageList[ind]

    return(list(mskgeo.nameList,frxstPts,basin2gageList,gage2basinList,
                mskgeo.areaList,mskgeo.countInds,mskgeo.List,mskgeo.maxInds,
                mskgeo.minInds,mskhyd.areaList,mskhyd.countInds,
                mskhyd.List,mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList,
                stid2gageList))
}

# Subset reach based NHD catchment points
subSetReachPts <- function(subSet,gageList){
    # First determine which subset names are of type reach based
    reachPtsSub = subSet$id[which(subSet$type == 1)]
    ind <- c()
    for (reachPt in reachPtsSub){
        i <- which(gageList == reachPt)
        ind <- append(ind,i)    
    }
    # Subset the gageList
    gageList <- gageList[ind,]
    return(list(gageList))
}

# Subset regions only (shapefile derived regions, etc)
subSetRegions <- function(mskgeo.nameList,
                          mskgeo.areaList,
                          mskgeo.countInds,
                          mskgeo.List,
                          mskgeo.maxInds,
                          mskgeo.minInds,
                          mskhyd.areaList,
                          mskhyd.countInds,
                          mskhyd.List,
                          mskhyd.maxInds,
                          mskhyd.minInds,
                          mskhyd.nameList,
                          subSet){

    # First determine which subset names are of type region.
    regionsSub <- subSet$id[which(subSet$type == 3)]
    ind <- c()
    for (region in regionSub){
        i <- which(mskgeo.nameList == region)
        ind <- append(ind,i)   
    }
    # Subset lists/data frames
    mskgeo.nameList <- mskgeo.nameList[ind]
    mskgeo.areaList <- mskgeo.areaList[ind]
    mskgeo.countInds <- mskgeo.countInds$x[ind,]
    mskgeo.List <- mskgeo.List[ind]
    mskgeo.maxInds <- mskgeo.maxInds[ind,]
    mskgeo.minInds <- mskgeo.minInds[ind,]
    mskhyd.areaList <- mskhyd.areaList[ind]
    mskhyd.countInds <- mskhyd.countInds[ind,]
    mskhyd.List <- mskhyd.List[ind]
    mskhyd.maxInds <- mskhyd.maxInds[ind,]
    mskhyd.minInds <- mskhyd.minInds[ind,]
    mskhyd.nameList <- mskhyd.nameList[ind]
    
    return(list(mskgeo.nameList,mskgeo.areaList,mskgeo.countInds,
                mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                mskhyd.areaList,mskhyd.countInds,mskhyd.List,
                mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList))
}

# Subset snow points
subSetPoints <- function(ptgeo.sno,subSet){
    # First determine which subset names are of type point
    ptsSub <- subSet$id[which(subSet$type == 4)]
    ind <- c()
    for (pt in ptsSub){
        i <- which(ptgeo.sno$id == pt)
        ind <- append(ind,i)
    }
    # Subset lists/data frames
    ptgeo.sno <- ptgeo.sno[ind,]
}

# Assign gageList points amongst different processors
mpiGageList <- function(size,rank,gageList){
    # Calculate total size of list. From there, split up portions
    # based on rank.
    masterLength <- length(gageList$site_no)
    localLength <- floor(masterLength/size)
    begInd <- localLength*rank + 1
    endInd <- localLength*(rank+1)
    if (rank != 0){
        localLength <- floor(masterLength/size)
        gageList <- gageList[begInd:endInd,]
    } else {
        localLength <- floor(masterLength/size) 
        remainder <- masterLength - (localLength*size)
        if (remainder == 0){
            gageList <- gageList[begInd:endInd,]
        } else {
            rBegInd <- size*localLength + 1
            rEndInd <- masterLength
            gageList <- rbind(gageList[begInd:endInd,],gageList[rBegInd:rEndInd,])
        }
    }

    # Return list
    return(list(gageList))
}

# Assign frxst points amongst different processors
mpiFrxst <- function(size,rank,frxstPts,basin2gageList,gage2basinList,
                     stid2gageList,mskgeo.areaList,mskgeo.countInds,
                     mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                     mskhyd.areaList,mskhyd.countInds,mskhyd.List,
                     mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList,
                     mskgeo.nameList,gageList){

    # Calculate total size of list. From there, split up portions
    # based on rank
    masterLength <- length(frxstPts$id)
    localLength <- floor(masterLength/size)
    begInd <- localLength*rank + 1
    endInd <- localLength*(rank+1)
    if (rank != 0){
        localLength <- floor(masterLength/size)
        frxstPts <- frxstPts[begInd:endInd,]
        basin2gageList <- basin2gageList[begInd:endInd]
        gage2basinList <- gage2basinList[begInd:endInd]
        stid2gageList <- stid2gageList[begInd:endInd]
        mskgeo.areaList <- mskgeo.areaList[begInd:endInd]
        mskgeo.countInds <- mskgeo.countInds[begInd:endInd,]
        mskgeo.List <- mskgeo.List[begInd:endInd]
        mskgeo.maxInds <- mskgeo.maxInds[begInd:endInd,]
        mskgeo.minInds <- mskgeo.minInds[begInd:endInd,]
        mskhyd.areaList <- mskhyd.areaList[begInd:endInd]
        mskhyd.countInds <- mskhyd.countInds[begInd:endInd,]
        mskhyd.List <- mskhyd.List[begInd:endInd]
        mskhyd.maxInds <- mskhyd.maxInds[begInd:endInd,]
        mskhyd.minInds <- mskhyd.minInds[begInd:endInd,]
        mskhyd.nameList <- mskhyd.nameList[begInd:endInd]
        mskgeo.nameList <- mskgeo.nameList[begInd:endInd]
        gageList <- gageList[begInd:endInd,]
    } else {
        localLength <- floor(masterLength/size) 
        remainder <- masterLength - (localLength*size)
        if (remainder == 0){
            frxstPts <- frxstPts[begInd:endInd,]
            basin2gageList <- basin2gageList[begInd:endInd]
            gage2basinList <- gage2basinList[begInd:endInd]
            stid2gageList <- stid2gageList[begInd:endInd]
            mskgeo.areaList <- mskgeo.areaList[begInd:endInd]
            mskgeo.countInds <- mskgeo.countInds[begInd:endInd,]
            mskgeo.List <- mskgeo.List[begInd:endInd]
            mskgeo.maxInds <- mskgeo.maxInds[begInd:endInd,]
            mskgeo.minInds <- mskgeo.minInds[begInd:endInd,]
            mskhyd.areaList <- mskhyd.areaList[begInd:endInd]
            mskhyd.countInds <- mskhyd.countInds[begInd:endInd,]
            mskhyd.List <- mskhyd.List[begInd:endInd]
            mskhyd.maxInds <- mskhyd.maxInds[begInd:endInd,]
            mskhyd.minInds <- mskhyd.minInds[begInd:endInd,]
            mskhyd.nameList <- mskhyd.nameList[begInd:endInd]
            mskgeo.nameList <- mskgeo.nameList[begInd:endInd]
            gageList <- gageList[begInd:endInd,]
        } else {
            rBegInd <- size*localLength + 1
            rEndInd <- masterLength
            frxstPts <- rbind(frxstPts[begInd:endInd,],frxstPts[rBegInd:rEndInd,])
            basin2gageList <- rbind(basin2gageList[begInd:endInd],basin2gageList[rBegInd:rEndInd])
            gage2basinList <- rbind(gage2basinList[begInd:endInd],gage2basinList[rBegInd:rEndInd])
            stid2gageList <- rbind(stid2gageList[begInd:endInd],stid2gageList[rBegInd:rEndInd])
            mskgeo.areaList <- rbind(mskgeo.areaList[begInd:endInd],mskgeo.areaList[rBegInd:rEndInd])
            mskgeo.countInds <- rbind(mskgeo.countInds[begInd:endInd,],mskgeo.countInds[rBegInd:rEndInd,])
            mskgeo.List <- rbind(mskgeo.List[begInd:endInd],mskgeo.List[rBegInd:rEndInd])
            mskgeo.maxInds <- rbind(msgeo.maxInds[begInd:endInd,],mskgeo.maxInds[rBegInd:rEndInd,])
            mskgeo.minInds <- rbind(mskgeo.minInds[begInd:endInd,],mskgeo.minInds[rBegInd:rEndInd,])
            mskhyd.areaList <- rbind(mskhyd.areaList[begInd:endInd],mskhyd.areaList[rBegInd:rEndInd])
            mskhyd.countInds <- rbind(mskhyd.countInds[begInd:endInd,],mskhyd.countInds[rBegInd:rEndInd,])
            mskhyd.List <- rbind(mskhyd.List[begInd:endInd],mskhyd.List[rBegInd:rEndInd])
            mskhyd.maxInds <- rbind(mskhyd.maxInds[begInd:endInd],mskhyd.maxInds[rBegInd:rEndInd,])
            mskhyd.minInds <- rbind(mskhyd.minInds[begInd:endInd],mskhyd.minInds[rBegInd:rEndInd,])
            mskhyd.nameList <- rbind(mskhyd.nameList[begInd:endInd],mskhyd.nameList[rBegInd:rEndInd])
            mskgeo.nameList <- rbind(mskgeo.nameList[begInd:endInd],mskgeo.nameList[rBegInd:rEndInd])
            gageList <- rbind(gageList[begInd:endInd,],gageList[rBegInd,rEndInd,])
        }
    }
    # Return list
    return(list(frxstPts,basin2gageList,gage2basinList,
                stid2gageList,mskgeo.areaList,mskgeo.countInds,
                mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                mskhyd.areaList,mskhyd.countInds,mskhyd.List,
                mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList,
                mskgeo.nameList,gageList))
}

# Assign different regions amongst different processors
mpiRegions <- function(size,rank,mskgeo.areaList,mskgeo.countInds,
                       mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                       mskhyd.areaList,mskhyd.countInds,mskhyd.List,
                       mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList,
                       mskgeo.nameList){

    # Calculate total size of list. From there, split up portions
    # based on rank.
    masterLength <- length(mskgeo.List)
    localLength <- floor(masterLength/size)
    begInd <- localLength*rank + 1
    endInd <- localLength*(rank+1)
    if (rank != 0){
        localLength <- floor(masterLength/size)
        mskgeo.areaList <- mskgeo.areaList[begInd:endInd]
        mskgeo.countInds <- mskgeo.countInds[begInd:endInd,]
        mskgeo.List <- mskgeo.List[begInd:endInd,]
        mskgeo.maxInds <- mskgeo.maxInds[begInd:endInd,]
        mskgeo.minInds <- mskgeo.minInds[begInd:endInd,]
        mskhyd.areaList <- mskhyd.areaList[begInd:endInd]
        mskhyd.countInds <- mskhyd.countInds[begInd:endInd,]
        mskhyd.List <- mskhyd.List[begInd:endInd]
        mskhyd.maxInds <- mskhyd.maxInds[begInd:endInd,]
        mskhyd.minInds <- mskhyd.minInds[begInd:endInd,]
        mskhyd.nameList <- mskhyd.nameList[begInd:endInd]
        mskgeo.nameList <- mskgeo.nameList[begInd:endInd]
    } else {
        localLength <- floor(masterLength/size) 
        remainder <- masterLength - (localLength*size)
        if (remainder == 0){
            mskgeo.areaList <- mskgeo.areaList[begInd:endInd]
            mskgeo.countInds <- mskgeo.countInds[begInd:endInd,]
            mskgeo.List <- mskgeo.List[begInd:endInd,]
            mskgeo.maxInds <- mskgeo.maxInds[begInd:endInd,]
            mskgeo.minInds <- mskgeo.minInds[begInd:endInd,]
            mskhyd.areaList <- mskhyd.areaList[begInd:endInd]
            mskhyd.countInds <- mskhyd.countInds[begInd:endInd,]
            mskhyd.List <- mskhyd.List[begInd:endInd]
            mskhyd.maxInds <- mskhyd.maxInds[begInd:endInd,]
            mskhyd.minInds <- mskhyd.minInds[begInd:endInd,]
            mskhyd.nameList <- mskhyd.nameList[begInd:endInd]
            mskgeo.nameList <- mskgeo.nameList[begInd:endInd]
        } else {
            rBegInd <- size*localLength + 1
            rEndInd <- masterLength
            mskgeo.areaList <- rbind(mskgeo.areaList[begInd:endInd],mskgeo.areaList[rBegInd:rEndInd])
            mskgeo.countInds <- rbind(mskgeo.countInds[begInd:endInd,],mskgeo.countInds[rBegInd:rEndInd,])
            mskgeo.List <- rbind(mskgeo.List[begInd:endInd],mskgeo.List[rBegInd:rEndInd])
            mskgeo.maxInds <- rbind(msgeo.maxInds[begInd:endInd,],mskgeo.maxInds[rBegInd:rEndInd,])
            mskgeo.minInds <- rbind(mskgeo.minInds[begInd:endInd,],mskgeo.minInds[rBegInd:rEndInd,])
            mskhyd.areaList <- rbind(mskhyd.areaList[begInd:endInd],mskhyd.areaList[rBegInd:rEndInd])
            mskhyd.countInds <- rbind(mskhyd.countInds[begInd:endInd,],mskhyd.countInds[rBegInd:rEndInd,])
            mskhyd.List <- rbind(mskhyd.List[begInd:endInd],mskhyd.List[rBegInd:rEndInd])
            mskhyd.maxInds <- rbind(mskhyd.maxInds[begInd:endInd],mskhyd.maxInds[rBegInd:rEndInd,])
            mskhyd.minInds <- rbind(mskhyd.minInds[begInd:endInd],mskhyd.minInds[rBegInd:rEndInd,])
            mskhyd.nameList <- rbind(mskhyd.nameList[begInd:endInd],mskhyd.nameList[rBegInd:rEndInd])
            mskgeo.nameList <- rbind(mskgeo.nameList[begInd:endInd],mskgeo.nameList[rBegInd:rEndInd])
        }
    }
    # Return List
    return(list(mskgeo.areaList,mskgeo.countInds,mskgeo.List,
                mskgeo.maxInds,mskgeo.minInds,mskhyd.areaList,
                mskhyd.countInds,mskhyd.List,mskhyd.maxInds,
                mskhyd.minInds,mskhyd.nameList,mskgeo.nameList))
}

# Assign different points amongst different processors
mpiPts <- function(size,rank,ptgeo.sno){
    # Calculate total size of list. From there, split up portions
    # based on rank.
    masterLength <- length(ptgeo.sno$id)
    localLength <- floor(masterLength/size)
    begInd <- localLength*rank + 1
    endInd <- localLength*(rank+1)
    print(masterLength)
    print(localLength)
    print(begInd)
    print(endInd)
    if (rank != 0){
        localLength <- floor(masterLength/size)
        ptgeo.sno <- ptgeo.sno[begInd:endInd,]
    } else {
        localLength <- floor(masterLength/size)
        remainder <- masterLength - (localLength*size)
        if (remainder == 0){
            ptgeo.sno <- ptgeo.sno[begInd:endInd,]               
        } else {
            ptgeo.sno <- rbind(ptgeo.sno[begInd:endInd,],ptgeo.sno[rBegInd:endInd,])
        }
    }
    # Return List
    return(list(ptgeo.sno))
}

