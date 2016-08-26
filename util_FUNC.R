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

# Subset basin mask information and update data frames/lists appropriately.
subsetBasins <- function(basinSub,mskgeo.nameList,frxstPts,basin2gageList,gage2basinList,
                         mskgeo.areaList, mskgeo.countInds, mskgeo.List, mskgeo.maxInds,
     			 mskgeo.minInds, mskhyd.areaList, mskhyd.countInds,
     			 mskhyd.List, mskhyd.maxInds, mskhyd.minInds, 
     			 mskhyd.nameList,stid2gageList){

  ind <- c()
  for (basin in basinSub$basin){
    i <- which(mskgeo.nameList == basin)
    ind <- append(ind,i)
  }

  # Subset lists/data frames
  mskgeo.nameList <- mskgeo.nameList[ind]
  frxstPts <- frxstPts[ind,]
  gage2basinList <- gage2basinList[ind]
  basin2gageList <- basin2gageList[ind]
  mskgeo.areaList <- mskgeo.areaList[ind]
  mskgeo.countInds <- mskgeo.countInds[ind,]
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

subsetRegions <- function(basinSub,mskgeo.nameList,basin2gageList,
                         mskgeo.areaList, mskgeo.countInds, mskgeo.List, mskgeo.maxInds,
                         mskgeo.minInds){

  ind <- c()
  for (basin in basinSub$basin){
    i <- which(mskgeo.nameList == basin)
    ind <- append(ind,i)
  }

  # Subset lists/data frames
  mskgeo.nameList <- mskgeo.nameList[ind]
  basin2gageList <- basin2gageList[ind]
  mskgeo.areaList <- mskgeo.areaList[ind]
  mskgeo.countInds <- mskgeo.countInds[ind,]
  mskgeo.List <- mskgeo.List[ind]
  mskgeo.maxInds <- mskgeo.maxInds[ind,]
  mskgeo.minInds <- mskgeo.minInds[ind,]

  return(list(mskgeo.nameList,basin2gageList,
              mskgeo.areaList,mskgeo.countInds,mskgeo.List,mskgeo.maxInds,
              mskgeo.minInds))
} 
