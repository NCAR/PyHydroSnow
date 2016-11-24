# Generic utility routines used by R functions.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

# Assign different regions amongst different processors
mpiLandRegions <- function(size,rank,mskgeo.areaList,mskgeo.countInds,
                           mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
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
         mskgeo.nameList <- mskgeo.nameList[begInd:endInd]
      } else {
         rBegInd <- size*localLength + 1
         rEndInd <- masterLength
         mskgeo.areaList <- rbind(mskgeo.areaList[begInd:endInd],mskgeo.areaList[rBegInd:rEndInd])
         mskgeo.countInds <- rbind(mskgeo.countInds[begInd:endInd,],mskgeo.countInds[rBegInd:rEndInd,])
         mskgeo.List <- rbind(mskgeo.List[begInd:endInd],mskgeo.List[rBegInd:rEndInd])
         mskgeo.maxInds <- rbind(msgeo.maxInds[begInd:endInd,],mskgeo.maxInds[rBegInd:rEndInd,])
         mskgeo.minInds <- rbind(mskgeo.minInds[begInd:endInd,],mskgeo.minInds[rBegInd:rEndInd,])
         mskgeo.nameList <- rbind(mskgeo.nameList[begInd:endInd],mskgeo.nameList[rBegInd:rEndInd])
      }
   }
   # Return List
   return(list(mskgeo.areaList,mskgeo.countInds,mskgeo.List,
               mskgeo.maxInds,mskgeo.minInds,mskhyd.areaList,
               ,mskgeo.nameList))
}

# Calculate various basin snow metrics
basSnowMetrics <- function(sweVar,mskVar,basElev,res) {
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
   return(outList)
}
