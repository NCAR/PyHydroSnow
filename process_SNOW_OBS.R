# Process snow observations that were extracted by the SQL database.

# Logan Karsten 
# National Center for Atmospheric Research
# Research Applications Laboratory

# Program will read in NetCDF file containing hourly SWE/Depth observations.
# Along with lat/lon information for each unique ID value. If a mask 
# file is provided, associated regions each unique station falls within
# is calculated. 

# Load necessary libraries
library(ncdf4)
library(rwrfhydro)

basinFlag <- 0
# Process command line arguments.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 3){
  stop('ERROR: Maximum of two arguments allowed to snow observations processing')
}
if (length(args) == 3){
  basinFlag <- 1
  maskFile <- args[3]
}
inFile <- args[1]
geoFile <- args[2]

print(inFile)
print(geoFile)
# Create output file based on input file.
inSplit = strsplit(inFile,'[.]')[[1]]
if (length(inSplit) != 2){
  stop(paste0('ERROR: Unexpected file formate: ',inFile))
}
outFile <- paste0(inSplit[1],'.Rdata')

print(outFile)
# Load in mask file if present.
if (basinFlag == 1){
  load(maskFile)
  print(maskFile)
}

# Open input NetCDF file containing extracted observations.
id <- nc_open(inFile)

# Extract number of observations
numSdObs <- ncatt_get(id,'sdObs','numObs')
numSweObs <- ncatt_get(id,'sweObs','numObs')
numSdObs <- numSdObs$value
numSweObs <- numSweObs$value

# Pull data
# All stations that were found for entire record for given
# desired networks.
uniqueStationsAll <- ncvar_get(id,'ptUniqueIds')
uniqueStationsLat <- ncvar_get(id,'ptLatitude')
uniqueStationsLon <- ncvar_get(id,'ptLongitude')
# SWE/Depth observations for desired period.
if (numSweObs != 0){
  sweObs <- ncvar_get(id,'sweObs')
  # SWE/Depth observation dates (hours since 1970-01-01 00:00:00)
  sweObsDate <- ncvar_get(id,'sweObsDates')
  # Associated unique ID from database for each observation.
  sweObsIds <- ncvar_get(id,'sweObsIds')
}
if (numSdObs != 0){
  sdObs <- ncvar_get(id,'sdObs')
  # SWE/Depth observation dates (hours since 1970-01-01 00:00:00)
  sdObsDate <- ncvar_get(id,'sdObsDates')
  # Associated unique ID from database for each observation.
  sdObsIds <- ncvar_get(id,'sdObsIds')
}

# Calculate x/y coordinates on modeling domain using point lat/lon
# coordinates and geogrid file.
geoCoords <- GetGeogridIndex(data.frame(lat=uniqueStationsLat,lon=uniqueStationsLon),geoFile)

print(geoCoords$ew[1])
# Create output dataframe
sweOut <- data.frame(matrix(NA,nrow=numSweObs,ncol=4))
sdOut <- data.frame(matrix(NA,nrow=numSdObs,ncol=4))
metaOut <- data.frame(matrix(NA,nrow=length(uniqueStationsAll),ncol=4))

print('NAMING DF')
names(metaOut) <- c("uniqueId","latitude","longitude","region")
names(sweOut) <- c("uniqueId","obs_mm","POSIXct","region")
names(sdOut) <- c("uniqueId","obs_mm","POSIXct","region")
if (numSweObs != 0){
  sweOut$POSIXct <- as.POSIXct('1900-01-01 00:00:00','%Y-%m-%d %H:%M:%S')
}
if (numSdObs != 0){
  sdOut$POSIXct <- as.POSIXct('1900-01-01 00:00:00','%Y-%m-%d %H:%M:%S')
}

print('PLACING METADATA INTO DF')
# Place unique stations into output meta data frame.
metaOut$uniqueId <- uniqueStationsAll
metaOut$latitude <- uniqueStationsLat
metaOut$longitude <- uniqueStationsLon

print('LOOPING THROUGH BASINS')
# If basin subsetting, loop through from mask file 
# and assign value to each unique station based on lat/lon
# information.
if (basinFlag == 1){
  for (basin in 1:length(mskgeo.nameList)){
    bName <- mskgeo.nameList[[basin]]

    minX <- mskgeo.minInds$x[basin]
    maxX <- mskgeo.maxInds$x[basin]
    minY <- mskgeo.minInds$y[basin]
    maxY <- mskgeo.maxInds$y[basin]
    print('XXXXXXXXX')
    print(minX)
    print(maxX)
    print(minY)
    print(maxY)

    print(bName)
    # Loop through points and determine if they fall within this region.
    for (point in 1:length(uniqueStationsAll)){
      xCoord <- geoCoords$ew[point]
      yCoord <- geoCoords$sn[point]

      if ((point == 1) && (basin == 1)){
        print(paste0('XCOORD = ',xCoord))
        print(paste0('YCOORD = ',yCoord))
      }
      if(is.na(xCoord)) next
      if(is.na(yCoord)) next # These are points outside the modeling domain

      # Check to see if within bounding box of region.
      if ((xCoord >= minX) && (xCoord <= maxX) &&
          (yCoord >= minY) && (yCoord <= maxY)){

        print(paste0('FOUND POINT IN BASIN: ',bname,' ID = ',metaOut$uniqueId))		
        # Next, calculate local bounding box coordinates.
        localX <- xCoord - minX + 1
        localY <- yCoord - minY + 1

        # Putting threshold of 0.75 for fraction of pixel cell covered by 
        # region for it to be classified.
        if (mskgeo.List[[basin]][localX,localY] > 0.75){
          metaOut$region[point] <- bName				
        }
      }
    }
  }
}

print('PLACING OBS INTO DATAFRAME')
# Loop through observations pulled and place into output dataframe. In additin,
# find region it falls within based on metadata frame
# SWE First
for (point in 1:numSweObs){
	if(numSweObs == 0) next
	sweOut$obs_mm[point] <- sweObs[point]
	sweOut$uniqueId[point] <- sweObsIds[point]
	sweOut$POSIXct[point] <- as.POSIXct(sweObsDate[point]*3600.0,origin="1970-01-01",tz="UTC") 
	if (basinFlag == 1){
		#print('-----------')
		#print(sweObsIds[point])
		#print('-----------')
		indTmp <- which(metaOut$uniqueId == sweObsIds[point])
		#print(indTmp)
		sweOut$region[point] <- metaOut$region[indTmp]
		#print('b')
	}
}

# Depth next
for (point in 1:numSdObs){
	if(numSdObs == 0) next
	sdOut$obs_mm[point] <- sdObs[point]
	sdOut$uniqueId[point] <- sdObsIds[point]
	sdOut$POSIXct[point] <- as.POSIXct(sdObsDate[point]*3600.0,origin="1970-01-01",tz="UTC")
	if (basinFlag == 1){
		indTmp <- which(metaOut$uniqueId == sdObsIds[point])
		#print('--------------')
		#print(sdObsIds[point])
		#print('--------------')
		#print(indTmp)
		sdOut$region[point] <- metaOut$region[indTmp]
	}
}

print('SAVING OUTPUT')
# Save output
save(sweOut,sdOut,metaOut,file=outFile)
