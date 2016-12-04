# Program to read in model snow values at point obseervation
# points given an observations file. User needs to run 
# the python utility to extract snow observations from
# the SQL database prior to running this program.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

# Load necessary libraries
library(rwrfhydro)
library(data.table)
library(ncdf4)
 
# Process command line arguments.
args <- commandArgs(trailingOnly = TRUE)
sourceFile <- args[1]

# Source temporary R file. This will load up options necessary
# to run analysis.
source(sourceFile)

# Source utility file
source('./R/UTILS.R')

# Check for existence of output file.
if(file.exists(outFile)){
   stop(paste0('ERROR: ',outFile,' Alread exists'))
}

# Establish time information
dUnits <- "days"
diff <- difftime(dateEnd,dateStart,units=dUnits)
nSteps <- diff <- as.numeric(diff)
dt <- 24*3600

# Open geogrid file and get number of rows,columns for indexing purposes.
id <- nc_open(geoFile)
hght <- ncvar_get(id,'HGT_M')
nRowMod <- dim(hght)[2]
nColMod <- dim(hght)[1]
rm(hght)
nc_close(id)

# Open point observations file and use meta dataframe
# to extract i/j coordinates for each point. These will
# be used to extract gridded snow values.
load(ptObsFile)

print(nRowMod)
print(nColMod)
dfCoord <- GetGeogridIndex(data.frame(lon=metaOut$longitude,lat=metaOut$latitude),
                           geoFile)

# Add coordinates to meta dataframe
metaOut[['iCoord']] <- dfCoord$ew
metaOut[['jCoord']] <- dfCoord$sn
metaOut[['kCoord']] <- (nRowMod*(dfCoord$ew-1)) + dfCoord$sn

# Loop through observations and assign kCoord to each entry. This will be used 
# when extracting gridded output.
sweOut[['kCoord']] <- -99
sdOut[['kCoord']] <- -99
sweOut[['latitude']] <- -99.0
sdOut[['latitude']] <- -99.0
sweOut[['longitude']] <- -99.0
sdOut[['longitude']] <- -99.0

uniqueSwePts <- unique(sweOut$uniqueId)
uniqueSdPts <- unique(sdOut$uniqueId)
print('Placing K,Lat,Lon into SWE Obs DF')
for(i in 1:length(uniqueSwePts)){
   idTmp <- uniqueSwePts[i]
   sweOut[uniqueId == idTmp]$kCoord <- metaOut[uniqueId == idTmp]$kCoord
   sweOut[uniqueId == idTmp]$latitude <- metaOut[uniqueId == idTmp]$latitude
   sweOut[uniqueId == idTmp]$longitude <- metaOut[uniqueId == idTmp]$longitude
}
print('Placing K,Lat,Lon into SD Obs DF')
for(i in 1:length(uniqueSdPts)){
   idTmp <- uniqueSdPts[i]
   sdOut[uniqueId == idTmp]$kCoord <- metaOut[uniqueId == idTmp]$kCoord
   sdOut[uniqueId == idTmp]$latitude <- metaOut[uniqueId == idTmp]$latitude
   sdOut[uniqueId == idTmp]$longitude <- metaOut[uniqueId == idTmp]$longitude
}
# Subset missing kCoord values as these points fall outside the modeling domain.
sweOut <- subset(sweOut,kCoord > 0)
sdOut <- subset(sdOut,kCoord > 0)
sweOut <- subset(sweOut,!is.na(kCoord))
sdOut <- subset(sdOut,!is.na(kCoord))

# truncate hourly observations to a daily mean.
print('Truncating Dates from Observations')
sweDatesTmp <- CalcDateTrunc(sweOut$POSIXct)
sdDatesTmp <- CalcDateTrunc(sdOut$POSIXct)
sweOut$POSIXct[] <- sweDatesTmp
sdOut$POSIXct[] <- sdDatesTmp
sweOut <- sweOut[, .(obs_mm=mean(obs_mm)), by=.(uniqueId,POSIXct,region,kCoord,latitude,longitude)]
sdOut <- sdOut[, .(obs_mm=mean(obs_mm)), by=.(uniqueId,POSIXct,region,kCoord,latitude,longitude)]

#print(as.data.frame(sweOut[uniqueId == 8449]))
# Calculate total number of observations based on observations file and number of model
# groups.
numPossSwePts <- length(sweOut$obs_mm)
numPossSdPts <- length(sdOut$obs_mm)

# Create output dataframes
print(numPossSwePts*(length(modTags)+2))
sweOutPts <- data.frame(matrix(NA,ncol=8,nrow=(numPossSwePts*(length(modTags)+2))))
sdOutPts <- data.frame(matrix(NA,ncol=8,nrow=(numPossSdPts*(length(modTags)+2))))

names(sweOutPts) <- c('uniqueId','lat','lon','region','POSIXct','value_mm','tag','kCoord')
names(sdOutPts) <- c('uniqueId','lat','lon','region','POSIXct','value_mm','tag','kCoord')

sweOutPts$POSIXct <- as.Date(as.POSIXct('1900-01-01'),tz='UTC')
sdOutPts$POSIXct <- as.Date(as.POSIXct('1900-01-01'),tz='UTC')

# Place observations, meta data into data frame to pre-populate everything except model/SNODAS
# values.
print(numPossSwePts)
sweOutPts$uniqueId[1:numPossSwePts] <- sweOut$uniqueId[1:numPossSwePts]
sweOutPts$lat[1:numPossSwePts] <- sweOut$latitude[1:numPossSwePts]
sweOutPts$lon[1:numPossSwePts] <- sweOut$longitude[1:numPossSwePts]
sweOutPts$region[1:numPossSwePts] <- sweOut$region[1:numPossSwePts]
sweOutPts$POSIXct[1:numPossSwePts] <- sweOut$POSIXct[1:numPossSwePts]
sweOutPts$value_mm[1:numPossSwePts] <- sweOut$obs_mm[1:numPossSwePts]
sweOutPts$tag[1:numPossSwePts] <- 'Obs'
sweOutPts$kCoord[1:numPossSwePts] <- sweOut$kCoord[1:numPossSwePts]
for(i in 1:length(modTags)){
   print(modTags[i])
   bInd <- numPossSwePts*i + 1
   eInd <- numPossSwePts*(i+1)
   print(bInd)
   print(eInd)
   sweOutPts$uniqueId[bInd:eInd] <- sweOut$uniqueId[1:numPossSwePts]
   sweOutPts$lat[bInd:eInd] <- sweOut$latitude[1:numPossSwePts]
   sweOutPts$lon[bInd:eInd] <- sweOut$longitude[1:numPossSwePts]
   sweOutPts$region[bInd:eInd] <- sweOut$region[1:numPossSwePts]
   sweOutPts$POSIXct[bInd:eInd] <- sweOut$POSIXct[1:numPossSwePts]
   sweOutPts$tag[bInd:eInd] <- modTags[i]
   sweOutPts$kCoord[bInd:eInd] <- sweOut$kCoord[1:numPossSwePts]
}
# Handle SNODAS data
bInd <- numPossSwePts*(length(modTags)+1)+1
eInd <- numPossSwePts*(length(modTags)+2)
print(bInd)
print(eInd)
sweOutPts$uniqueId[bInd:eInd] <- sweOut$uniqueId[1:numPossSwePts]
sweOutPts$lat[bInd:eInd] <- sweOut$latitude[1:numPossSwePts]
sweOutPts$lon[bInd:eInd] <- sweOut$longitude[1:numPossSwePts]
sweOutPts$region[bInd:eInd] <- sweOut$region[1:numPossSwePts]
sweOutPts$POSIXct[bInd:eInd] <- sweOut$POSIXct[1:numPossSwePts]
sweOutPts$tag[bInd:eInd] <- 'SNODAS'
sweOutPts$kCoord[bInd:eInd] <- sweOut$kCoord[1:numPossSwePts]

## Fill out snow depth data frame
sdOutPts$uniqueId[1:numPossSdPts] <- sdOut$uniqueId[1:numPossSdPts]
sdOutPts$lat[1:numPossSdPts] <- sdOut$latitude[1:numPossSdPts]
sdOutPts$lon[1:numPossSdPts] <- sdOut$longitude[1:numPossSdPts]
sdOutPts$region[1:numPossSdPts] <- sdOut$region[1:numPossSdPts]
sdOutPts$POSIXct[1:numPossSdPts] <- sdOut$POSIXct[1:numPossSdPts]
sdOutPts$value_mm[1:numPossSdPts] <- sdOut$obs_mm[1:numPossSdPts]
sdOutPts$tag[1:numPossSdPts] <- 'Obs'
sdOutPts$kCoord[1:numPossSdPts] <- sdOut$kCoord[1:numPossSdPts]
for(i in 1:length(modTags)){
   print(modTags[i])
   bInd <- numPossSdPts*i + 1
   eInd <- numPossSdPts*(i+1)
   print(bInd)
   print(eInd)
   sdOutPts$uniqueId[bInd:eInd] <- sdOut$uniqueId[1:numPossSdPts]
   sdOutPts$lat[bInd:eInd] <- sdOut$latitude[1:numPossSdPts]
   sdOutPts$lon[bInd:eInd] <- sdOut$longitude[1:numPossSdPts]
   sdOutPts$region[bInd:eInd] <- sdOut$region[1:numPossSdPts]
   sdOutPts$POSIXct[bInd:eInd] <- sdOut$POSIXct[1:numPossSdPts]
   sdOutPts$tag[bInd:eInd] <- modTags[i]
   sdOutPts$kCoord[bInd:eInd] <- sdOut$kCoord[1:numPossSdPts]
}
# Handle SNODAS data
bInd <- numPossSdPts*(length(modTags)+1)+1
eInd <- numPossSdPts*(length(modTags)+2)
print(bInd)
print(eInd)
sdOutPts$uniqueId[bInd:eInd] <- sdOut$uniqueId[1:numPossSdPts]
sdOutPts$lat[bInd:eInd] <- sdOut$latitude[1:numPossSdPts]
sdOutPts$lon[bInd:eInd] <- sdOut$longitude[1:numPossSdPts]
sdOutPts$region[bInd:eInd] <- sdOut$region[1:numPossSdPts]
sdOutPts$POSIXct[bInd:eInd] <- sdOut$POSIXct[1:numPossSdPts]
sdOutPts$tag[bInd:eInd] <- 'SNODAS'
sdOutPts$kCoord[bInd:eInd] <- sdOut$kCoord[1:numPossSdPts]

# Subset any values that fall outside the date range
sweOutPts <- subset(sweOutPts,as.POSIXct(POSIXct,'%Y-%m-%d %H:%M:%S',tz='UTC') >= dateStart &
                    as.POSIXct(POSIXct,'%Y-%m-%d %H:%M:%S',tz='UTC') <= dateEnd)
sdOutPts <- subset(sdOutPts,as.POSIXct(POSIXct,'%Y-%m-%d %H:%M:%S',tz='UTC') >= dateStart &
                    as.POSIXct(POSIXct,'%Y-%m-%d %H:%M:%S',tz='UTC') <= dateEnd)

# Convert to data table
sweOutPts <- as.data.table(sweOutPts)
sdOutPts <- as.data.table(sdOutPts)

# Loop through each day in the time period of analysis. Read in model/SNODAS grids,
# then use kCoord values for each data table to extract all obs for that time.
# SWE First.
for (day in 1:nSteps){
   dCurrent <- dateStart + dt*day
   print(dCurrent)
   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

   # Read in model output. If multiple model directories, stack each model output
   # array ontop of each other to create a 3D array for referencing. 
   for (tag in 1:length(modTags)){
      modTag <- modTags[tag]
      tmpPath <- modPaths[[tag]]
      snowPath <- paste0(modPaths[[tag]],"/",strftime(dCurrent,"%Y%m%d"),
                         "00.LDASOUT_DOMAIN1")
      id <- nc_open(snowPath)
      tmpModel <- ncvar_get(id,'SNEQV')
      nc_close(id)
      # Extract kCoord values for this particular time step
      kCoordsTmp <- sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == modTag]$kCoord
      # Pull values for these coordinates out of file
      modelValuesTmp <- tmpModel[kCoordsTmp]
      # Place into data table
      #sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & modTag == modTag]$value_mm <- modelValuesTmp
   }

   # Read in SNODAS data
   snodasFilePath <- paste0(snodasPath,"/SNODAS_REGRIDDED_",
                            strftime(dCurrent,"%Y%m%d"),".nc")
   id <- nc_open(snodasFilePath)
   sweSnodas <- ncvar_get(id,'SNEQV')
   nc_close(id)
   # Extract kCoord values for this particular time step 
   kCoordsTmp <- sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == 'SNODAS']$kCoord
   # Pull values for these coordinates out of file
   modelValuesTmp <- tmpModel[kCoordsTmp]
   print(modelValuesTmp)
   # Place into data table
   #sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & modTag == 'SNODAS']$value_mm <- modelValuesTmp
}

# Snow Depth Next.
for (day in 1:nSteps){
   dCurrent <- dateStart + dt*day
   print(dCurrent)
   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

   for (tag in 1:length(modTags)){
      modTag <- modTags[tag]
      tmpPath <- modPaths[[tag]]
      snowPath <- paste0(modPaths[[tag]],"/",strftime(dCurrent,"%Y%m%d"),
                         "00.LDASOUT_DOMAIN1")
      id <- nc_open(snowPath)
      tmpModel <- ncvar_get(id,'SNOWH')
      nc_close(id)
      # Extract kCoord values for this particular time step 
      kCoordsTmp <- sdOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == modTag]$kCoord
      # Pull values for these coordinates out of file
      modelValuesTmp <- tmpModel[kCoordsTmp]
      # Place into data table
      #sdOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & modTag == modTag]$value_mm <- modelValuesTmp
   }

   # Read in SNODAS data
   snodasFilePath <- paste0(snodasPath,"/SNODAS_REGRIDDED_",
                            strftime(dCurrent,"%Y%m%d"),".nc")
   id <- nc_open(snodasFilePath)
   sdSnodas <- ncvar_get(id,'SNOWH')
   nc_close(id)
   # Extract kCoord values for this particular time step
   kCoordsTmp <- sdOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == 'SNODAS']$kCoord
   # Pull values for these coordinates out of file
   modelValuesTmp <- tmpModel[kCoordsTmp]
   # Place into data table
   #sdOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & modTag == 'SNODAS']$value_mm <- modelValuesTmp
}

# Subset data frames to exclude any missing values
#sweOutPts <- subset(sweOutPts,!is.na(sweOutPts$value_mm))
#sdOutPts <- subset(sdOutPts,!is.na(sdOutPts$value_mm))

# Save output
save(sweOutPts,sdOutPts,file=outFile)
