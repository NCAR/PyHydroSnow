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

# Open point observations file and use meta dataframe
# to extract i/j coordinates for each point. These will
# be used to extract gridded snow values.
load(ptObsFile)

# Loop through each obs point and calculate i/j coordinates
# using geogrid file. 
iMod <- vector()
jMod <- vector()

dfCoord <- GetGeogridIndex(data.frame(lon=metaOut$longitude,lat=metaOut$latitude),
                           geoFile)

# Add coordinates to meta dataframe
metaOut[['iCoord']] <- dfCoord$ew
metaOut[['jCoord']] <- dfCoord$sn

# Subset coordinates to remove NA values that fall outside of domain.
metaOut <- subset(metaOut,!is.na(metaOut$iCoord))

# Calculate total number of observations based on observations file and number of model
# groups.
numPossSwePts <- length(sweOut$obs_mm)*length(modTags)
numPossSdPts <- length(sdOut$obs_mm)*length(modTags)

# Create output dataframes
sweOutPts <- data.frame(matrix(NA,ncol=6,nrow=numPossSwePts))
sdOutPts <- data.frame(matrix(NA,ncol=6,nrow=numPossSdPts))

names(sweOutPts) <- c('uniqueId','lat','lon','POSIXct','value_mm','tag')
names(sdOutPts) <- c('uniqueId','lat','lon','POSIXct','value_mm','tag')

sweOutPts$POSIXct <- as.Date(as.POSIXct('1900-01-01'),tz='UTC')
sdOutPts$POSIXct <- as.Date(as.POSIXct('1900-01-01'),tz='UTC')

# Perform two loops. First will loop through each day and pull model/SNODAS output for each
# observation station. Second loop will loop through each observation station, subset 
# observations based on station ID, and then loop through each time step to pull out 
# a daily average value. 

count <- 1
# SWE First.
for (day in 1:nSteps){
   dCurrent <- dateStart + dt*day
   print(dCurrent)
   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

   # Create list of ID values 
   uniqueTmp <- unique(metaOut$uniqueId)
   
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
      if(tag == 1){
         sweModel <- tmpModel
         nCol <- dim(sweModel)[1]
         nRow <- dim(sweModel)[2]
      } else {
         sweModel <- cbind(sweModel,tmpModel)
      }
   }
   sweModel <- array(sweModel,dim=c(nCol,nRow,length(modTags)))

   # Loop through each observation station and pull values based on I/J coordinates calculated earlier. 
   for (station in 1:length(uniqueTmp)){
      latTmp <- metaOut$latitude[station]
      lonTmp <- metaOut$longitude[station]

      # Loop through model groups to read in.
      for (tag in 1:length(modTags)){
         modTag <- modTags[tag]
         sweOutPts$uniqueId[count] <- uniqueTmp[station]
         sweOutPts$POSIXct[count] <- dCurrent
         sweOutPts$value_mm[count] <- sweModel[metaOut$iCoord[station],metaOut$jCoord[station],tag]
         sweOutPts$tag[count] <- modTag
         sweOutPts$lat[count] <- latTmp
         sweOutPts$lon[count] <- lonTmp
         count <- count + 1
      } 
   }
}

# Second loop to read in and average daily observed values. 
uniqueTmp <- unique(metaOut$uniqueId)
for (station in 1:length(uniqueTmp)){
   # Subset observations based on ID.
   obsTmp <- sweOut[uniqueId == uniqueTmp[station]]

   #count <- 1 
   for (day in 1:nSteps){
      if(day == 1){
         print(station)
      }
      dCurrent <- dateStart + dt*day
      dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

      sweOutPts$uniqueId[count] <- uniqueTmp[station]
      sweOutPts$POSIXct[count] <- dCurrent
      sweOutPts$value_mm[count] <- mean(obsTmp[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1]$obs_mm)
      sweOutPts$tag[count] <- 'Obs'

      latTmp <- metaOut$latitude[station]
      lonTmp <- metaOut$longitude[station]

      sweOutPts$lat[count] <- latTmp
      sweOutPts$lon[count] <- lonTmp
      count <- count + 1
   }

}

# Snow Depth Next.
count <- 1
for (day in 1:nSteps){
   dCurrent <- dateStart + dt*day
   print(dCurrent)
   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

   # Create list of ID values
   uniqueTmp <- unique(metaOut$uniqueId)

   # Read in model output. If multiple model directories, stack each model output
   # array ontop of each other to create a 3D array for referencing.
   for (tag in 1:length(modTags)){
      modTag <- modTags[tag]
      tmpPath <- modPaths[[tag]]
      snowPath <- paste0(modPaths[[tag]],"/",strftime(dCurrent,"%Y%m%d"),
                         "00.LDASOUT_DOMAIN1")
      id <- nc_open(snowPath)
      tmpModel <- ncvar_get(id,'SNOWH')
      nc_close(id)
      if(tag == 1){
         sdModel <- tmpModel
         nCol <- dim(sdModel)[1]
         nRow <- dim(sdModel)[2]
      } else {
         sdModel <- cbind(sdModel,tmpModel)
      }
   }
   sdModel <- array(sdModel,dim=c(nCol,nRow,length(modTags)))

   # Loop through each observation station and pull values based on I/J coordinates calculated earlier. 
   for (station in 1:length(uniqueTmp)){
      latTmp <- metaOut$latitude[station]
      lonTmp <- metaOut$longitude[station]

      # Loop through model groups to read in.
      for (tag in 1:length(modTags)){
         modTag <- modTags[tag]
         sdOutPts$uniqueId[count] <- uniqueTmp[station]
         sdOutPts$POSIXct[count] <- dCurrent
         sdOutPts$value_mm[count] <- sdModel[metaOut$iCoord[station],metaOut$jCoord[station],tag]
         sdOutPts$tag[count] <- modTag
         sdOutPts$lat[count] <- latTmp
         sdOutPts$lon[count] <- lonTmp
         count <- count + 1
      }
   }
}

# Second loop to read in and average daily observed values.
uniqueTmp <- unique(metaOut$uniqueId)
for (station in 1:length(uniqueTmp)){
   # Subset observations based on ID.
   obsTmp <- sdOut[uniqueId == uniqueTmp[station]]

   #count <- 1
   for (day in 1:nSteps){
      if(day == 1){
         print(station)
      }
      dCurrent <- dateStart + dt*day
      dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

      sdOutPts$uniqueId[count] <- uniqueTmp[station]
      sdOutPts$POSIXct[count] <- dCurrent
      sdOutPts$value_mm[count] <- mean(obsTmp[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1]$obs_mm)
      sdOutPts$tag[count] <- 'Obs'

      latTmp <- metaOut$latitude[station]
      lonTmp <- metaOut$longitude[station]

      sdOutPts$lat[count] <- latTmp
      sdOutPts$lon[count] <- lonTmp
      count <- count + 1
   }

}

# Subset data frames to exclude any missing values
sweOutPts <- subset(sweOutPts,!is.na(sweOutPts$value_mm))
sdOutPts <- subset(sdOutPts,!is.na(sdOutPts$value_mm))

# Save output
save(sweOutPts,sdOutPts,file=outFile)
