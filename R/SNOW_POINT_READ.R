# Program to read in model snow values at point obseervation
# points given an observations file. User needs to run 
# the python utility to extract snow observations from
# the SQL database prior to running this program.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

# Load necessary libraries
library(rwrfhydro)

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

# Loop time period of interest. Pull model values for each daily output file. 
# In addition, check for observations for given day, if they exist, calculate
# an average for the 24 hour time period and use that as the "observed" value.

count <- 1
# SWE First.
for (day in 1:nSteps){
   dCurrent <- dateStart + dt*day
   print(dCurrent)
#   # Find all observations that fall on this day
#   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')
#   ind <- which(strftime(sweOut$POSIXct,'%Y-%m-%d',tz='UTC') == dStr1)
#   if(length(ind) == 0){
#      print(paste0('WARNING: No SWE Observations Found For: ',strftime(dCurrent,'%Y-%m-%d',tz='UTC')))
#      continue
#   }
#
#   # Subset observation database for this timestep
#   obsTmp <- sweOut[indTmp,]
#   uniqueTmp <- unique(obsTmp$uniqueId)
#
#   # Loop through unique stations found in this time step. Average observed values found during 
#   # time period are averaged. Modeled values are then pulled.
#   for (station in 1:length(uniqueTmp)){
#      indObs <- which(obsTmp$uniqueId == uniqueTmp[station])
#      sweOutPts$uniqueId[count] <- uniqueTmp[station]
#      sweOutPts$POSIXct[count] <- dCurrent
#      sweOutPts$value_mm[count] <- mean(obsTmp$obs_mm[indObs])
#      sweOutPts$tag[count] <- 'Obs'
#
#      # Pull meta data info for this station
#      indMeta <- which(metaOut$uniqueId == uniqueTmp[station])
#      latTmp <- metaOut$latitude[indMeta]
#      lonTmp <- metaOut$longitude[indMeta]
#
#      sweOutPts$lat[count] <- latTmp
#      sweOutPts$lon[count] <- lonTmp
#      count <- count + 1
#
#      # Loop through model groups to read in.
#      for (tag in 1:length(modTags)){
#         modTag <- modTags[tag]
#         tmpPath = modPaths[[tag]]
#         snowPath <- paste0(modPaths[[k]],"/",strftime(dCurrent,"%Y%m%d"),
#                            "00.LDASOUT_DOMAIN1")
#         id <- nc_open(snowPath)
#         sweModel <- ncvar_get(id,'SNEQV')
#         nc_close(id)
#
#         sweOutPts$uniqueId[count] <- uniqueTmp[station]
#         sweOutPts$POSIXct[count] <- dCurrent
#         sweOutPts$value_mm[count] <- sweMod[metaOut$iCoord[indMeta],metaOut$jCoord[indMeta]]
#         sweOutPts$tag[count] <- modTag
#         sweOutPts$lat[count] <- latTmp
#         sweOutPts$lon[count] <- lonTmp
#         count <- count + 1
#      } 
#   }
}

## Depth Second.
#count <- 1
#for (day in 1:nSteps){
#   dCurrent <- dateStart + dt*j
#
#   # Find all observations that fall on this day
#   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')
#   ind <- which(strftime(sdOut$POSIXct,'%Y-%m-%d',tz='UTC') == dStr1)
#   if(length(ind) == 0){
#      print(paste0('WARNING: No SWE Observations Found For: ',strftime(dCurrent,'%Y-%m-%d',tz='UTC')))
#      continue
#   }
#
#   # Subset observation database for this timestep
#   obsTmp <- sdOut[indTmp,]
#   uniqueTmp <- unique(obsTmp$uniqueId)
#
#   # Loop through unique stations found in this time step. Average observed values found during
#   # time period are averaged. Modeled values are then pulled.
#   for (station in 1:length(uniqueTmp)){
#      indObs <- which(obsTmp$uniqueId == uniqueTmp[station])
#      sdOutPts$uniqueId[count] <- uniqueTmp[station]
#      sdOutPts$POSIXct[count] <- dCurrent
#      sdOutPts$value_mm[count] <- mean(obsTmp$obs_mm[indObs])
#      sdOutPts$tag[count] <- 'Obs'
#
#      # Pull meta data info for this station
#      indMeta <- which(metaOut$uniqueId == uniqueTmp[station])
#      latTmp <- metaOut$latitude[indMeta]
#      lonTmp <- metaOut$longitude[indMeta]
#
#      sdOutPts$lat[count] <- latTmp
#      sdOutPts$lon[count] <- lonTmp
#      count <- count + 1
#
#      # Loop through model groups to read in.
#      for (tag in 1:length(modTags)){
#         modTag <- modTags[tag]
#         tmpPath = modPaths[[tag]]
#         snowPath <- paste0(modPaths[[k]],"/",strftime(dCurrent,"%Y%m%d"),
#                            "00.LDASOUT_DOMAIN1")
#         id <- nc_open(snowPath)
#         sdModel <- ncvar_get(id,'SNEQV')
#         nc_close(id)
#
#         sdOutPts$uniqueId[count] <- uniqueTmp[station]
#         sdOutPts$POSIXct[count] <- dCurrent
#         sdOutPts$value_mm[count] <- sdMod[metaOut$iCoord[indMeta],metaOut$jCoord[indMeta]]
#         sdOutPts$tag[count] <- modTag
#         sdOutPts$lat[count] <- latTmp
#         sdOutPts$lon[count] <- lonTmp
#         count <- count + 1
#      }
#   }
#}

## Subset data frames to exclude any missing values
#sweOutPts <- subset(sweOutPts,!is.na(sweOutPts$value_mm))
#sdOutPts <- subset(sdOutPts,!is.na(sdOutPts$value_mm))

## Save output
#save(sweOut,sdOut,file=outFile)
