# R script to read in model + SNODAS SWE fields aggregated to basins/regions
# based on the user provided mask file.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

# Load necessary libraries
library(ncdf4)

# Process command line arguments.
args <- commandArgs(trailingOnly = TRUE)
sourceFile <- args[1]

# Source temporary R file. This will load up options necessary
# to run analysis.
source(sourceFile)

# Source utility file
source('./R/UTILS.R')

print(modPaths[1])
print(geoFile)
# Open geogrid file
idGeo <- nc_open(geoFile)

resKM <- (ncatt_get(idGeo,0,'DX')$value)/1000.0

print(resKM)
# Check for existence of output file.
if(file.exists(outFile)){
   stop(paste0('ERROR: ',outFile,' Alread exists'))
}

# Load mask file
load(bsnMskFile)

# If reads are being ran across multiple processors, split regions based 
# on number of processors.
if (size > 1){
   listMpi <- mpiLandRegions(size,rank,mskgeo.areaList,mskgeo.countInds,
                             mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                             mskgeo.nameList)
   mskgeo.areaList <- listMpi[[1]]
   mskgeo.countInds <- listMpi[[2]]
   mskgeo.List <- listMpi[[3]]
   mskgeo.maxInds <- listMpi[[4]]
   mskgeo.minInds <- listMpi[[5]]
   mskgeo.nameList <- listMpi[[6]]
}

# Establish time information
dUnits <- "days"
diff <- difftime(dateEnd,dateStart,units=dUnits)
nSteps <- diff <- as.numeric(diff)
dt <- 24*3600

# Create output data frame that will hold data
snowBasinData <- data.frame(matrix(NA,ncol=17,nrow=(nSteps*length(mskgeo.nameList)*(length(modPaths)+1))))
names(snowBasinData) <- c("Basin","Date","basin_area_km","product","snow_area_km","snow_cover_fraction",
                        "mean_snow_line_meters","mean_snow_line_feet","snow_volume_cub_meters",
                        "snow_volume_acre_feet","mean_swe_mm","mean_depth_mm","max_depth_mm",
                        "max_swe_mm","min_rho_kgm3","max_rho_kgm3","mean_rho_kgm3")
snowBasinData$Basin <- NA
snowBasinData$Date <- as.Date(as.POSIXct('1900-01-01'),'GMT')
snowBasinData$basin_area_km <- NA
snowBasinData$product <- NA
snowBasinData$snow_area_km <- NA
snowBasinData$snow_cover_fraction <- NA
snowBasinData$mean_snow_line_meters <- NA
snowBasinData$mean_snow_line_feet <- NA
snowBasinData$snow_volume_cub_meters <- NA
snowBasinData$snow_volume_acre_feet <- NA
snowBasinData$mean_swe_mm <- NA
snowBasinData$mean_depth_mm <- NA
snowBasinData$max_depth_mm <- NA
snowBasinData$max_swe_mm <- NA
snowBasinData$min_rho_kgm3 <- NA
snowBasinData$max_rho_kgm3 <- NA
snowBasinData$mean_rho_kgm3 <- NA

count = 1

# Loop through basins and calculate SNODAS/model statistics
for (i in 1:length(mskgeo.nameList)) {
   bName <- mskgeo.nameList[[i]]
   bStart <- c(mskgeo.minInds$x[i],mskgeo.minInds$y[i],1)
   bEnd <- c(mskgeo.maxInds$x[i]+1,mskgeo.maxInds$y[i]+1,2)
   bCount <- bEnd - bStart

   message(paste0('Processing Basin: ',bName))
   # Extract elevation data for basin
   basElev <- ncvar_get(idGeo,'HGT_M',start=bStart,count=bCount)

   mskVar <- mskgeo.List[[i]]

   # Loop through days and peform analysis
   for (j in 1:nSteps){
      dCurrent <- dateStart + dt*j
		
      message(paste0('Processing: ',dCurrent))
      count = count + 1
      # Model data
      for(k in 1:length(modPathList)) {
         modoutTag <- modTags[k]
         tmpPath = modPaths[[k]]
         snowPath <- paste0(modPaths[[k]],"/",strftime(dCurrent,"%Y%m%d"),
			    "00.LDASOUT_DOMAIN1")
         id <- nc_open(snowPath)
         sweModel <- ncvar_get(id,'SNEQV',start=bStart,count=bCount)
         nc_close(id)

         statsTemp <- basSnowMetrics(sweModel,mskVar,basElev,res=resKM)
         snowBasinData$Basin[count] <- bName
	 snowBasinData$Date[count] <- dCurrent
         snowBasinData$product[count] <- modoutTag
         snowBasinData$basin_area_km[count] <- statsTemp$totArea
         snowBasinData$snow_area_km[count] <- statsTemp$totSnoArea
         snowBasinData$snow_cover_fraction[count] <- statsTemp$snoFrac
         snowBasinData$mean_snow_line_meters[count] <- statsTemp$meanSnoElevMeters
         snowBasinData$mean_snow_line_feet[count] <- statsTemp$meanSnoElevFeet
         snowBasinData$snow_volume_cub_meters[count] <- statsTemp$sweVolCubMeters
         snowBasinData$snow_volume_acre_feet[count] <- statsTemp$sweVolAcreFeet
         snowBasinData$mean_swe_mm[count] <- statsTemp$meanSweMM
         snowBasinData$max_swe_mm[count] <- statsTemp$maxSweMM

         count = count + 1
      }
   }
}

# Close geogrid file
nc_close(idGeo)

# Save data to output file
save(snowBasinData,file=outFile)
