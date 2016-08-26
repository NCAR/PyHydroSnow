##########################################################
##      Read SNODAS/WRF-Hydro Snow basin data           ##
##########################################################

library(data.table)
library(ncdf4)


# Open geogrid file
idGeo <- nc_open(geoFile)

# Establish delta time information based on SNODAS start/end dates
dUnits <- "days"
diff1 <- difftime(readSnodasEnd,readSnodasStart,units=dUnits)
nSteps <- diff1 <- as.numeric(diff1)
dt <- 24*3600

if (readEnsemble) {
        numEns <- length(ensembleList)
        print(numEns)
        # Expand modPathList to number of ensembles
        modPathList <- expandModPathList(numEns,ensembleList,length(modPathList),modPathList)
        print(modPathList)
        modTagList <- expandModTagList(numEns,length(modPathList),modTagList)
        print(modTagList)
        ensTagList <- expandEnsTagList(numEns,length(modPathList),modPathList,ensembleList)
        print(ensTagList)
} else {
        numEns <- 1
}

# Create output data frames that will hold data
snowBasinData <- data.frame(matrix(NA,ncol=20,nrow=(nSteps*length(mskgeo.nameList)*(length(modPathList)+1))))
names(snowBasinData) <- c("Basin","Date","basin_area_km","product","ens","snow_area_km","snow_cover_fraction",
                        "mean_snow_line_meters","mean_snow_line_feet","snow_volume_cub_meters",
                        "snow_volume_acre_feet","mean_swe_mm","mean_depth_mm","max_depth_mm",
                        "max_swe_mm","min_rho_kgm3","max_rho_kgm3","mean_rho_kgm3","acc_runoff_cub_meters",
			"acc_runoff_af")
snowBasinData$Basin <- NA
snowBasinData$Date <- as.Date(as.POSIXct('1900-01-01'),'GMT')
snowBasinData$basin_area_km <- NA
snowBasinData$product <- NA
snowBasinData$ens <- NA
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
snowBasinData$acc_runoff_cub_meters <- NA
snowBasinData$acc_runoff_af <- NA

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
		dCurrent <- readSnodasStart + dt*j
		
                message(paste0('Processing: ',dCurrent))
		# SNODAS first
		snodasPath <- paste0(snodasPathList[1],"/SNODAS_REGRIDDED_",
                  strftime(dCurrent,"%Y%m%d"),".nc")
		id <- nc_open(snodasPath)
		sweSnodas <- ncvar_get(id,'SNEQV',start=bStart,count=bCount)
		runoffSnodas <- 0.0 # Runoff not calculated from SNODAS
                nc_close(id)

		statsTemp <- basSnowMetrics(sweSnodas,mskVar,basElev,runoffSnodas,res=resMod,runoffFlag=0)
                snowBasinData$Basin[count] <- bName
                snowBasinData$Date[count] <- as.Date(dCurrent)
                snowBasinData$product[count] <- "SNODAS"
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
                # Model data
		for(k in 1:length(modPathList)) {
			modoutTag <- modTagList[k]
			if (numEns > 1) {
                        	ensoutTag <- ensTagList[k]
                	} else {
                        	ensoutTag <- modoutTag 
                	}
			tmpPath = modPathList[[k]]
			snowPath <- paste0(modPathList[[k]],"/",strftime(dCurrent,"%Y%m%d"),
			  "00.LDASOUT_DOMAIN1")
			id <- nc_open(snowPath)
			sweModel <- ncvar_get(id,'SNEQV',start=bStart,count=bCount)
			runoffModel <- ncvar_get(id,'SFCRNOFF',start=bStart,count=bCount)
			nc_close(id)

			statsTemp <- basSnowMetrics(sweModel,mskVar,basElev,runoffModel,res=resMod,runoffFlag=1)
			snowBasinData$Basin[count] <- bName
	                snowBasinData$Date[count] <- dCurrent
                        snowBasinData$product[count] <- modoutTag
			snowBasinData$ens[count] <- ensoutTag 
                	snowBasinData$basin_area_km[count] <- statsTemp$totArea
    	                snowBasinData$snow_area_km[count] <- statsTemp$totSnoArea
        	        snowBasinData$snow_cover_fraction[count] <- statsTemp$snoFrac
                	snowBasinData$mean_snow_line_meters[count] <- statsTemp$meanSnoElevMeters
                	snowBasinData$mean_snow_line_feet[count] <- statsTemp$meanSnoElevFeet
                	snowBasinData$snow_volume_cub_meters[count] <- statsTemp$sweVolCubMeters
                	snowBasinData$snow_volume_acre_feet[count] <- statsTemp$sweVolAcreFeet
                	snowBasinData$mean_swe_mm[count] <- statsTemp$meanSweMM
                	snowBasinData$max_swe_mm[count] <- statsTemp$maxSweMM
			snowBasinData$acc_runoff_cub_meters[count] <- statsTemp$roCubMeters
			snowBasinData$acc_runoff_af[count] <- statsTemp$roAcreFeet

			count = count + 1
		}

	}

}

# Close geogrid file
nc_close(idGeo)

# Save data to output file
save(snowBasinData,file=snodasReadFileOut)

