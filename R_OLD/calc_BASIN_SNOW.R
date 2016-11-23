##########################################################
##    Calculate SNODAS/WRF-Hydro Snow basin metrics     ##
##########################################################

library(data.table)

# Establish date/time information
dUnits <- "days"
diff1 <- difftime(enddate_stats,stdate_stats,units=dUnits)
nSteps <- diff1 <- as.numeric(diff1)
dt <- 24*3600
resSquared <- (resMod*1000.0)*(resMod*1000.0)

# Establish output dataframe
snowMetrics <- data.frame(matrix(NA,ncol=14,nrow=(length(mskgeo.nameList)*(length(modPathList)+1))))
names(snowMetrics) <- c("Basin","product","start_date","end_date","bias","temporal_correlation",
		       "peak_swe_volume_af","peak_swe_volume_cub_meters","acc_snow_melt_af",
		       "acc_snow_melt_cub_meters","acc_runoff_af","acc_runoff_cub_meters",
		       "snowmelt_prod_efficiency","snowmelt_stream_efficiency")

snowMetrics$Basin <- NA
snowMetrics$product <- NA
snowMetrics$start_date <- as.Date(as.POSIXct('1900-01-01'),'GMT')
snowMetrics$end_date <- as.Date(as.POSIXct('1900-01-01'),'GMT')
snowMetrics$bias <- NA
snowMetrics$temporal_correlation <- NA
snowMetrics$peak_swe_volume_af <- NA
snowMetrics$peak_swe_volume_cub_meters <- NA
snowMetrics$acc_snow_melt_af <- NA
snowMetrics$acc_snow_melt_cub_meters <- NA
snowMetrics$acc_runoff_af <- NA
snowMetrics$acc_runoff_cub_meters <- NA
snowMetrics$snowmelt_prod_efficiency <- NA
snowMetrics$snowmelt_stream_efficiency <- NA

count <- 1

# Loop through basins and calculate SNODAS/model error metrics
for (i in 1:length(mskgeo.nameList)) {
      bName <- mskgeo.nameList[[i]]
      gageId <- mskgeo.nameList[[i]]
      mskvar <- mskgeo.List[[i]]
        
      message(paste0('Processing Basin: ',bName))

      peak_swe_volume_cub_meters <- 0.0
      acc_snow_melt_cub_meters <- 0.0
      acc_runoff_cub_meters <- 0.0
      acc_bias <- 0.0
      countBias <- 0
      totalStreamFlow <- 0.0

	# Loop through days and peform SNODAS analysis
        for (j in 1:nSteps){
		dCurrent <- stdate_stats + dt*j
		dPrev <- stdate_stats + dt*(j - 1)

		ind <- which(snowBasinData$Date == as.Date(dCurrent) & snowBasinData$Basin == bName &
		             snowBasinData$product == "SNODAS")
		if (j > 1){
			indPrev <- which(snowBasinData$Date == as.Date(dPrev) & snowBasinData$Basin == bName &
                             snowBasinData$product == "SNODAS")
		}

		if (length(ind) != 1) {
			message(paste0("ERROR: SNODAS data not found for: ",dCurrent))
			quit()
		}

		# Tally peak SWE volume
		if (snowBasinData$snow_volume_cub_meters[ind] > peak_swe_volume_cub_meters) {
			peak_swe_volume_cub_meters <- snowBasinData$snow_volume_cub_meters[ind]
		}

		# Accumulated snowmelt
		if (j > 1){
			if (snowBasinData$snow_volume_cub_meters[ind] < snowBasinData$snow_volume_cub_meters[indPrev]) {
				acc_snow_melt_cub_meters <- acc_snow_melt_cub_meters + 
				  (snowBasinData$snow_volume_cub_meters[indPrev] - snowBasinData$snow_volume_cub_meters[ind])
			}
		}

	}

      snowMetrics$Basin[count] <- bName
      snowMetrics$product[count] <- "SNODAS"
      snowMetrics$start_date[count] <- as.Date(stdate_stats)
      snowMetrics$end_date[count] <- as.Date(enddate_stats)
      snowMetrics$bias[count] <- NA # Since SNODAS is our "verification" data, no bias calculated for it.
      snowMetrics$temporal_correlation[count] <- NA # Correlation is calculated for models.
      snowMetrics$peak_swe_volume_cub_meters[count] <- peak_swe_volume_cub_meters
      snowMetrics$peak_swe_volume_af[count] <- peak_swe_volume_cub_meters*0.000810714
      snowMetrics$acc_snow_melt_cub_meters[count] <- acc_snow_melt_cub_meters
      snowMetrics$acc_snow_melt_af[count] <- acc_snow_melt_cub_meters*0.000810714
      snowMetrics$acc_runoff_cub_meters[count] <- NA # SNODAS does not contain runoff 
      snowMetrics$acc_runoff_af[count] <- NA # SNODAS does not contain runoff
      snowMetrics$snowmelt_prod_efficiency[count] <- acc_snow_melt_cub_meters/peak_swe_volume_cub_meters
      snowMetrics$snowmelt_stream_efficiency[count] <- NA # Can't calculate since SNODAS isn't water conservative.

      count <- count + 1

	# Loop through models and peform analysis
	for(k in 1:length(modPathList)) {
		peak_swe_volume_cub_meters <- 0.0
        	acc_snow_melt_cub_meters <- 0.0
        	acc_runoff_cub_meters <- 0.0

        	acc_bias <- 0.0
        	countBias <- 0
        	totalStreamFlow <- 0.0

		product <- modTagList[[k]]	
		for (j in 1:nSteps){
                	dCurrent <- stdate_stats + dt*j
			dCurrentFrxst <- stdate_stats + dt*j + 3600 # frxst points are at 1 UTC.
                	dPrev <- stdate_stats + dt*(j - 1)

			print(dCurrent)
                	ind <- which(snowBasinData$Date == as.Date(dCurrent) & snowBasinData$Basin == bName &
                        	     snowBasinData$product == product)
                	if (j > 1){
                        	indPrev <- which(snowBasinData$Date == as.Date(dPrev) & snowBasinData$Basin == bName &
                             	snowBasinData$product == product)
                	}

                	if (length(ind) != 1) {
                        	message(paste0("ERROR: ",product,"data not found for: ",dCurrent))
                        	quit()
                	}

                	# Tally peak SWE volume
                	if (snowBasinData$snow_volume_cub_meters[ind] > peak_swe_volume_cub_meters) {
                        	peak_swe_volume_cub_meters <- snowBasinData$snow_volume_cub_meters[ind]
                	}

                	# Accumulated snowmelt
                	if (j > 1){
                        	if (snowBasinData$snow_volume_cub_meters[ind] < snowBasinData$snow_volume_cub_meters[indPrev]) {
                                	acc_snow_melt_cub_meters <- acc_snow_melt_cub_meters +
                                  	(snowBasinData$snow_volume_cub_meters[indPrev] - snowBasinData$snow_volume_cub_meters[ind])
                        	}
                	}

                	# Accumulated runoff
			if (j > 1){
				if (snowBasinData$acc_runoff_cub_meters[ind] > snowBasinData$acc_runoff_cub_meters[indPrev]) {
					acc_runoff_cub_meters <- acc_runoff_cub_meters + 
					(snowBasinData$acc_runoff_cub_meters[ind] - snowBasinData$acc_runoff_cub_meters[indPrev])
				}
			}
			acc_runoff_af <- acc_runoff_cub_meters*0.000810714

			# Calculate bias
			ind2 <- which(snowBasinData$Date == as.Date(dCurrent) & snowBasinData$Basin == bName &
                                     snowBasinData$product == "SNODAS")
			sweVolSnodasTemp <- snowBasinData$snow_volume_acre_feet[ind2]
			sweVolModelTemp <- snowBasinData$snow_volume_acre_feet[ind]
			if (sweVolSnodasTemp > 0.0 & sweVolModelTemp > 0.0) {
				biasTemp <- sweVolModelTemp - sweVolSnodasTemp
				acc_bias <- acc_bias + biasTemp
				countBias <- countBias + 1
			}

			# Total accumulated streamflow
			#ind3 <- which(modFrxstout$tag == product & modFrxstout$site_no == gageId & modFrxstout$POSIXct == dCurrentFrxst)
			#totalStreamFlowTmp <- modFrxstout$ACCFLOW[ind3]

        	}

		# Convert total streamflow from mm to basin volume in cubic meters
		#totalStreamFlow <- sum((mskvar/1000.0))*resSquared*totalStreamFlowTmp

		snowMetrics$Basin[count] <- bName
        	snowMetrics$product[count] <- modTagList[[k]]
        	snowMetrics$start_date[count] <- as.Date(stdate_stats)
        	snowMetrics$end_date[count] <- as.Date(enddate_stats)
		# Calculate bias using SNODAS
        	snowMetrics$bias[count] <- acc_bias/countBias # Units of mm
		# Calculate correlation ONLY where snow exists for both model and SNODAS
		indCor1 <- which(snowBasinData$product == "SNODAS" & snowBasinData$Basin == bName)
		snodasTmp <- snowBasinData$snow_volume_cub_meters[indCor1]
		indCor2 <- which(snowBasinData$product == product & snowBasinData$Basin == bName) 
		modelTmp <- snowBasinData$snow_volume_cub_meters[indCor2]
		indCor3 <- which(snodasTmp > 0.0 & modelTmp > 0.0)
        	snowMetrics$temporal_correlation[count] <- cor(snodasTmp[indCor3],modelTmp[indCor3])
        	snowMetrics$peak_swe_volume_cub_meters[count] <- peak_swe_volume_cub_meters
        	snowMetrics$peak_swe_volume_af[count] <- peak_swe_volume_cub_meters*0.000810714
        	snowMetrics$acc_snow_melt_cub_meters[count] <- acc_snow_melt_cub_meters
        	snowMetrics$acc_snow_melt_af[count] <- acc_snow_melt_cub_meters*0.000810714
        	snowMetrics$acc_runoff_cub_meters[count] <- acc_runoff_cub_meters
        	snowMetrics$acc_runoff_af[count] <- acc_runoff_af
        	snowMetrics$snowmelt_prod_efficiency[count] <- acc_snow_melt_cub_meters/peak_swe_volume_cub_meters
		#snowMetrics$snowmelt_stream_efficiency[count] <- totalStreamFlow/acc_snow_melt_cub_meters
		
		count <- count + 1	
		

	}

}

# Save data to output file
save(snowMetrics,file=statsFileOut)
