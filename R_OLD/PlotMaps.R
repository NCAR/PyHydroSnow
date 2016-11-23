SetupMap <- function(file) {
        ncid <- ncdf4::nc_open(file)
        lats<-ncdf4::ncvar_get(ncid, "XLAT_M")
        lons<-ncdf4::ncvar_get(ncid, "XLONG_M")
        ncdf4::nc_close(ncid)
        myLocation <- c(min(lons)-0.1, min(lats)-0.1, max(lons)+0.1, max(lats)+0.1)
        myMap <- ggmap::get_map(location=myLocation, source="stamen", maptype="terrain", crop=TRUE)
	myMap
}

PlotMapErrors <- function(myMap, statsObj, 
			statsTag, statsVar, statsSeas, 
			plotTitle="Model Errors", plotSubTitle="",
			sizeVar="t_mae", colorVar="t_msd",
			sizeLab="Mean Absolute Error", colorLab="Mean Signed Deviation",
			colorLow="blue", colorMid="white", colorHigh="red",
			minThreshSize=NULL, maxThreshSize=NULL,
			minThreshCol=NULL, maxThreshCol=NULL,
			minPtsize=1, maxPtsize=8,
			exclVar="t_n", exclThresh=0.8,
			colBreaks, 
			valBreaks) {
	if (is.null(statsVar)) {
		myData <- subset(statsObj, statsObj$tag==statsTag & statsObj$seas==statsSeas)
	} else {
		myData <- subset(statsObj, statsObj$tag==statsTag & statsObj$var==statsVar & statsObj$seas==statsSeas)
	}
	maxCnt <- max(myData[,exclVar], na.rm=TRUE)
	myData <- subset(myData, myData[,exclVar] >= exclThresh*maxCnt)
	#myData <- subset(myData, myData[,sizeVar]>quantile(myData[,sizeVar], 0.05, na.rm=TRUE) & 
	#		myData[,sizeVar]<quantile(myData[,sizeVar], 0.95, na.rm=TRUE))
	if (is.null(minThreshSize)) minThreshSize <- min(myData[,sizeVar], na.rm=TRUE)
	if (is.null(maxThreshSize)) maxThreshSize <- max(myData[,sizeVar], na.rm=TRUE)
	if (is.null(minThreshCol)) minThreshCol <- min(myData[,colorVar], na.rm=TRUE)
	if (is.null(maxThreshCol)) maxThreshCol <- max(myData[,colorVar], na.rm=TRUE)
	xCol <- ifelse("lon" %in% names(statsObj), "lon", "st_lon")
	yCol <- ifelse("lat" %in% names(statsObj), "lat", "st_lat")
	myData$plotcol <- cut(myData[,colorVar], breaks = valBreaks, right = FALSE)
	valBreaksScaled <- scales::rescale(valBreaks, from=range(myData[,colorVar], na.rm = TRUE, finite = TRUE))
	gg <- ggmap::ggmap(myMap) + 
		ggplot2::geom_point(aes_string(x=xCol, y=yCol, size=sizeVar, fill="plotcol"), data=myData, alpha=0.8, shape=21) + 
		ggplot2::scale_size(sizeLab, range=c(minPtsize, maxPtsize), limits=c(minThreshSize, maxThreshSize)) +
		ggplot2::scale_fill_manual(colorLab, values=colBreaks) +
		#ggplot2::scale_fill_gradient2(colorLab, low=colorLow, mid=colorMid, high=colorHigh, midpoint=0, limits=c(minThreshCol, maxThreshCol)) +
		#ggplot2::scale_fill_gradientn(colorLab, colours = colBreaks, values = scales::rescale(valBreaks)) +
		ggplot2::ggtitle(bquote(atop(.(plotTitle), atop(italic(.(plotSubTitle)), "")))) +
		ggplot2::theme(plot.title = element_text(size=18, face="bold", vjust=-1)) +
		ggplot2::guides(fill = guide_legend(override.aes = list(size=3)))
	gg
}

PlotPeakSweMap <- function(myMap,dStart,dEnd,geoNX,geoNY) {
	# Generate map of peak SWE julian day from beginning
	# of water year. Also output to NetCDF for further analysis
	
	# Establish datetime information based on passed data.
	dUnits <- "days"
	diff1 <- difftime(dEnd,dStart,units=dUnits)
	nSteps <- diff1 <- as.numeric(diff1)
	dt <- 24*3600

	numMod <- length(modPathList)

	# Initialize peak swe date to -9999.
	peakDaySnodas <- array(-9999,c(geoNX,geoNY))
	peakDayMod <- array(-9999,c(geoNX,geoNY,numMod))
	# Initialize count array to filter days out later
	dayCountSnodas <- array(0,c(geoNX,geoNY))
	dayCountMod <- array(0,c(geoNX,geoNY,numMod))

	# Loop through and read in data
	for (i in 1:nSteps) {
		dCurrent <- dStart + dt*i

		message(paste0('Processing: ',dCurrent))
		# Processing SNODAS first
		snodasPath <- paste0(snodasPathList[1],"/SNODAS_REGRIDDED_",
                  strftime(dCurrent,"%Y%m%d"),".nc")
		message(snodasPath)
                id <- nc_open(snodasPath)
                sweSnodas <- ncvar_get(id,'SNEQV')
                nc_close(id)
		if (i == 1) {
			snodasTest <- sweSnodas
		} else {
			maxInd <- which(sweSnodas > snodasTest)
			peakDaySnodas[maxInd] <- i
			snodasTest[maxInd] <- sweSnodas[maxInd]
			dayCountSnodas[maxInd] <- dayCountSnodas[maxInd] + 1
		}

		# Processing WRF-Hydro output
		for (j in 1:numMod) {
			modPath <- paste0(modPathList[[j]],"/",strftime(dCurrent,"%Y%m%d"),
                          "00.LDASOUT_DOMAIN1")
                        id <- nc_open(modPath)
                        sweModel <- ncvar_get(id,'SNEQV')
                        nc_close(id)

			if (i == 1) {
				if (j == 1) {
					modTest <- array(-9999.0,c(geoNX,geoNY,numMod))
				}
				modTest[,,j] <- sweModel
			} else {
				testTemp <- modTest[,,j]
				peakDayTemp <- peakDayMod[,,j]
				dayCountTemp <- dayCountMod[,,j]
				maxInd <- which(sweModel > testTemp)
				testTemp[maxInd] <- sweModel[maxInd]
				peakDayTemp[maxInd] <- i
				dayCountTemp[maxInd] <- dayCountTemp[maxInd] + 1
				peakDayMod[,,j] <- peakDayTemp
				modTest[,,j] <- testTemp
				dayCountMod[,,j] <- dayCountTemp
			}
		}
	}

	# Cleanup data
	indTmp <- which(dayCountSnodas <= 21)
	peakDaySnodas[indTmp] <- -9999

	indTmp <- which(dayCountMod <= 21)
	peakDayMod[indTmp] <- -9999

	# Caculate difference grid between SNODAS and Model on peak timing
	gridDiff <- array(-9999,c(geoNX,geoNY,numMod))
	for (j in 1:numMod) {
		peakDayModTmp <- peakDayMod[,,j]
		diffTmp <- array(-9999,c(geoNX,geoNY))
		indTmp <- which(peakDaySnodas != -9999 & peakDayModTmp != -9999)
		diffTmp[indTmp] <- peakDayModTmp[indTmp] - peakDaySnodas[indTmp]
		gridDiff[,,j] <- diffTmp
	}

	# Output to NetCDF

	varList <- list()
	varList[[1]] <- list(name='SNODAS_PEAK_DAY_SWE',
			     longname='WY DOY Peak SWE is achieved',
			     units='Julian', precision='integer',
		             missing=-9999,
		             dimensionList=list(x=list(name='longitude',values=1:geoNX,
                             units='Degrees East',
                             unlimited=FALSE,
                             create_dimvar=FALSE),
                             y=list(name='latitude',values=1:geoNY,
                               units='Degrees North',
                               unlimited=FALSE,create_dimvar=FALSE)),
			     data=peakDaySnodas)
	for (j in 1:numMod) {
		varList[[(j+1)]] <- list(name=paste0('MODEL',toString(j),'_PEAK_DAY_SWE'),
               	             longname='WY DOY Peak SWE is achieved',
			     model_tags=modTagList[[j]],
                     	     units='Julian', precision='integer',
  	                     missing=-9999,
                             dimensionList=list(x=list(name='longitude',values=1:geoNX,
                       	     units='Degrees East',
                             unlimited=FALSE,
               	             create_dimvar=FALSE),
                       	     y=list(name='latitude',values=1:geoNY,
                               units='Degrees North',
                               unlimited=FALSE,create_dimvar=FALSE)),
			     data=peakDayMod[,,j])
	}
	for (j in 1:numMod) {
                varList[[(j+numMod+1)]] <- list(name=paste0('MODEL',toString(j),'_PEAK_DAY_DIFFERENCE'),
                             longname='Model Minus SNODAS Peak DOY',
                             model_tags=modTagList[[j]],
                             units='Julian', precision='integer',
                             missing=-9999,
                             dimensionList=list(x=list(name='longitude',values=1:geoNX,
                             units='Degrees East',
                             unlimited=FALSE,
                             create_dimvar=FALSE),
                             y=list(name='latitude',values=1:geoNY,
                               units='Degrees North',
                               unlimited=FALSE,create_dimvar=FALSE)),
                             data=gridDiff[,,j])
	}

        globalAttList <- list()
        globalAttList[[1]] <- list(name='Institution',value='NCAR-RAL',precision="text")
	
	outPath <- paste0(writeDir,"/SNODAS_WRF_HYDRO_SWE_PEAK_DOY_WY",strftime(dEnd,"%Y"),".nc")
	MkNcdf(varList,filename=outPath,globalAttList=globalAttList)

	# Create GeoTiff for GIS plotting
	outPathTif <- paste0(writeDir,"/SNODAS_PEAK_DOY_WY",strftime(dEnd,"%Y"),".tif")
	ExportGeogrid(outPath,'SNODAS_PEAK_DAY_SWE',outPathTif,inCoordFile=geoFile)
     
	for (j in 1:numMod) {
		outPathTif <- paste0(writeDir,"/",modTagList[[j]],
				"_WRF_HYDRO_SWE_PEAK_DOY_WY",strftime(dEnd,"%Y"),".tif")
		var <- paste0('MODEL',toString(j),'_PEAK_DAY_SWE')
		ExportGeogrid(outPath,var,outPathTif,inCoordFile=geoFile)
		outPathTif <- paste0(writeDir,"/",modTagList[[j]],
				"_WRF_HYDRO_SWE_PEAK_DOY_DIFF_WY",strftime(dEnd,"%Y"),".tif")
		var <- paste0('MODEL',toString(j),'_PEAK_DAY_DIFFERENCE')
		ExportGeogrid(outPath,var,outPathTif,inCoordFile=geoFile)
	}


}

PlotSnodasGridMap <- function(dStart,dEnd,geoNX,geoNY) {
	# Generate spatial gridded map of model vs SNODAS error metrics:
	# 1.) bias
	# 2.) temporal correlation
	# 3.) snowmelt production efficiency

        # Establish datetime information based on passed data.
        dUnits <- "days"
        diff1 <- difftime(dEnd,dStart,units=dUnits)
        nSteps <- diff1 <- as.numeric(diff1)
        dt <- 24*3600

	resSquared <- (resMod*1000.0)*(resMod*1000.0)
	validMin <- 0.0
	validMax <- 10000.0
	validMinBias <- 10.0
	validMax <- 1000.0

        numMod <- length(modPathList)

	# Initialize output arrays to -9999.
	biasOut <- array(-9999.0,c(geoNX,geoNY,numMod))
	corrOut <- array(-9999.0,c(geoNX,geoNY,numMod))
	snodasMeanSwe <- array(-9999.0,c(geoNX,geoNY))
	modMeanSwe <- array(-9999.0,c(geoNX,geoNY,numMod))

	# Initialize input arrays
	sweSnodasIn <- array(-9999,c(geoNX,geoNY,nSteps))
	sweModIn <- array(-9999,c(geoNX,geoNY,nSteps,numMod))

	# Loop through and read in data
        for (i in 1:nSteps) {
                dCurrent <- dStart + dt*i

                message(paste0('Processing: ',dCurrent))
                # Processing SNODAS first
                snodasPath <- paste0(snodasPathList[1],"/SNODAS_REGRIDDED_",
                  strftime(dCurrent,"%Y%m%d"),".nc")
		print(snodasPath)
                idSnodas <- nc_open(snodasPath)
                sweSnodas <- ncvar_get(idSnodas,'SNEQV')
		sweSnodasIn[,,i] <- sweSnodas
		rm(sweSnodas)
                nc_close(idSnodas)

		# Processing WRF-Hydro output
                for (j in 1:numMod) {
                        modPath <- paste0(modPathList[[j]],"/",strftime(dCurrent,"%Y%m%d"),
                          "00.LDASOUT_DOMAIN1")
                        id <- nc_open(modPath)
                        sweModel <- ncvar_get(id,'SNEQV')
			sweModIn[,,i,j] <- sweModel
			rm(sweModel)
                        nc_close(id)

		}

	}

	# Loop through grid and calculate error metrics
	for (i in 1:geoNX){
		message(paste0('Processing Column: ',toString(i)))

		for (j in 1:geoNY){
			snodasTmp <- sweSnodasIn[i,j,]

			# Loop through model groups
			for (k in 1:numMod){
				acc_bias <- 0.0
                		countBias <- 0

				sweModSum <- 0.0

				modelTmp <- sweModIn[i,j,,k]

				for (m in 1:nSteps){
					v1 <- snodasTmp[m]
					v2 <- modelTmp[m]

					if (! is.na(v1) & ! is.na(v2)){
						if (v1 > validMin & v2 > validMin & v1 < validMax & v2 < validMax){
							sweVolSnodasTmp <- (v1/1000.0)*resSquared # Cubic meters
							sweVolModTmp <- (v2/1000.0)*resSquared

							# Calculate bias
							if (v1 > validMinBias & v2 > validMinBias) {
								biasTmp <- v2 - v1
								acc_bias <- acc_bias + biasTmp
								countBias <- countBias + 1
							}
	
        	                                	sweModSum <- sweModSum + modelTmp[m]
	
						}
					}

				
				}

				if (countBias > 0){
					biasOut[i,j,k] <- acc_bias/countBias
				}
				# Calculate temporal correlation
				ind1 <- which(snodasTmp > 0.0 & modelTmp > 0.0)
				corrOut[i,j,k] <- cor(snodasTmp[ind1],modelTmp[ind1])

				# Mean SWE for time period 
				modMeanSwe[i,j,k] <- sweModSum/nSteps

			}

			indSnodas <- which(snodasTmp >= 0.0 & snodasTmp < 10000.0)
			snodasMeanSwe[i,j] <- (sum(snodasTmp[indSnodas]))/length(indSnodas)

		}
	}

	# Output to NetCDF
	varList <- list()
	varList[[1]] <- list(name='SNODAS_mean_SWE',
			     longname='SNODAS Mean SWE',
                             units='mm', precision='float',
                             missing=-9999,
                             dimensionList=list(x=list(name='longitude',values=1:geoNX,
                             units='Degrees East',
                             unlimited=FALSE,
                             create_dimvar=FALSE),
                             y=list(name='latitude',values=1:geoNY,
                               units='Degrees North',
                               unlimited=FALSE,create_dimvar=FALSE)),
                             data=snodasMeanSwe)
	for (n in 1:numMod) {
		varList[[n+1]] <- list(name=paste0('MODEL',toString(n),'_mean_SWE'),
                	             longname='MODEL Mean SWE',
				     model_tags=modTagList[[n]],
                             	     units='mm', precision='float',
                             	     missing=-9999,
                                     dimensionList=list(x=list(name='longitude',values=1:geoNX,
                                     units='Degrees East',
                                     unlimited=FALSE,
                                     create_dimvar=FALSE),
                                     y=list(name='latitude',values=1:geoNY,
                                       units='Degrees North',
                                       unlimited=FALSE,create_dimvar=FALSE)),
                                     data=modMeanSwe[,,n])
	}
	for (n in 1:numMod) {
		varList[[(n+numMod+1)]] <- list(name=paste0('MODEL',toString(n),'_swe_bias'),
			     longname='MODEL SWE bias against SNODAS',
			     model_tags=modTagList[[n]],
                             units='mm', precision='float',
                             missing=-9999,
                             dimensionList=list(x=list(name='longitude',values=1:geoNX,
                             units='Degrees East',
                             unlimited=FALSE,
                             create_dimvar=FALSE),
                             y=list(name='latitude',values=1:geoNY,
                               units='Degrees North',
                               unlimited=FALSE,create_dimvar=FALSE)),
                             data=biasOut[,,n])
	}
	
	globalAttList <- list()
        globalAttList[[1]] <- list(name='Institution',value='NCAR-RAL',precision="text")

        outPath <- paste0(writeDir,"/SNODAS_WRF_HYDRO_GRIDDED_SWE_ANALYSIS_",strftime(dStart,"%Y%m%d"),
	  "_",strftime(dEnd,"%Y%m%d"),".nc")
        MkNcdf(varList,filename=outPath,globalAttList=globalAttList)

	# Export to Tif format for GIS plotting
	outPathTif <- paste0(writeDir,"/SNODAS_MEAN_SWE_",strftime(dStart,"%Y%m%d"),
			"_",strftime(dEnd,"%Y%m%d"),".tif")
	var <- "SNODAS_mean_SWE"
	ExportGeogrid(outPath,var,outPathTif,inCoordFile=geoFile)

	for (n in 1:numMod){
		outPathTif <- paste0(writeDir,"/",modTagList[[n]],"_MEAN_SWE_",strftime(dStart,"%Y%m%d"),
       	                 "_",strftime(dEnd,"%Y%m%d"),".tif")
        	var <- paste0("MODEL",toString(n),"_mean_SWE")
       		ExportGeogrid(outPath,var,outPathTif,inCoordFile=geoFile)
		outPathTif <- paste0(writeDir,"/",modTagList[[n]],"_SWE_BIAS_",strftime(dStart,"%Y%m%d"),
                         "_",strftime(dEnd,"%Y%m%d"),".tif")
                var <- paste0("MODEL",toString(n),"_swe_bias")
                ExportGeogrid(outPath,var,outPathTif,inCoordFile=geoFile)
	}
}
