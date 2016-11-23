# Load color pallete for ensemble plotting
load('./ensembleColors.Rdata')

PlotAccPrecipFlow <- function(n, str1=modFrxstout_wy2015_NLDAS2dwnsc_fullrtng,
                        lsm1=modLdasout_wy2015_NLDAS2dwnsc_fullrtng_BAS,
                        obsstr=obsStr.dy,
                        str2=modFrxstout_wy2015_NLDAS2dwnsc_NSSL_fullrtng,
                        lsm2=modLdasout_wy2015_NLDAS2dwnsc_NSSL_fullrtng_BAS) {
  str1 <- subset(str1, str1$site_no==n)
  lsm1 <- subset(lsm1, lsm1$site_no==n)
  obsstr <- subset(obsstr, obsstr$site_no==n)
  str2 <- subset(str2, str2$site_no==n)
  lsm2 <- subset(lsm2, lsm2$site_no==n)
  with(lsm1, plot(POSIXct, ACCPRCP, typ='l', col='darkmagenta'))
  with(str1, lines(POSIXct, ACCFLOW, col='deepskyblue'))
  with(lsm1, lines(POSIXct, ACCECAN+ACCEDIR+ACCETRAN, col='darkolivegreen3'))
  with(lsm2, lines(POSIXct, ACCPRCP, col='darkmagenta', lty=2))
  with(str2, lines(POSIXct, ACCFLOW, col='deepskyblue', lty=2))
  with(lsm2, lines(POSIXct, ACCECAN+ACCEDIR+ACCETRAN, col='darkolivegreen3', lty=2))
  with(obsstr, lines(POSIXct, cumqvol_mm, col='blue', lwd=2, lty=1))
}


PlotAccPrecip <- function(n, modDfs,
                        labMods=NULL,
                        labTitle="Accumulated Precipitation",
                        lnCols=NULL, lnWds=NULL, lnTyps=NULL,
                        stdate=NULL,
                        enddate=NULL,
                        modCol="ACCPRCP") {
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  if (is.data.frame(modDfs)) {
        str1 <- modDfs
        strcnt <- 1
  } else if (is.list(modDfs)) {
        str1 <- modDfs[[1]]
        strcnt <- length(modDfs)
  } else {
        stop("modDfs must be a dataframe or a list of dataframes")
  }
  # Subset by dates
  if (is.null(stdate)) stdate <- min(str1$POSIXct)
  if (is.null(enddate)) enddate <- max(str1$POSIXct)
  str1 <- subset(str1, str1$statArg==n & str1$POSIXct>=stdate & str1$POSIXct<=enddate)
  # Calculate maximum y val for plot limits
  ymax <- max(str1[nrow(str1),modCol]-str1[1,modCol], na.rm=TRUE)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (stri in modDfs) {
                stri <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                ymax <- max(ymax, stri[nrow(stri),modCol]-stri[1,modCol], na.rm=TRUE)
                }
        }
  # Set colors, widths, types
  if (is.null(lnCols)) lnCols <- sample(colours(), strcnt)
  if (is.null(lnWds)) lnWds <- rep(1, strcnt)
  if (is.null(lnTyps)) lnTyps <- rep(1, strcnt)
  # Set labels
  if (is.null(labMods)) labMods <- paste0("Model", 1:strcnt)
  # Create plot
  plot(str1$POSIXct, str1[,modCol]-str1[1,modCol], typ='l', ylim=c(0, ymax),
        xlim=c(stdate, enddate), col=lnCols[1], lty=lnTyps[1], lwd=lnWds[1],
        xlab="", ylab="Accumulated precipitation (mm)", cex.axis=1.2, cex.lab=1.2)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (j in 2:length(modDfs)) {
                stri <- modDfs[[j]]
                stri <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                lines(stri$POSIXct, stri[,modCol]-stri[1,modCol], col=lnCols[j], lty=lnTyps[j], lwd=lnWds[j])
                }
        }
  title(labTitle, cex.main=1.6)
  legend("topleft", c(labMods[1:strcnt]),
                lty=c(lnTyps[1:strcnt]), lwd=c(lnWds[1:strcnt]),
                col=c(lnCols[1:strcnt]), cex=1.2)
}


PlotAccFlow <- function(n, modDfs, obs,
                        labMods=NULL,
                        labObs="Observed",
                        labTitle="Accumulated Flow",
                        lnCols=NULL, lnWds=NULL, lnTyps=NULL,
                        stdate=NULL,
                        enddate=NULL,
                        modCol="ACCFLOW", obsCol="cumqvol_mm") {
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  if (is.data.frame(modDfs)) {
        str1 <- modDfs
        strcnt <- 1
  } else if (is.list(modDfs)) {
        str1 <- modDfs[[1]]
        strcnt <- length(modDfs)
  } else {
        stop("modDfs must be a dataframe or a list of dataframes")
  }
  # Subset by dates
  if (is.null(stdate)) stdate <- min(str1$POSIXct)
  if (is.null(enddate)) enddate <- max(str1$POSIXct)
  str1 <- subset(str1, str1$site_no==n & str1$POSIXct>=stdate & str1$POSIXct<=enddate)
  obs <- subset(obs, obs$site_no==n & obs$POSIXct>=stdate & obs$POSIXct<=enddate)
  # Calculate maximum y val for plot limits
  ymax <- max(str1[nrow(str1),modCol]-str1[1,modCol], obs[nrow(obs),obsCol]-obs[1,obsCol], na.rm=TRUE)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (stri in modDfs) {
                stri <- subset(stri, stri$site_no==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                ymax <- max(ymax, stri[nrow(stri),modCol]-stri[1,modCol], na.rm=TRUE)
                }
        }
  # Set colors, widths, types
  if (is.null(lnCols)) lnCols <- sample(colours(), strcnt)
  if (is.null(lnWds)) lnWds <- rep(1, strcnt)
  if (is.null(lnTyps)) lnTyps <- rep(1, strcnt)
  # Set labels
  if (is.null(labMods)) labMods <- paste0("Model", 1:strcnt)
  # Create plot
  plot(str1$POSIXct, str1[,modCol]-str1[1,modCol], typ='l', ylim=c(0, ymax),
        xlim=c(stdate, enddate), col=lnCols[1], lty=lnTyps[1], lwd=lnWds[1],
        xlab="", ylab="Accumulated flow (mm)", cex.axis=1.2, cex.lab=1.2)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (j in 2:length(modDfs)) {
                stri <- modDfs[[j]]
                stri <- subset(stri, stri$site_no==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                lines(stri$POSIXct, stri[,modCol]-stri[1,modCol], col=lnCols[j], lty=lnTyps[j], lwd=lnWds[j])
                }
        }
  lines(obs$POSIXct, obs[,obsCol]-obs[1,obsCol], col='black', lwd=2, lty=1)
  title(labTitle, cex.main=1.6)
  legend("topleft", c(labMods[1:strcnt], labObs),
                lty=c(lnTyps[1:strcnt],1), lwd=c(lnWds[1:strcnt],2),
                col=c(lnCols[1:strcnt], 'black'), cex=1.2)
}


PlotFlow <- function(n, modDfs, obs,
                        labMods=NULL,
                        labObs="Observed",
                        labTitle="Streamflow",
                        lnCols=NULL, lnWds=NULL, lnTyps=NULL,
                        stdate=NULL,
                        enddate=NULL,
                        modCol="q_cms", obsCol="mean_qcms",
			idCol="site_no"){
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  if (is.data.frame(modDfs)) {
        str1 <- modDfs
        strcnt <- 1
  } else if (is.list(modDfs)) {
        str1 <- modDfs[[1]]
        strcnt <- length(modDfs)
  } else {
        stop("modDfs must be a dataframe or a list of dataframes")
  }
  #print('------------------------------------------------------------------')
  if (is.data.table(str1)) str1<-data.frame(str1)
  # Subset by dates
  if (is.null(stdate)) stdate <- min(str1$POSIXct)
  if (is.null(enddate)) enddate <- max(str1$POSIXct)
  print(str1)
  str1 <- subset(str1, str1[,idCol]==n & str1$POSIXct>=stdate & str1$POSIXct<=enddate)
  if (is.data.table(obs)) {
        obs <- obs[get(idCol)==as.integer(n) & POSIXct>=stdate & POSIXct<=enddate,]
  	obs <- data.frame(obs)
  } else {
      	obs <- subset(obs, obs[,idCol]==n & obs$POSIXct>=stdate & obs$POSIXct<=enddate)
  }
  # Calculate maximum y val for plot limits
  ymax <- max(str1[,modCol], obs[,obsCol], na.rm=TRUE)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (stri in modDfs) {
		if (is.data.table(stri)) stri<-data.frame(stri)
        	stri <- subset(stri, stri[,idCol]==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
		ymax <- max(ymax, stri[,modCol], na.rm=TRUE)
                }
        }
  # Set colors, widths, types
  if (is.null(lnCols)) lnCols <- sample(colours(), strcnt)
  if (is.null(lnWds)) lnWds <- rep(1, strcnt)
  if (is.null(lnTyps)) lnTyps <- rep(1, strcnt)
  # Set labels
  if (is.null(labMods)) labMods <- paste0("Model", 1:strcnt)
  # Create plot
    plot(str1$POSIXct, str1[,modCol], typ='l', ylim=c(0, ymax),
          xlim=c(stdate, enddate), col=lnCols[1], lty=lnTyps[1], lwd=0,
          xlab="", ylab="Streamflow (m3/s)", cex.axis=1.2, cex.lab=1.2)
    lines(obs$POSIXct, obs[,obsCol], col='black', lwd=1.2, lty=1)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (j in 1:length(modDfs)) {
                stri <- modDfs[[j]]
		if (is.data.table(stri)) stri<-data.frame(stri)
                stri <- subset(stri, stri[,idCol]==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                lines(stri$POSIXct, stri[,modCol], col=lnCols[j], lty=lnTyps[j], lwd=lnWds[j])
                }
        }
  title(labTitle, cex.main=1.6)
  legend("topleft", c(labMods[1:strcnt], labObs),
                lty=c(lnTyps[1:strcnt],1), lwd=c(lnWds[1:strcnt],2),
                col=c(lnCols[1:strcnt], 'black'), cex=1.2,
                bg="white")
}

plotEnsFlowWObs <- function(n, modDfs, obs,
		            labObs=NULL,
			    title="Ensemble Streamflow",
			    startDate=NULL,
			    endDate=NULL,
		            outDir=NULL) {


	#Spaghetti plots
	#Subset data based on gage, date range
	dfTmp <- subset(modDfs,site_no==n & POSIXct >= startDate & POSIXct <= endDate)
	dates <- unique(dfTmp$POSIXct)
        nSteps <- length(dates)
	ensLab <- unique(modDfs$enstag)

	if (hydroEnsBiasCorr == 1){
		if (n == "RIODELCO") {
                	bias = 0.74
        	} else if (n == "CONMOGCO") {
                	bias = 1.16
        	} else if (n == "LOSORTCO") {
                	bias = 1.55
        	} else if (n == "SANORTCO") {
                	bias = 0.91
        	} else {
                	bias = 1.0
        	}
        } else {
		bias = 1.0
	}

	dfTmp$q_cfs = dfTmp$q_cfs * bias
   dfTmp$ACCFLOW_af = dfTmp$ACCFLOW_af*bias

	if (hydroEnsBaseFlowCorr == 1){
		startDateBaseFlow <- startDate
	}

	# Determine beginning dates, info for padding.
	if (padSteps > 0){
		dt <- as.numeric(difftime(dfTmp$POSIXct[2],dfTmp$POSIXct[1],units="hours"))
		nSteps <- nSteps + padSteps
		startDatePad <- startDate
		startDate <- startDate - dt*padSteps*3600
		dfPad <- data.frame(matrix(NA,nrow=padSteps*length(ensLab),ncol=length(names(dfTmp))))
		names(dfPad) <- names(dfTmp)
		dfPad$POSIXct <- as.POSIXct('1900-01-01 00:00',format='%Y-%m-%d %H:%M')
		count <- 1
		for (i in 1:padSteps){
			dateTmp <- startDate + dt*(i-1)*3600
			for (j in 1:length(ensLab)){
				dfPad$POSIXct[count] <- as.POSIXct(dateTmp,format='%Y-%m-%d %H:%M')
				dfPad$enstag[count] <- ensLab[j]
				#dfPad$st_lon[count] <- unique(dfTmp$st_lon)
				#dfPad$st_lat[count] <- unique(dfTmp$st_lat)
				#dfPad$st_id[count] <- unique(dfTmp$st_id)
				dfPad$tag[count] <- unique(dfTmp$tag)[1]
				dfPad$site_no[count] <- n
   			count <- count + 1
			}
		}
		# Bind to existing dfTmp 
		dfTmp <- rbind(dfPad,dfTmp)
		# Reset dates array
		dates <- unique(dfTmp$POSIXct)
	}

	if (hydroEnsBaseFlowCorr == 1){
		for (i in 1:nSteps){
			dfTmp1a <- subset(dfTmp, POSIXct == dates[i])

			indReplace <- which(dfTmp$POSIXct == dates[i])
			# First store observation at beginning of forecast period.
			if (i == 1){
				indTmp <- which(obs$site_no == n & obs$POSIXct == startDateBaseFlow)
            obsBaseFlow <- obs$q_cms[indTmp[1]]*35.3147
         }

			# Next, calculate minimum ESP forecast value.
			minCfs <- min(dfTmp1a$q_cfs)

			# Next, calculate difference between minimum ESP forecast value and observation from
         # the beginning of the forecast period. Apply that value to ALL ESP forecast values.
         # If modeled streamflow goes below zero, set it to 0. This is for cases where observations
         # have already gone to zero at the beginning of the forecast time period.
         for (j in 1:length(dfTmp1a$q_cfs)){
            diffTmp <- minCfs - obsBaseFlow
            if (!is.na(dfTmp1a$q_cfs[j])){
					if (diffTmp >= 0.0){
               	dfTmp1a$q_cfs[j] <- dfTmp1a$q_cfs[j] - diffTmp
					}
               if (dfTmp1a$q_cfs[j] < 0.0){
                  dfTmp1a$q_cfs[j] <- 0.0
               }
            }
         }

			# Place data back into data frame
			dfTmp$q_cfs[indReplace] <- dfTmp1a$q_cfs
		}

		# Re-calculate accumulated model runoff using updated streamflow values
		for (i in 1:length(ensLab)){
			indTmp <- which(dfTmp$enstag == ensLab[i] & !is.na(dfTmp$q_cfs) & dfTmp$site_no == n)
			cfsTmp <- dfTmp$q_cfs[indTmp]
			posixTmp <- dfTmp$POSIXct[indTmp]
			afTmp <- dfTmp$q_af[indTmp]
			for (j in 1:length(posixTmp)){
				if (j == 1){
					dtSec <- 0.0
				} else {
					dtSec <- as.numeric(difftime(posixTmp[j],posixTmp[j-1],units='secs'))
				}
				afTmp[j] <- ((cfsTmp[j]*dtSec)/43559.9)
			}
			dfTmp$q_af[indTmp] <- afTmp
			dfTmp$ACCFLOW_af[indTmp] <- cumsum(afTmp) 
		}
	}
	# Set accumulated acre-feet to thousands of acre-feet
	dfTmp$ACCFLOW_af <- dfTmp$ACCFLOW_af/1000.0

	print(n)
	# Create df with observations and modeled values for raster hydrograph
	dfTmp3 = data.frame(matrix(NA,nrow=nSteps*(length(ensLab)+1),ncol=3))
	names(dfTmp3) <- c('POSIXct','tag','q_cfs')
	dfTmp3$POSIXct <- as.POSIXct('1900-01-01 00:00',format='%Y-%m-%d %H:%M')
	countTmp1 <- 1
	for (i in 1:nSteps){
		for (j in 1:length(ensLab)){
			ind <- which(modDfs$site_no == n & modDfs$enstag == ensLab[j] & modDfs$POSIXct == dates[i])
			if (length(ind) != 0){
				dfTmp3$q_cfs[countTmp1] <- modDfs$q_cfs[ind]
			}
			dfTmp3$POSIXct[countTmp1] <- dates[i]
			dfTmp3$tag[countTmp1] <- ensLab[j]
			countTmp1 <- countTmp1 + 1
		}

		ind <- which(obs$site_no == n & strftime(obs$POSIXct,"%Y-%m-%d %H:%M") == strftime(dates[i],"%Y-%m-%d %H:%M"))
		if (length(ind) != 0){
			dfTmp3$POSIXct[countTmp1] <- dates[i] 
			dfTmp3$tag[countTmp1] <- 'Obs'
			dfTmp3$q_cfs[countTmp1] <- obs$q_cms[ind[1]]*35.3147
		}
		countTmp1 <- countTmp1 + 1

	}

	# Remove any erroneous NA values
	dfTmp3 <- subset(dfTmp3, !is.na(tag))

	# If no valid observations present, set plot flag to 0
	indCheck <- which(dfTmp3$tag == "Obs")
	if (length(indCheck == 0)){
		plotFlag <- 1
	} else {
		plotFlag <- 0
	}

	# Calculate maximum value for plotting purposes.
	yMax <- 1.2*max(dfTmp3$q_cfs)

	# Spread plots
	spreadDf <- data.frame(matrix(NA, nrow=nSteps,ncol=22))
	names(spreadDf) <- c('st_id','st_lon','st_lat','POSIXct','site_no','tag',
			     'q25','q50','q75','q0','q100','mean',
			     'af25','af50','af75','af0','af100',
			     'mean_af','median_af','ObsCFS','ObsAF','ObsAccAF')
	spreadDf$st_id <- NA 
	spreadDf$st_lon <- NA 
	spreadDf$st_lat <- NA 
	spreadDf$POSIXct <- as.POSIXct('1900-01-01 00:00',format='%Y-%m-%d %H:%M')
	spreadDf$site_no <- NA 
	spreadDf$tag <- NA 
	spreadDf$q25 <- NA
	spreadDf$q50 <- NA
	spreadDf$q75 <- NA
	spreadDf$q0 <- NA
	spreadDf$q100 <- NA
	spreadDf$mean <- NA
	spreadDf$ObsCFS <- NA
	spreadDf$af25 <- NA
	spreadDf$af50 <- NA
	spreadDf$af75 <- NA
	spreadDf$af0 <- NA
	spreadDf$af100 <- NA
	spreadDf$mean_af <- NA
	spreadDf$median_af <- NA
	spreadDf$ObsAF <- NA
	spreadDf$ObsAccAF <- NA

	for (i in 1:nSteps) {
		dfTmp2 <- subset(dfTmp, POSIXct == dates[i])

		qCalc <- quantile(dfTmp2$q_cfs, probs=seq(0,1,0.25), na.rm = TRUE)
		afCalc <- quantile(dfTmp2$ACCFLOW_af, probs=seq(0,1,0.25), na.rm = TRUE)
		#spreadDf$st_id[i] <- dfTmp2$st_id[1]
		#spreadDf$st_lon[i] <- dfTmp2$st_lon[1]
		#spreadDf$st_lat[i] <- dfTmp2$st_lat[1]
		spreadDf$POSIXct[i] <- dfTmp2$POSIXct[1]
		spreadDf$site_no[i] <- dfTmp2$site_no[1]
		spreadDf$tag[i] <- dfTmp2$tag[1]
		spreadDf$q25[i] <- qCalc[[2]]
		spreadDf$q50[i] <- qCalc[[3]]
		spreadDf$q75[i] <- qCalc[[4]]
		spreadDf$q0[i] <- qCalc[[1]]
		spreadDf$q100[i] <- qCalc[[5]]
		spreadDf$mean[i] <- mean(dfTmp2$q_cfs)
		spreadDf$af25[i] <- afCalc[[2]]
		spreadDf$af50[i] <- afCalc[[3]]
		spreadDf$af75[i] <- afCalc[[4]]
		spreadDf$af0[i] <- afCalc[[1]]
		spreadDf$af100[i] <- afCalc[[5]]
		spreadDf$mean_af[i] <- mean(dfTmp2$ACCFLOW_af)
		spreadDf$median_af[i] <- median(dfTmp2$ACCFLOW_af)

		# Observations
      ind <- which(obs$site_no == n & strftime(obs$POSIXct,"%Y-%m-%d %H:%M") == strftime(dates[i],"%Y-%m-%d %H:%M"))
      if (length(ind) != 0){
         spreadDf$ObsCFS[i] <- obs$q_cms[ind[1]]*35.3147
      }
      # Calculate volume of water in terms of acre-feet
      if (i == 1){
         spreadDf$ObsAF[i] <- 0.0
      } else {
         dtSec <- as.numeric(difftime(spreadDf$POSIXct[i],spreadDf$POSIXct[i-1],units='secs'))
         if (!is.na(spreadDf$ObsCFS[i])){
                                spreadDf$ObsAF[i] <- ((spreadDf$ObsCFS[i]*dtSec)/43559.9)/1000.0
                        }
      }

	}

	# Calculate cumulative observed flow in thousands acre-feet
	tmpFlow <- spreadDf$ObsAF
	spreadDf$ObsAccAF[!is.na(spreadDf$ObsAF)] <- cumsum(spreadDf$ObsAF[!is.na(spreadDf$ObsAF)])

	# If padding is present, add observed accumulated flow to modeled accumulated flow.
	if (padSteps > 0 ) {
		ind <- which(spreadDf$POSIXct == startDatePad)
		if (length(ind) != 0) {
			obsAccPad <- spreadDf$ObsAccAF[ind]
		}
		if (!is.na(obsAccPad)){
			spreadDf$af25[!is.na(spreadDf$af25)] <- spreadDf$af25[!is.na(spreadDf$af25)] + obsAccPad
			spreadDf$af50[!is.na(spreadDf$af50)] <- spreadDf$af50[!is.na(spreadDf$af50)] + obsAccPad
			spreadDf$af75[!is.na(spreadDf$af75)] <- spreadDf$af75[!is.na(spreadDf$af75)] + obsAccPad
			spreadDf$af0[!is.na(spreadDf$af0)] <- spreadDf$af0[!is.na(spreadDf$af0)] + obsAccPad
			spreadDf$af100[!is.na(spreadDf$af100)] <- spreadDf$af100[!is.na(spreadDf$af100)] + obsAccPad
			spreadDf$median_af[!is.na(spreadDf$median_af)] <- spreadDf$median_af[!is.na(spreadDf$median_af)] + obsAccPad
			spreadDf$mean_af[!is.na(spreadDf$mean_af)] <- spreadDf$mean_af[!is.na(spreadDf$mean_af)] + obsAccPad
			dfTmp$ACCFLOW_af[!is.na(dfTmp$ACCFLOW_af)] <- dfTmp$ACCFLOW_af[!is.na(dfTmp$ACCFLOW_af)] + obsAccPad
		}
	}

	# Calculate maximum accumulated flow values for plotting purposes
	yMaxAF <- 1.2*max(dfTmp$ACCFLOW_af[!is.na(dfTmp$ACCFLOW_af)])
	yMaxAFCheck <- 1.2*max(spreadDf$ObsAccAF[!is.na(spreadDf$ObsAccAF)])
	if (yMaxAFCheck > yMaxAF){
		yMaxAF <- yMaxAFCheck
	}

	spreadDf$Date <- as.Date(spreadDf$POSIXct)
	dfTmp$Date <- as.Date(dfTmp$POSIXct)

	dfTmp3$q_cfs = dfTmp3$q_cfs * bias

	if (plotFlag == 1) {
		colOut <- c('red','black')
		gg <- ggplot() + 
	      		geom_smooth(data=spreadDf, aes(x=POSIXct,y=q50,ymin=q25,ymax=q75,color="Mean Modeled"),stat="identity",alpha=1) +
	      		geom_line(data=spreadDf, aes(x=POSIXct,y=ObsCFS,color='Observed'),size=1.2,linetype='dashed') +
	      		scale_color_manual(name='Model Run',values = colOut) +  
	      		ggtitle(title) + xlab('Date') + ylab('Streamflow (cfs)') + ylim(0,yMax)
		fileOutPath <- paste0(outDir,'/streamflow_spread_',n,'_',strftime(startDate,"%Y%m%d%H"),
                	        '_',strftime(endDate,"%Y%m%d%H"),'.png')
		ggsave(filename=fileOutPath, plot = gg)

		gg <- ggplot() +
              		geom_smooth(data=spreadDf, aes(x=POSIXct,y=af50,ymin=af25,ymax=af75,color="Mean Modeled"),stat="identity",alpha=1) +
	      		geom_line(data=spreadDf, aes(x=POSIXct,y=ObsAccAF,color='Observed'),size=1.2,linetype='dashed') + 
              		scale_color_manual(name='Model Run',values = colOut) +
              		ggtitle(title) + xlab('Date') + ylab('Accumulated Runoff (thousands acre-feet)') + ylim(0,yMaxAF)
        	fileOutPath <- paste0(outDir,'/acc_runoff_af_spread_',n,'_',strftime(startDate,"%Y%m%d%H"),
                       	'_',strftime(endDate,"%Y%m%d%H"),'.png')
        	ggsave(filename=fileOutPath, plot = gg)

		#Spaghetti plots
		numColor = length(unique(dfTmp$enstag)) + 1
		colOut <- colOutList[1:numColor]
		colOut[length(colOut)] <- 'black'
		# Re-order to account for alphabetical order
		labels1 <- c(unique(dfTmp$enstag),'Observed')
		order1 <- order(labels1)
		colOut <- colOut[order1]
		gg <- ggplot(data=dfTmp,aes(x=POSIXct,y=q_cfs,color=enstag)) + geom_line() + 
	      		geom_line(data=spreadDf, aes(x=POSIXct,y=ObsCFS,color='Observed'),size=1.2,linetype='dashed') + 
	      		scale_color_manual(name='Model Run',values=colOut) + 
	      		ggtitle(title) + xlab('Date') + ylab('Streamflow (cfs)') + ylim(0,yMax)  
        	fileOutPath <- paste0(outDir,'/streamflow_spaghetti_',n,'_',strftime(startDate,"%Y%m%d%H"),
                	        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        	ggsave(filename = fileOutPath, plot = gg)

		gg <- ggplot(data=dfTmp,aes(x=POSIXct,y=ACCFLOW_af,color=enstag)) + geom_line() +
	      		geom_line(data=spreadDf, aes(x=POSIXct,y=ObsAccAF,color='Observed'),size=1.2,linetype='dashed') + 
              		scale_color_manual(name='Model Run',values = colOut) +
              		ggtitle(title) + xlab('Date') + ylab('Accumulated Runoff (thousands acre-feet') + ylim(0,yMaxAF)
        	fileOutPath <- paste0(outDir,'/acc_runoff_af_spaghetti_',n,'_',strftime(startDate,"%Y%m%d%H"),
                	        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        	ggsave(filename = fileOutPath, plot = gg)

		# Produce hydrograph raster
		gg <- ggplot(dfTmp3, aes(x=POSIXct, y=tag, fill=q_cfs)) + geom_raster() +
	      		scale_fill_gradientn(colours = rainbow(10)) + 
	      		ggtitle(title) + xlab('Date') + ylab('Ensemble')  
	      		#scale_x_date(labels = date_format("%d:%H")) 
		fileOutPath <- paste0(outDir,'/streamflow_raster_hydrograph_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
		ggsave(filename=fileOutPath, plot=gg)

		gg <- ggplot(dfTmp, aes(x=POSIXct, y=enstag, fill=ACCFLOW_af)) + geom_raster() +
              		scale_fill_gradientn(colours = rainbow(10)) +
              		ggtitle(title) + xlab('Date') + ylab('Ensemble')
        	fileOutPath <- paste0(outDir,'/acc_flow_af_raster_hydrograph_',n,'_',strftime(startDate,"%Y%m%d%H"),
        	                '_',strftime(endDate,"%Y%m%d%H"),'.png')
 	       	ggsave(filename=fileOutPath, plot=gg)

		#LK TMP PRINT STATS TO SCREEN
        	print(paste0('BAINS: ',n))
        	print(paste0('MEAN ACCUMULATED RUNOFF (kaf): ',max(spreadDf$mean_af[!is.na(spreadDf$mean_af)])))
	        print(paste0('MEDIAN ACCUMULATED RUNOFF (kaf): ',max(spreadDf$median_af[!is.na(spreadDf$median_af)])))

	}
}

plotEnsFlow <- function(n, modDfs,
                        title="Ensemble Streamflow",
                        startDate=NULL,
                        endDate=NULL,
                        outDir=NULL) {


        #Spaghetti plots
        #Subset data based on gage, date range
        dfTmp <- subset(modDfs,site_no==n & POSIXct >= startDate & POSIXct <= endDate)
        dates <- unique(dfTmp$POSIXct)
        nSteps <- length(dates)
        ensLab <- unique(modDfs$enstag)

	# Set accumulated acre-feet to thousands of acre-feet
	dfTmp$ACCFLOW_af <- dfTmp$ACCFLOW_af/1000.0

	# Establish max values
	yMax <- 1.2*max(dfTmp$q_cfs)
	yMaxAF <- 1.2*max(dfTmp$ACCFLOW_af)

	# Spread plots
        spreadDf <- data.frame(matrix(NA, nrow=nSteps,ncol=19))
        names(spreadDf) <- c('st_id','st_lon','st_lat','POSIXct','site_no','tag',
                             'q25','q50','q75','q0','q100','mean',
			     'af25','af50','af75','af0','af100','mean_af','median_af')
        spreadDf$st_id <- NA
        spreadDf$st_lon <- NA
        spreadDf$st_lat <- NA
        spreadDf$POSIXct <- as.POSIXct('1900-01-01 00:00',format='%Y-%m-%d %H:%M')
        spreadDf$site_no <- NA
        spreadDf$tag <- NA
        spreadDf$q25 <- NA
        spreadDf$q50 <- NA
        spreadDf$q75 <- NA
        spreadDf$q0 <- NA
        spreadDf$q100 <- NA
        spreadDf$mean <- NA
	spreadDf$af25 <- NA
	spreadDf$af50 <- NA
	spreadDf$af75 <- NA
	spreadDf$af0 <- NA
	spreadDf$af100 <- NA
	spreadDf$mean_af <- NA
	spreadDf$median_af <- NA

	if (hydroEnsBiasCorr == 1){
                if (n == "RIODELCO") {
                        bias = 0.74
                } else if (n == "CONMOGCO") {
                        bias = 1.16
                } else if (n == "LOSORTCO") {
                        bias = 1.55
                } else if (n == "SANORTCO") {
                        bias = 0.91
                } else {
                        bias = 1.0
                }
        } else {
                bias = 1.0
        }

	dfTmp$q_cfs = dfTmp$q_cfs * bias
        dfTmp$ACCFLOW_af = dfTmp$ACCFLOW_af*bias

        for (i in 1:nSteps) {
                dfTmp2 <- subset(dfTmp, POSIXct == dates[i])
                qCalc <- quantile(dfTmp2$q_cfs, probs=seq(0,1,0.25), na.rm = TRUE)
		afCalc <- quantile(dfTmp2$ACCFLOW_af, probs=seq(0,1,0.25), na.rm = TRUE)
                #spreadDf$site_id[i] <- dfTmp2$st_id[1]
                #spreadDf$st_lon[i] <- dfTmp2$st_lon[1]
                #spreadDf$st_lat[i] <- dfTmp2$st_lat[1]
                spreadDf$POSIXct[i] <- dfTmp2$POSIXct[1]
                spreadDf$site_no[i] <- dfTmp2$site_no[1]
                spreadDf$tag[i] <- dfTmp2$tag[1]
                spreadDf$q25[i] <- qCalc[[2]]
                spreadDf$q50[i] <- qCalc[[3]]
                spreadDf$q75[i] <- qCalc[[4]]
                spreadDf$q0[i] <- qCalc[[1]]
                spreadDf$q100[i] <- qCalc[[5]]
                spreadDf$mean[i] <- mean(dfTmp2$q_cfs)
		spreadDf$af25[i] <- afCalc[[2]]
		spreadDf$af50[i] <- afCalc[[3]]
		spreadDf$af75[i] <- afCalc[[4]]
		spreadDf$af0[i] <- afCalc[[1]]
		spreadDf$af100[i] <- afCalc[[5]]
		spreadDf$mean_af[i] <- mean(dfTmp2$ACCFLOW_af)
		spreadDf$median_af[i] <- median(dfTmp2$ACCFLOW_af)
        }

	#LK TMP PRINT STATS TO SCREEN
	print(paste0('BAINS: ',n))
	print(paste0('MEAN ACCUMULATED RUNOFF (kaf): ',max(spreadDf$mean_af)))
	print(paste0('MEDIAN ACCUMULATED RUNOFF (kaf): ',max(spreadDf$median_af)))

	dfTmp3$q_cfs = dfTmp3$q_cfs * bias

        spreadDf$Date <- as.Date(spreadDf$POSIXct)
        dfTmp$Date <- as.Date(dfTmp$POSIXct)
        colOut <- c('red')
        gg <- ggplot() +
              geom_smooth(data=spreadDf, aes(x=POSIXct,y=q50,ymin=q25,ymax=q75,color=site_no),stat="identity",alpha=1) +
              scale_color_manual(name='Model Run',values = colOut,label=c('Mean Modeled')) +
              ggtitle(title) + xlab('Date') + ylab('Streamflow (cfs)') + ylim(0,yMax)
        fileOutPath <- paste0(outDir,'/streamflow_spread_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename=fileOutPath, plot = gg)

	gg <- ggplot() +
              geom_smooth(data=spreadDf, aes(x=POSIXct,y=af50,ymin=af25,ymax=af75,color=site_no),stat="identity",alpha=1) +
              scale_color_manual(name='Model Run',values = colOut,label=c('Mean Modeled')) +
              ggtitle(title) + xlab('Date') + ylab('Accumulated Runoff (thousands acre-feet)') + ylim(0,yMaxAF)
        fileOutPath <- paste0(outDir,'/acc_runoff_af_spread_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename=fileOutPath, plot = gg)

	#Spaghetti plots
        numColor = length(unique(dfTmp$enstag))
	colOut <- colOutList[1:numColor]
        colOut[length(colOut)] <- 'black'
        gg <- ggplot(data=dfTmp,aes(x=POSIXct,y=q_cfs,color=enstag)) + geom_line() +
              scale_color_manual(name='Model Run',values = colOut,label=c(unique(dfTmp$enstag))) +
              ggtitle(title) + xlab('Date') + ylab('Streamflow (cfs)') + ylim(0,yMax)
        fileOutPath <- paste0(outDir,'/streamflow_spaghetti_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename = fileOutPath, plot = gg)

	gg <- ggplot(data=dfTmp,aes(x=POSIXct,y=ACCFLOW_af,color=enstag)) + geom_line() +
              scale_color_manual(name='Model Run',values = colOut,label=c(unique(dfTmp$enstag))) +
              ggtitle(title) + xlab('Date') + ylab('Accumulated Runoff (thousands acre-feet') + ylim(0,yMaxAF)
        fileOutPath <- paste0(outDir,'/acc_runoff_af_spaghetti_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename = fileOutPath, plot = gg)

        # Produce hydrograph raster
        gg <- ggplot(dfTmp, aes(x=POSIXct, y=enstag, fill=q_cfs)) + geom_raster() +
              scale_fill_gradientn(colours = rainbow(10)) +
              ggtitle(title) + xlab('Date') + ylab('Ensemble')
        fileOutPath <- paste0(outDir,'/streamflow_raster_hydrograph_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename=fileOutPath, plot=gg)

	gg <- ggplot(dfTmp, aes(x=POSIXct, y=enstag, fill=ACCFLOW_af)) + geom_raster() +
              scale_fill_gradientn(colours = rainbow(10)) +
              ggtitle(title) + xlab('Date') + ylab('Ensemble')
        fileOutPath <- paste0(outDir,'/acc_flow_af_raster_hydrograph_',n,'_',strftime(startDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename=fileOutPath, plot=gg)
}

plotEnsSWE <- function(n, modDfs,
		       title='Basin SWE',
		       stDate=NULL,
		       endDate=NULL,
		       outDir='./') {

	# Subset data based on dates and basin
	dfTmp <- subset(modDfs,statArg==n & POSIXct >= stDate & POSIXct <= endDate)
	
	# Convert SWE volume from cubic meters to thousands of acre-feet
	dfTmp$SNEQV_SUM <- (dfTmp$SNEQV_SUM/1233.48)/1000.0

	yMax <- max(dfTmp$SNEQV_SUM)

	dates <- unique(dfTmp$POSIXct)
	nSteps <- length(dates)

	# Spread plots
        spreadDf <- data.frame(matrix(NA, nrow=nSteps,ncol=18))
        names(spreadDf) <- c('POSIXct','basin',
                             'q25','q50','q75','q0','q100','mean','median')
        spreadDf$POSIXct <- as.POSIXct('1900-01-01 00:00',format='%Y-%m-%d %H:%M')
        spreadDf$basin <- NA
        spreadDf$q25 <- NA
        spreadDf$q50 <- NA
        spreadDf$q75 <- NA
        spreadDf$q0 <- NA
        spreadDf$q100 <- NA
        spreadDf$mean <- NA
        spreadDf$median <- NA

        for (i in 1:nSteps) {
                dfTmp2 <- subset(dfTmp, POSIXct == dates[i])
                qCalc <- quantile(dfTmp2$SNEQV_SUM, probs=seq(0,1,0.25), na.rm = TRUE)
                spreadDf$POSIXct[i] <- dfTmp2$POSIXct[1]
                spreadDf$basin[i] <- dfTmp2$statArg[1]
                spreadDf$q25[i] <- qCalc[[2]]
                spreadDf$q50[i] <- qCalc[[3]]
                spreadDf$q75[i] <- qCalc[[4]]
                spreadDf$q0[i] <- qCalc[[1]]
                spreadDf$q100[i] <- qCalc[[5]]
                spreadDf$mean[i] <- mean(dfTmp2$SNEQV_SUM)
                spreadDf$median[i] <- median(dfTmp2$SNEQV_SUM)
        }

	#LK TMP PRINT STATS TO SCREEN
        print(paste0('BAINS: ',n))
        print(paste0('MEAN MAXIMUM SWE VOLUME (kaf): ',max(spreadDf$mean)))
        print(paste0('MEDIAN MAXIMUM SWE VOLUME (kaf): ',max(spreadDf$median)))

        spreadDf$Date <- as.Date(spreadDf$POSIXct)
        dfTmp$Date <- as.Date(dfTmp$POSIXct)
        colOut <- c('red')
	gg <- ggplot() +
              geom_smooth(data=spreadDf, aes(x=POSIXct,y=q50,ymin=q25,ymax=q75,color=basin),stat="identity",alpha=1) +
              scale_color_manual(name='Model Run',values = colOut,label=c('Mean Modeled')) +
              ggtitle(title) + xlab('Date') + ylab('SWE Volume (thousands acre-feet)') + ylim(0,yMax)
        fileOutPath <- paste0(outDir,'/Basin_SWE_spread_',n,'_',strftime(stDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename=fileOutPath, plot = gg)

	# Product spaghetti plots
	numColor <- length(unique(dfTmp$enstag))
	colOut <- colOutList[1:numColor]
	gg <- ggplot(data=dfTmp,aes(x=POSIXct,y=SNEQV_SUM,color=enstag)) + geom_line() + 
	      scale_color_manual(name='Model Run',values = colOut,label=c(unique(dfTmp$enstag))) +
	      ggtitle(title) + xlab('Date') + ylab('SWE Volume (thousands acre-feet)') + ylim(0,yMax)
	fileOutPath <- paste0(outDir,'/SWE_Volume_spaghetti_',n,'_',strftime(stDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename = fileOutPath, plot = gg)

	# Produce raster
	gg <- ggplot(dfTmp, aes(x=POSIXct, y=enstag, fill=SNEQV_SUM)) + geom_raster() +
              scale_fill_gradientn(colours = rainbow(10)) +
              ggtitle(title) + xlab('Date') + ylab('Ensemble')
        fileOutPath <- paste0(outDir,'/Basin_SWE_Volume_raster_hydrograph_',n,'_',strftime(stDate,"%Y%m%d%H"),
                        '_',strftime(endDate,"%Y%m%d%H"),'.png')
        ggsave(filename=fileOutPath, plot=gg)

}
PlotFlowSwe <- function(n, modDfs, lsmDfs, obs,
                        labMods=NULL,
                        labObs="Observed",
                        labTitle="Streamflow with Basin-Mean SWE",
                        lnCols=NULL, lnWds=NULL, lnTyps=NULL,
                        stdate=NULL,
                        enddate=NULL,
                        modCol="q_cms", lsmCol="SNEQV", obsCol="mean_qcms") {
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  if (is.data.frame(modDfs)) {
        str1 <- modDfs
        strcnt <- 1
  } else if (is.list(modDfs)) {
        str1 <- modDfs[[1]]
        strcnt <- length(modDfs)
  } else {
        stop("modDfs must be a dataframe or a list of dataframes")
  }
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  if (is.data.frame(lsmDfs)) {
        lsm1 <- lsmDfs
        lsmcnt <- 1
  } else if (is.list(lsmDfs)) {
        lsm1 <- lsmDfs[[1]]
        lsmcnt <- length(lsmDfs)
  } else {
        stop("lsmDfs must be a dataframe or a list of dataframes")
  }
  # Subset by dates
  if (is.null(stdate)) stdate <- min(str1$POSIXct)
  if (is.null(enddate)) enddate <- max(str1$POSIXct)
  str1 <- subset(str1, str1[idCol]==n & str1$POSIXct>=stdate & str1$POSIXct<=enddate)
  lsm1 <- subset(lsm1, lsm1$statArg==n & lsm1$POSIXct>=stdate & lsm1$POSIXct<=enddate)
  obs <- subset(obs, obs[idCol]==n & obs$POSIXct>=stdate & obs$POSIXct<=enddate)
  # Calculate maximum y val for plot limits
  ymax <- max(str1[,modCol], obs[,obsCol], na.rm=TRUE)
  ymax_lsm <- max(lsm1[,lsmCol], na.rm=TRUE)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (stri in modDfs) {
                stri <- subset(stri, stri[idCol]==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                ymax <- max(ymax, stri[,modCol], na.rm=TRUE)
                }   
        }   
  if (!is.data.frame(lsmDfs) & is.list(lsmDfs) & length(lsmDfs)>1) {
        for (lsmi in lsmDfs) {
                lsmi <- subset(lsmi, lsmi$statArg==n & lsmi$POSIXct>=stdate & lsmi$POSIXct<=enddate)
                ymax_lsm <- max(ymax_lsm, lsmi[,lsmCol], na.rm=TRUE)
                }
        }
  # Set colors, widths, types
  if (is.null(lnCols)) lnCols <- sample(colours(), strcnt)
  if (is.null(lnWds)) lnWds <- rep(1, strcnt)
  if (is.null(lnTyps)) lnTyps <- rep(1, strcnt)
  # Set labels
  if (is.null(labMods)) labMods <- paste0("Model", 1:strcnt)
  # Create plot
  par(mar=c(5,4,4,5)+.1)
  plot(str1$POSIXct, str1[,modCol], typ='l', ylim=c(0, ymax),
        xlim=c(stdate, enddate), col=lnCols[1], lty=lnTyps[1], lwd=lnWds[1],
        xlab="", ylab="Streamflow (m3/s)", cex.axis=1.2, cex.lab=1.2)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (j in 2:length(modDfs)) {
                stri <- modDfs[[j]]
                stri <- subset(stri, stri[idCol]==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                lines(stri$POSIXct, stri[,modCol], col=lnCols[j], lty=lnTyps[j], lwd=lnWds[j])
                }   
        }   
  lines(obs$POSIXct, obs[,obsCol], col='black', lwd=2, lty=1)
  par(new=TRUE)
  plot(lsm1$POSIXct, lsm1[,lsmCol], col=lnCols[1], lty=2, lwd=lnWds[1], typ='l',
        ylim=c(0, ymax_lsm),
        xaxt="n", yaxt="n", xlab="", ylab="")
  axis(4)
  mtext("SWE (mm)", side=4, line=3)
  if (!is.data.frame(lsmDfs) & is.list(lsmDfs) & length(lsmDfs)>1) {
        for (j in 2:length(lsmDfs)) {
                lsmi <- lsmDfs[[j]]
                lsmi <- subset(lsmi, lsmi$statArg==n)
                lines(lsmi$POSIXct, lsmi[,lsmCol], col=lnCols[j], lty=2, lwd=lnWds[j])
                }
        }
  title(labTitle, cex.main=1.6)
  legend("topleft", c(labMods[1:strcnt], labObs, "", "Streamflow (m3/s)", "SWE (mm)"),
                lty=c(lnTyps[1:strcnt],1,1,1,2), lwd=c(lnWds[1:strcnt],2,1,2,2),
                col=c(lnCols[1:strcnt], 'black','white','grey40','grey40'), cex=1.2,
                bg="white")
}


PlotFlowLsm <- function(n, modDf, lsmDf, obs, 
                        labMods=NULL,
                        labObs="Observed",
                        labTitle="Streamflow",
                        lnCols=NULL, lnWds=NULL, lnTyps=NULL,
                        stdate=NULL,
                        enddate=NULL,
                        modCol="q_cms", obsCol="mean_qcms",
                        idCol="site_no", tsSecs=86400, areaSqKm, ngage=NULL) {
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  str1 <- modDf
  lsm1 <- lsmDf
  if (is.data.table(str1)) str1<-data.frame(str1)
  if (is.data.table(lsm1)) lsm1<-data.frame(lsm1)
  # Subset by dates
  if (is.null(stdate)) stdate <- min(str1$POSIXct)
  if (is.null(enddate)) enddate <- max(str1$POSIXct)
  ngage <- ifelse(!is.null(ngage), ngage, n)
  str1 <- subset(str1, str1[idCol]==n & str1$POSIXct>=stdate & str1$POSIXct<=enddate)
  lsm1 <- subset(lsm1, lsm1["statArg"]==ngage & lsm1$POSIXct>=stdate & lsm1$POSIXct<=enddate)
  if (is.data.table(obs)) {
        obs <- obs[get(idCol)==n & POSIXct>=stdate & POSIXct<=enddate,]
        obs <- data.frame(obs)
  } else {
        obs <- subset(obs, obs[idCol]==n & obs$POSIXct>=stdate & obs$POSIXct<=enddate)
  }
  # Calculate maximum y val for plot limits
  ymax <- max(str1[,modCol], obs[,obsCol], na.rm=TRUE)
  # Set colors, widths, types
  if (is.null(lnCols)) lnCols <- sample(colours(), 2)
  if (is.null(lnWds)) lnWds <- rep(1, 2)
  if (is.null(lnTyps)) lnTyps <- rep(1, 2)
  # Set labels
  if (is.null(labMods)) labMods <- paste0("Model", 1)
  # Create plot
  plot(str1$POSIXct, str1[,modCol], typ='l', ylim=c(0, ymax),
        xlim=c(stdate, enddate), col=lnCols[1], lty=lnTyps[1], lwd=lnWds[1],
        xlab="", ylab="Streamflow (m3/s)", cex.axis=1.2, cex.lab=1.2)
  with(lsm1, lines(POSIXct, (DEL_UGDRNOFF+DEL_SFCRNOFF)/tsSecs/1000*areaSqKm*1000*1000, 
	col=lnCols[2], lty=lnTyps[2], lwd=lnWds[2]))
  lines(obs$POSIXct, obs[,obsCol], col='black', lwd=2, lty=1)
  title(labTitle, cex.main=1.6)
  legend("topleft", c(labMods, "LSM Runoff", labObs),
                lty=c(lnTyps, 1), lwd=c(lnWds, 2),
                col=c(lnCols, 'black'), cex=1.2,
                bg="white")
}

PlotBasSnoMetrics <- function(n, modDfs,
			      var=NULL,
			      title="",
			      xlab="",
		              ylab="",
			      fileOut=""){
  #Plot data depending on exact metric of interest
  if (var == 1) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=snow_area_km,color=product)) + ggplot2::geom_line() +
	  ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else if (var == 2) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=snow_cover_fraction,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
	  scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else if (var == 3) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=snow_volume_cub_meters,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else if (var == 4) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=snow_volume_acre_feet,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else if (var == 5) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=mean_snow_line_meters,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else if (var == 6) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=mean_snow_line_feet,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else if (var == 7) {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=mean_swe_mm,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  } else {
    gp <- ggplot2::ggplot(modDfs,ggplot2::aes(x=Date,y=max_swe_mm,color=product)) + ggplot2::geom_line() +
          ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          scale_x_date(labels = date_format("%m/%y")) + theme(plot.title = element_text(size = 12))
  }

  # Save output
  ggsave(filename = fileOut, plot = gp)

}

