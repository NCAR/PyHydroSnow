###################################################
##      Main Control Script to process obs       ##
###################################################

## Post-process Ameriflux data?
postprocAmf <- TRUE

        # If TRUE, specify Ameriflux data file:
        dataAmfIn <- '/glade/p/ral/RHAP/adugger/CONUS_IOC/OBS/AMF/obs_AMF_1998_current.Rdata_BU'
	# Specify output data file (if NULL, will overwrite in file):
	dataAmfOut <- '/glade/p/ral/RHAP/adugger/CONUS_IOC/OBS/AMF/obs_AMF_1998_current.Rdata'


## Post-process USGS streamflow data?
postprocUsgs <- FALSE

        # If TRUE, specify Ameriflux data file:
        dataUsgs <- '/glade/p/ral/RHAP/adugger/CONUS_IOC/OBS/USGS/'
        # Specify output data file (if NULL, will overwrite in file):
        dataUsgsOut <- NULL

## Use parallel processing? Specify number of cores:
ncores <- 15


###########################################################################################
## RUN (do not change anything below this line)

library(rwrfhydro)
library(data.table)

# Multi-core
parallelFlag <- FALSE
if (ncores>1) {
        library(doParallel)
        cl <- makeForkCluster(ncores)
        registerDoParallel(cl)
        parallelFlag <- TRUE
}


###----------- AMERIFLUX ------------###
if (postprocAmf) {
	load(dataAmfIn)
	if (is.null(dataAmfOut)) dataAmfOut <- dataAmfIn 
	
	# Convert to data table
	obsAmfData <- data.table(obsAmfData)
	obsAmfData <- subset(obsAmfData, !is.na(obsAmfData$site_id))
	
	# Derived vars
	obsAmfData$RgNet <- with(obsAmfData, Rg - RgOut)
	obsAmfData$RglNet <- with(obsAmfData, RglOut - Rgl)
	obsAmfData$EnResid <- with(obsAmfData, RgNet - RglNet - LE - H - FG)
	obsAmfData$H2O_mmps <- with(obsAmfData, ifelse(TA>0, LE/2510/1000, LE/2844/1000))

	# Daily means --------------------------------

        # Date
        obsAmfData$UTC_date <- as.Date(trunc(as.POSIXct(format(obsAmfData$POSIXct, tz="UTC"), tz="UTC"), "days"))
	setkey(obsAmfData, "site_id", "POSIXct", "UTC_date")

	# Run aggregations
	obsAmfData.d <- obsAmfData[, list(TA_mean=mean(TA, na.rm=TRUE), TA_cnt=length(subset(TA, !is.na(TA))), 
                            WS_mean=mean(WS, na.rm=TRUE), WS_cnt=length(subset(WS, !is.na(WS))),
                            NEE_mean=mean(NEE, na.rm=TRUE), NEE_cnt=length(subset(NEE, !is.na(NEE))),
                            FC_mean=mean(FC, na.rm=TRUE), FC_cnt=length(subset(FC, !is.na(FC))),
                            H_mean=mean(H, na.rm=TRUE), H_cnt=length(subset(H, !is.na(H))),
                            SH_mean=mean(SH, na.rm=TRUE), SH_cnt=length(subset(SH, !is.na(SH))),
                            LE_mean=mean(LE, na.rm=TRUE), LE_cnt=length(subset(LE, !is.na(LE))),
                            RH_mean=mean(RH, na.rm=TRUE), RH_cnt=length(subset(RH, !is.na(RH))),
                            PRESS_mean=mean(PRESS, na.rm=TRUE), PRESS_cnt=length(subset(PRESS, !is.na(PRESS))),
                            CO2_mean=mean(CO2, na.rm=TRUE), CO2_cnt=length(subset(CO2, !is.na(CO2))),
                            VPD_mean=mean(VPD, na.rm=TRUE), VPD_cnt=length(subset(VPD, !is.na(VPD))),
                            SWC1_mean=mean(SWC1, na.rm=TRUE), SWC1_cnt=length(subset(SWC1, !is.na(SWC1))),
                            SWC2_mean=mean(SWC2, na.rm=TRUE), SWC2_cnt=length(subset(SWC2, !is.na(SWC2))),
                            Rn_mean=mean(Rn, na.rm=TRUE), Rn_cnt=length(subset(Rn, !is.na(Rn))),
                            Rg_mean=mean(Rg, na.rm=TRUE), Rg_cnt=length(subset(Rg, !is.na(Rg))),
                            RgOut_mean=mean(RgOut, na.rm=TRUE), RgOut_cnt=length(subset(RgOut, !is.na(RgOut))),
                            Rgl_mean=mean(Rgl, na.rm=TRUE), Rgl_cnt=length(subset(Rgl, !is.na(Rgl))),
                            RglOut_mean=mean(RglOut, na.rm=TRUE), RglOut_cnt=length(subset(RglOut, !is.na(RglOut))),
                            H2O_mean=mean(H2O, na.rm=TRUE), H2O_cnt=length(subset(H2O, !is.na(H2O))),
                            RE_mean=mean(RE, na.rm=TRUE), RE_cnt=length(subset(RE, !is.na(RE))),
                            GPP_mean=mean(GPP, na.rm=TRUE), GPP_cnt=length(subset(GPP, !is.na(GPP))),
                            ZL_mean=mean(ZL, na.rm=TRUE), ZL_cnt=length(subset(ZL, !is.na(ZL))),
                            H2Ommps_mean=mean(H2O_mmps, na.rm=TRUE), H2Ommps_cnt=length(subset(H2O_mmps, !is.na(H2O_mmps)))),
			by=list(site_id,UTC_date)]

#	obsAmfData.d <- plyr::ddply(obsAmfData, plyr::.(site_id, UTC_date), 
#                         plyr::summarise, 
#                            TA_mean=mean(TA, na.rm=TRUE), TA_cnt=length(subset(TA, !is.na(TA))),
#                            WS_mean=mean(WS, na.rm=TRUE), WS_cnt=length(subset(WS, !is.na(WS))),
#                            NEE_mean=mean(NEE, na.rm=TRUE), NEE_cnt=length(subset(NEE, !is.na(NEE))),
#                            FC_mean=mean(FC, na.rm=TRUE), FC_cnt=length(subset(FC, !is.na(FC))),
#                            H_mean=mean(H, na.rm=TRUE), H_cnt=length(subset(H, !is.na(H))),
#                            SH_mean=mean(SH, na.rm=TRUE), SH_cnt=length(subset(SH, !is.na(SH))),
#                            LE_mean=mean(LE, na.rm=TRUE), LE_cnt=length(subset(LE, !is.na(LE))),
#                            RH_mean=mean(RH, na.rm=TRUE), RH_cnt=length(subset(RH, !is.na(RH))),
#                            PRESS_mean=mean(PRESS, na.rm=TRUE), PRESS_cnt=length(subset(PRESS, !is.na(PRESS))),
#                            CO2_mean=mean(CO2, na.rm=TRUE), CO2_cnt=length(subset(CO2, !is.na(CO2))),
#                            VPD_mean=mean(VPD, na.rm=TRUE), VPD_cnt=length(subset(VPD, !is.na(VPD))),
#                            SWC1_mean=mean(SWC1, na.rm=TRUE), SWC1_cnt=length(subset(SWC1, !is.na(SWC1))),
#                            SWC2_mean=mean(SWC2, na.rm=TRUE), SWC2_cnt=length(subset(SWC2, !is.na(SWC2))),
#                            Rn_mean=mean(Rn, na.rm=TRUE), Rn_cnt=length(subset(Rn, !is.na(Rn))),
#                            Rg_mean=mean(Rg, na.rm=TRUE), Rg_cnt=length(subset(Rg, !is.na(Rg))),
#                            RgOut_mean=mean(RgOut, na.rm=TRUE), RgOut_cnt=length(subset(RgOut, !is.na(RgOut))),
#                            Rgl_mean=mean(Rgl, na.rm=TRUE), Rgl_cnt=length(subset(Rgl, !is.na(Rgl))),
#                            RglOut_mean=mean(RglOut, na.rm=TRUE), RglOut_cnt=length(subset(RglOut, !is.na(RglOut))),
#                            H2O_mean=mean(H2O, na.rm=TRUE), H2O_cnt=length(subset(H2O, !is.na(H2O))),
#                            RE_mean=mean(RE, na.rm=TRUE), RE_cnt=length(subset(RE, !is.na(RE))),
#                            GPP_mean=mean(GPP, na.rm=TRUE), GPP_cnt=length(subset(GPP, !is.na(GPP))),
#                            ZL_mean=mean(ZL, na.rm=TRUE), ZL_cnt=length(subset(ZL, !is.na(ZL))),
#                            H2Ommps_mean=mean(H2O_mmps, na.rm=TRUE), H2Ommps_cnt=length(subset(H2O_mmps, !is.na(H2O_mmps))),
#                         .parallel=parallelFlag)

	# Add a POSIXct date
	obsAmfData.d$POSIXct <- as.POSIXct(paste0(obsAmfData.d$UTC_date," 00:00"),
                                       format="%Y-%m-%d %H:%M", tz="UTC")

	# Unit conversions
	obsAmfData.d$wy <- CalcWaterYear(obsAmfData.d$POSIXct)
	obsAmfData.d$H2O_mm <- with(obsAmfData.d, H2Ommps_mean*86400)


        # Monthly means -------------------------------

	# Date 
        mo <- as.integer(format(obsAmfData.d$POSIXct, "%m"))
        yr <- as.integer(format(obsAmfData.d$POSIXct, "%Y"))
        obsAmfData.d$UTC_month <- as.Date(paste0(yr,"-",mo,"-15"), format="%Y-%m-%d")
	setkey(obsAmfData.d, "site_id", "UTC_date", "UTC_month")

	# Run aggregation
	obsAmfData.mo <- obsAmfData.d[, list(TA_mean=mean(TA_mean, na.rm=TRUE), TA_cnt=length(subset(TA_mean, !is.na(TA_mean))),
                            WS_mean=mean(WS_mean, na.rm=TRUE), WS_cnt=length(subset(WS_mean, !is.na(WS_mean))),
                            NEE_mean=mean(NEE_mean, na.rm=TRUE), NEE_cnt=length(subset(NEE_mean, !is.na(NEE_mean))),
                            FC_mean=mean(FC_mean, na.rm=TRUE), FC_cnt=length(subset(FC_mean, !is.na(FC_mean))),
                            H_mean=mean(H_mean, na.rm=TRUE), H_cnt=length(subset(H_mean, !is.na(H_mean))),
                            SH_mean=mean(SH_mean, na.rm=TRUE), SH_cnt=length(subset(SH_mean, !is.na(SH_mean))),
                            LE_mean=mean(LE_mean, na.rm=TRUE), LE_cnt=length(subset(LE_mean, !is.na(LE_mean))),
                            RH_mean=mean(RH_mean, na.rm=TRUE), RH_cnt=length(subset(RH_mean, !is.na(RH_mean))),
                            PRESS_mean=mean(PRESS_mean, na.rm=TRUE), PRESS_cnt=length(subset(PRESS_mean, !is.na(PRESS_mean))),
                            CO2_mean=mean(CO2_mean, na.rm=TRUE), CO2_cnt=length(subset(CO2_mean, !is.na(CO2_mean))),
                            VPD_mean=mean(VPD_mean, na.rm=TRUE), VPD_cnt=length(subset(VPD_mean, !is.na(VPD_mean))),
                            SWC1_mean=mean(SWC1_mean, na.rm=TRUE), SWC1_cnt=length(subset(SWC1_mean, !is.na(SWC1_mean))),
                            SWC2_mean=mean(SWC2_mean, na.rm=TRUE), SWC2_cnt=length(subset(SWC2_mean, !is.na(SWC2_mean))),
                            Rn_mean=mean(Rn_mean, na.rm=TRUE), Rn_cnt=length(subset(Rn_mean, !is.na(Rn_mean))),
                            Rg_mean=mean(Rg_mean, na.rm=TRUE), Rg_cnt=length(subset(Rg_mean, !is.na(Rg_mean))),
                            RgOut_mean=mean(RgOut_mean, na.rm=TRUE), RgOut_cnt=length(subset(RgOut_mean, !is.na(RgOut_mean))),
                            Rgl_mean=mean(Rgl_mean, na.rm=TRUE), Rgl_cnt=length(subset(Rgl_mean, !is.na(Rgl_mean))),
                            RglOut_mean=mean(RglOut_mean, na.rm=TRUE), RglOut_cnt=length(subset(RglOut_mean, !is.na(RglOut_mean))),
                            H2O_mean=mean(H2O_mean, na.rm=TRUE), H2O_cnt=length(subset(H2O_mean, !is.na(H2O_mean))),
                            RE_mean=mean(RE_mean, na.rm=TRUE), RE_cnt=length(subset(RE_mean, !is.na(RE_mean))),
                            GPP_mean=mean(GPP_mean, na.rm=TRUE), GPP_cnt=length(subset(GPP_mean, !is.na(GPP_mean))),
                            ZL_mean=mean(ZL_mean, na.rm=TRUE), ZL_cnt=length(subset(ZL_mean, !is.na(ZL_mean))),
                            H2Ommpd_mean=mean(H2O_mm, na.rm=TRUE), H2Ommpd_cnt=length(subset(H2O_mm, !is.na(H2O_mm)))),
			by=list(site_id,UTC_month)]

        # Add a POSIXct date
        obsAmfData.mo$POSIXct <- as.POSIXct(paste0(obsAmfData.mo$UTC_month," 00:00",
                                       format="%Y-%m-%d %H:%M", tz="UTC"), tz="UTC")

        # Unit conversions
        obsAmfData.mo$wy <- CalcWaterYear(obsAmfData.mo$POSIXct)
        obsAmfData.mo$H2O_mm <- with(obsAmfData.mo, H2Ommpd_mean * 
		CalcMonthDays(as.integer(format(obsAmfData.mo$POSIXct, "%m", tz="UTC")), as.integer(format(obsAmfData.mo$POSIXct, "%Y", tz="UTC"))))

	# Save outputs
	save(obsAmfData, obsAmfMeta, obsAmfData.d, obsAmfData.mo, file=dataAmfOut)
}


###----------- USGS ------------###

if (postprocUsgs) {
	load(dataUsgsIn)
	if (is.null(dataUsgsOut)) dataUsgsOut <- dataUsgsIn

	# Aggregate the observed data to a daily timestep.
	obsStrData$UTC_date <- as.Date(trunc(as.POSIXct(format(obsStrData$POSIXct, tz="UTC"), tz="UTC"), "days"))
	obsStrData.d <- plyr::ddply(obsStrData, plyr::.(site_no, UTC_date),
                         plyr::summarise, mean_qcms=mean(q_cms, na.rm=TRUE),
                         .parallel=parallelFlag)
	# Unit conversion: m^3/s -> m^3/dy -> ft^3/dy -> ac-ft/dy
	obsStrData.d$qvol_acft <- obsStrData.d$mean_qcms * 86400 / (0.3048^3) / 43560

	# Add a POSIXct column for ease of calculations and plotting.
	obsStrData.d$POSIXct <- as.POSIXct(paste0(obsStrData.d$UTC_date," 00:00",
                                       format="%Y-%m-%d %H:%M", tz="UTC"), tz="UTC")

	# Calculate a cumulative volume
	obsStrData.d$wy <- CalcWaterYear(obsStrData.d$POSIXct)
	obsStrData.d <- obsStrData.d[order(obsStrData.d$site_no, obsStrData.d$UTC_date),]
	wyList <- unique(obsStrData.d$wy)
	gageList <- unique(obsStrData.d$site_no)
	obsStrData.d$cumqvol_acft <- 0
	obsStrData.d$cumqvol_mm <- 0
	for (i in 1:length(gageList)) {
  		print(paste0("index ",i, " gage ", gageList[[i]]))
  		tmpgage <- subset(obsStrData.d, obsStrData.d$site_no==gageList[i])
    		obsStrData.d$cumqvol_acft[obsStrData.d$site_no==gageList[i]] <- CumsumNa(tmpgage$qvol_acft)
    		obsStrData.d$cumqvol_mm[obsStrData.d$site_no==gageList[i]] <- CumsumNa(tmpgage$qvol_acft) /
      						obsStrMeta$area_sqmi[obsStrMeta$site_no==gageList[i]] /
      						(5280^2) * 43560 * 25.4 * 12
    		rm(tmpgage)
  	}

	# Save outputs
	save(obsStrData, obsStrMeta, obsStrData.d, file=dataUsgsOut)
}

stopCluster(cl)
proc.time()

quit(save="no")
