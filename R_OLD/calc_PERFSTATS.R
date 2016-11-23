###################################################
##         CALCULATE PERFORMANCE STATS           ##
###################################################

# Multi-core
parallelFlag <- FALSE
if (ncores>1) {
        library(doParallel)
        cl <- makeForkCluster(ncores)
        registerDoParallel(cl)
        parallelFlag <- TRUE
}


################## General Setup ##################

saveList <- c()
if (writeStatsFile) {
	dir.create(writeDir, showWarnings = FALSE)
}

## ------------------------------------------------------------------------
# Setup processing functions

CalcStrStats <- function(modDf, obsDf, obsMeta, stid2gageList.=gageList, 
                      stdate=NULL, enddate=NULL,
		      idCol.obs="site_no", idCol.mod="st_id", 
			parallel=FALSE) {
  sites <- stid2gageList.[,idCol.mod]
  stnIDs <- unique(obsMeta[c(idCol.obs, "lat", "lon")])
  results <- data.frame()
  if (parallel) {
  	results <- foreach(n=1:length(sites), .combine=rbind, .inorder=FALSE, .errorhandling='remove') %dopar% {
			gageID <- subset(stid2gageList.[,idCol.obs],stid2gageList.[,idCol.mod]==sites[n])
  			out <- tryCatch(suppressWarnings( CalcModPerfMulti( modDf[get(idCol.mod)==sites[n],], 
                                                      subset(obsDf, obsDf[,idCol.obs]==gageID), 
                                                      flxCol.obs="q_cms", flxCol.mod="q_cms",
                                                      stdate=stdate,
                                                      enddate=enddate) ), 
                  		error=function(cond) {message(cond); return(NA)})
			if ( !is.na(out) ) {
      				out[,idCol.mod] <- sites[n]
      				out[,idCol.obs] <- gageID
      				out
      			}
  		}
  } else {
	for (n in 1:length(sites)) {
			gageID <- subset(stid2gageList.[,idCol.obs],stid2gageList.[,idCol.mod]==sites[n])
		       out <- tryCatch(suppressWarnings( CalcModPerfMulti( modDf[get(idCol.mod)==sites[n],],
                                                      subset(obsDf, obsDf[,idCol.obs]==gageID),
                                                      flxCol.obs="q_cms", flxCol.mod="q_cms",
                                                      stdate=stdate,
                                                      enddate=enddate) ),
                                error=function(cond) {message(cond); return(NA)})
                        if ( !is.na(out) ) {
                                out[,idCol.mod] <- sites[n]
                                out[,idCol.obs] <- gageID
                                results <- rbind(results, out)
                        }
	}
  }
  results <- plyr::join(results, stnIDs, by=idCol.obs)
  results[results=="Inf"]<-NA
  results[results=="-Inf"]<-NA
  results
}

CalcVarStats <- function(modDf, siteDf, obsDf,
                        stdate=NULL, enddate=NULL,
                        flxCol.obs, flxCol.mod,
                        idCol.obs="site_id", idCol.mod="statArg",
			overwriteDate=FALSE,
			parallel=FALSE) {
  # Subset
  if (!is.null(stdate) & !is.null(enddate)) {
    modDf <- subset(modDf, modDf$POSIXct >= stdate & modDf$POSIXct <= enddate)
  }
  if (!is.null(stdate) & is.null(enddate)) {
    modDf <- subset(modDf, modDf$POSIXct >= stdate)
  }
  if (is.null(stdate) & !is.null(enddate)) {
    modDf <- subset(modDf, modDf$POSIXct <= enddate)
  }
  # Set date if flagged
  if (overwriteDate) {
  	modDf$POSIXct <- as.POSIXct(CalcDateTrunc(modDf$POSIXct, timeZone = "UTC"), tz="UTC")
  	obsDf$POSIXct <- as.POSIXct(CalcDateTrunc(obsDf$POSIXct, timeZone = "UTC"), tz="UTC")  
  }
  # Run stats
  sites <- unique(obsDf[,idCol.obs])
  results <- data.frame()
  if (parallel) {
  	results <- foreach(n=1:length(sites), .combine=plyr::rbind.fill, .inorder=FALSE, .errorhandling='remove') %dopar% {
  			out <- tryCatch(suppressWarnings( CalcModPerfMulti( subset(modDf, modDf[,idCol.mod]==sites[n]),
                                                      subset(obsDf, obsDf[,idCol.obs]==sites[n]),
                                                      flxCol.obs=flxCol.obs, flxCol.mod=flxCol.mod) ),
                                    error=function(cond) {message(cond); return(NA)})
       			if ( !is.na(out) ) {
         			out$site_id <- sites[n]
         			out
         		}
       		}
  } else {
	for (n in 1:length(sites)) {
		       out <- tryCatch(suppressWarnings( CalcModPerfMulti( subset(modDf, modDf[,idCol.mod]==sites[n]),
                                                      subset(obsDf, obsDf[,idCol.obs]==sites[n]),
                                                      flxCol.obs=flxCol.obs, flxCol.mod=flxCol.mod) ),
                                    error=function(cond) {message(cond); return(NA)})
                        if ( !is.na(out) ) {
                                out$site_id <- sites[n]
                                results <- rbind(results, out)
                        }
	}
  }
  results<-plyr::join(results, siteDf, by=idCol.obs)

  results[results=="Inf"]<-NA
  results[results=="-Inf"]<-NA
  return(results)
}

## ------------------------------------------------------------------------
# Calculate Streamflow Stats
if (strProc) {
	message("Calculating streamflow stats...")
	# Initialize
	stats_str <- data.frame()
	if (reachRting) {
		runTagList <- unique(modChrtout$tag)
		idCol.mod <- "link"	
	} else {
		runTagList <- unique(modFrxstout$tag)
		idCol.mod <- "st_id"
	}
	# Loop through runs
	for (j in 1:length(runTagList)) {
		runTag <- runTagList[j]
		# Subset
		if (reachRting) {
			if (strProcDaily) {
				modFrxstout_tmp <- subset(modChrtout.d, modChrtout.d$tag==runTag)
			} else {
				modFrxstout_tmp <- subset(modChrtout, modChrtout$tag==runTag)
			}
		} else {
			if (strProcDaily) {
				modFrxstout_tmp <- subset(modFrxstout.d, modFrxstout.d$tag==runTag)
			} else {
				modFrxstout_tmp <- subset(modFrxstout, modFrxstout$tag==runTag)
			}
		}
		# Stats
		results <- CalcStrStats(modFrxstout_tmp, obsStrData, obsStrMeta, gageList,
					idCol.mod=idCol.mod, 
					stdate=stdate_stats, enddate=enddate_stats,
					parallel=parallelFlag)
		results$tag <- runTag
		results$seas <- "Full"
		stats_str <- rbind(stats_str, results)
		results <- CalcStrStats(modFrxstout_tmp, obsStrData, obsStrMeta, gageList,
					idCol.mod=idCol.mod, 
					stdate=stdate_stats_sub, enddate=enddate_stats_sub, 
					parallel=parallelFlag)
		results$tag <- runTag
		results$seas <- "Sub"
		stats_str <- rbind(stats_str, results)
	}
        # Mean across all sites
        stats_str_MEAN <- aggregate(stats_str[,1:57], by=list(stats_str$tag, stats_str$seas), mean)
	names(stats_str_MEAN)[1:2] <- c("tag", "seas")
	# Output
	if (writeStatsFile) {
		# Change NAs to large negative for QGIS so data type is not affected
		stats_str_tmp <- stats_str
		stats_str_tmp[is.na(stats_str_tmp)]<-(-1e+30)
		write.table(stats_str_tmp, file=paste0(writeDir, "/stats_str.txt"), sep="\t", row.names=FALSE)
                stats_str_MEAN_tmp <- stats_str_MEAN
                stats_str_MEAN_tmp[is.na(stats_str_MEAN_tmp)]<-(-1e+30)
                write.table(stats_str_MEAN_tmp, file=paste0(writeDir, "/stats_str_MEAN.txt"), sep="\t", row.names=FALSE)
	}
saveList <- c(saveList, "stats_str", "stats_str_MEAN")
}

## -----------------------------------------------------------------------
# Calculate SNOTEL Stats
if (snoProc) {
	message("Calculating SNOTEL stats...")
	# Forcing stats
	if (exists("modLdasin_SNO")) {
		message("SNOTEL: LDASIN")
	        # Initialize
        	stats_ldasin_sno <- data.frame()
        	runTagList <- unique(modLdasin_SNO[["snoday"]]$tag)
		modLdasin_tmp.snod_ALL <- modLdasin_SNO[["snoday"]]
        	# Loop through runs
        	for (j in 1:length(runTagList)) {
                	runTag <- runTagList[j]
  			# Subset
			modLdasin_tmp.snod <- subset(modLdasin_tmp.snod_ALL, modLdasin_tmp.snod_ALL$tag==runTag)
                        # SNOTEL (daily)
                        # Full run
                        results <- CalcVarStats(modLdasin_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="Tavg_K", flxCol.mod="T2D_mean", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Tmean"
                        results$seas <- "Full"
                        stats_ldasin_sno <- plyr::rbind.fill(stats_ldasin_sno, results)

                        results <- CalcVarStats(modLdasin_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="Tmin_K", flxCol.mod="T2D_min", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Tmin"
                        results$seas <- "Full"
                        stats_ldasin_sno <- plyr::rbind.fill(stats_ldasin_sno, results)

                        results <- CalcVarStats(modLdasin_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="Tmax_K", flxCol.mod="T2D_max", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Tmax"
                        results$seas <- "Full"
                        stats_ldasin_sno <- plyr::rbind.fill(stats_ldasin_sno, results)

                        # Subset (e.g., spring)
                        results <- CalcVarStats(modLdasin_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="Tavg_K", flxCol.mod="T2D_mean", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Tmean"
                        results$seas <- "Sub"
                        stats_ldasin_sno <- plyr::rbind.fill(stats_ldasin_sno, results)

                        results <- CalcVarStats(modLdasin_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="Tmin_K", flxCol.mod="T2D_min", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Tmin"
                        results$seas <- "Sub"
                        stats_ldasin_sno <- plyr::rbind.fill(stats_ldasin_sno, results)

                        results <- CalcVarStats(modLdasin_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="Tmax_K", flxCol.mod="T2D_max", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Tmax"
                        results$seas <- "Sub"
                        stats_ldasin_sno <- plyr::rbind.fill(stats_ldasin_sno, results)
		} # end loop
                # Mean across all sites
                stats_ldasin_sno_MEAN <- aggregate(stats_ldasin_sno[,1:57], by=list(stats_ldasin_sno$tag, stats_ldasin_sno$var, stats_ldasin_sno$seas), mean)
		names(stats_ldasin_sno_MEAN)[1:3] <- c("tag", "var", "seas")
        	# Output
        	if (writeStatsFile) {
                	# Change NAs to large negative for QGIS so data type is not affected
                	stats_ldasin_sno_tmp <- stats_ldasin_sno
                	stats_ldasin_sno_tmp[is.na(stats_ldasin_sno_tmp)]<-(-1e+30)
                	write.table(stats_ldasin_sno_tmp, file=paste0(writeDir, "/stats_ldasin_sno.txt"), sep="\t", row.names=FALSE)
                        stats_ldasin_sno_MEAN_tmp <- stats_ldasin_sno_MEAN
                        stats_ldasin_sno_MEAN_tmp[is.na(stats_ldasin_sno_MEAN_tmp)]<-(-1e+30)
                        write.table(stats_ldasin_sno_MEAN_tmp, file=paste0(writeDir, "/stats_ldasin_sno_MEAN.txt"), sep="\t", row.names=FALSE)
        	}
		saveList <- c(saveList, "stats_ldasin_sno", "stats_ldasin_sno_MEAN")
	} # end if modLdasin

        # Output stats
        if (exists("modLdasout") & ("ldasout.sno" %in% unique(modLdasout[[1]]$fileGroup))) {
		message("SNOTEL: LDASOUT")
                # Initialize
                stats_ldasout_sno <- data.frame()
		modLdasout_SNO <- list(native=subset(modLdasout[["native"]], modLdasout[["native"]]$fileGroup=="ldasout.sno"), 
					snoday=subset(modLdasout[["snoday"]], modLdasout[["snoday"]]$fileGroup=="ldasout.sno"),
					utcday=subset(modLdasout[["utcday"]], modLdasout[["utcday"]]$fileGroup=="ldasout.sno"))
                runTagList <- unique(modLdasout_SNO[["snoday"]]$tag)
                modLdasout_tmp.snod_ALL <- modLdasout_SNO[["snoday"]]
		overwriteDate_flag <- FALSE
		# Loop through runs
                for (j in 1:length(runTagList)) {
                        runTag <- runTagList[j]
                        # Subset
                        modLdasout_tmp.snod <- subset(modLdasout_tmp.snod_ALL, modLdasout_tmp.snod_ALL$tag==runTag)
                	# SNOTEL (daily)
                	# Full time period
                	results <- CalcVarStats(modLdasout_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="Prec_mm", flxCol.mod="DEL_ACCPRCP", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "Precip"
                	results$seas <- "Full"
                	stats_ldasout_sno <- plyr::rbind.fill(stats_ldasout_sno, results)

                	results <- CalcVarStats(modLdasout_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="SWE_mm", flxCol.mod="SNEQV_last", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SWE"
                	results$seas <- "Full"
                	stats_ldasout_sno <- plyr::rbind.fill(stats_ldasout_sno, results)

	                # Subset time period
        	        results <- CalcVarStats(modLdasout_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                	                flxCol.obs="Prec_mm", flxCol.mod="DEL_ACCPRCP", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "Precip"
                	results$seas <- "Sub"
                	stats_ldasout_sno <- plyr::rbind.fill(stats_ldasout_sno, results)

                	results <- CalcVarStats(modLdasout_tmp.snod, obsSnoMeta, obsSnoData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                       		        flxCol.obs="SWE_mm", flxCol.mod="SNEQV_last", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SWE"
                	results$seas <- "Sub"
                	stats_ldasout_sno <- plyr::rbind.fill(stats_ldasout_sno, results)
                } # end loop
		# Mean across all sites
		stats_ldasout_sno_MEAN <- aggregate(stats_ldasout_sno[,1:57], by=list(stats_ldasout_sno$tag, stats_ldasout_sno$var, stats_ldasout_sno$seas), mean)
		names(stats_ldasout_sno_MEAN)[1:3] <- c("tag", "var", "seas")
                # Output
                if (writeStatsFile) {
                        # Change NAs to large negative for QGIS so data type is not affected
                        stats_ldasout_sno_tmp <- stats_ldasout_sno
                        stats_ldasout_sno_tmp[is.na(stats_ldasout_sno_tmp)]<-(-1e+30)
                        write.table(stats_ldasout_sno_tmp, file=paste0(writeDir, "/stats_ldasout_sno.txt"), sep="\t", row.names=FALSE)
                        stats_ldasout_sno_MEAN_tmp <- stats_ldasout_sno_MEAN
                        stats_ldasout_sno_MEAN_tmp[is.na(stats_ldasout_sno_MEAN_tmp)]<-(-1e+30)
                        write.table(stats_ldasout_sno_MEAN_tmp, file=paste0(writeDir, "/stats_ldasout_sno_MEAN.txt"), sep="\t", row.names=FALSE)
                }
                saveList <- c(saveList, "stats_ldasout_sno", "stats_ldasout_sno_MEAN")
        } # end if modLdasout
} # end snoProc


## -----------------------------------------------------------------------
# Calculate MET Stats
if (metProc) {
	message("Calculating MET stats...")
        # Forcing stats
        if (exists("modLdasin_MET")) {
		message("MET: LDASIN")
                # Initialize
                stats_ldasin_met <- data.frame()
                runTagList <- unique(modLdasin_MET[["native"]]$tag)
                modLdasin_tmp.meth_ALL <- modLdasin_MET[["native"]]
                # Loop through runs
                for (j in 1:length(runTagList)) {
                        runTag <- runTagList[j]
                        # Subset
                        modLdasin_tmp.meth <- subset(modLdasin_tmp.meth_ALL, modLdasin_tmp.meth_ALL$tag==runTag)
                        # MET (hourly)
                        # Full run
                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats, enddate=enddate_stats, 
					flxCol.obs="Temp_K", flxCol.mod="T2D", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Temp"
                        results$seas <- "Full"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="relative_humidity", flxCol.mod="RelHum", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "RelHum"
                        results$seas <- "Full"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="SurfPress_Pa", flxCol.mod="PSFC", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SurfPress"
                        results$seas <- "Full"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="wind", flxCol.mod="Wind", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Wind"
                        results$seas <- "Full"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="shortwave_radiation", flxCol.mod="SWDOWN", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SWdown"
                        results$seas <- "Full"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        # Subset (e.g., spring)
                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="Temp_K", flxCol.mod="T2D", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Temp"
                        results$seas <- "Sub"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="relative_humidity", flxCol.mod="RelHum", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "RelHum"
                        results$seas <- "Sub"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="SurfPress_Pa", flxCol.mod="PSFC", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SurfPress"
                        results$seas <- "Sub"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="wind", flxCol.mod="Wind", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Wind"
                        results$seas <- "Sub"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        results <- CalcVarStats(modLdasin_tmp.meth, obsMetMeta, obsMetData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="shortwave_radiation", flxCol.mod="SWDOWN", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SWdown"
                        results$seas <- "Sub"
                        stats_ldasin_met <- plyr::rbind.fill(stats_ldasin_met, results)

                        } # end modldasin loop
                # Mean across all sites
                stats_ldasin_met_MEAN <- aggregate(stats_ldasin_met[,1:57], by=list(stats_ldasin_met$tag, stats_ldasin_met$var, stats_ldasin_met$seas), mean)
		names(stats_ldasin_met_MEAN)[1:3] <- c("tag", "var", "seas")
                # Output
                if (writeStatsFile) {
                        # Change NAs to large negative for QGIS so data type is not affected
                        stats_ldasin_met_tmp <- stats_ldasin_met
                        stats_ldasin_met_tmp[is.na(stats_ldasin_met_tmp)]<-(-1e+30)
                        write.table(stats_ldasin_met_tmp, file=paste0(writeDir, "/stats_ldasin_met.txt"), sep="\t", row.names=FALSE)
                        stats_ldasin_met_MEAN_tmp <- stats_ldasin_met_MEAN
                        stats_ldasin_met_MEAN_tmp[is.na(stats_ldasin_met_MEAN_tmp)]<-(-1e+30)
                        write.table(stats_ldasin_met_MEAN_tmp, file=paste0(writeDir, "/stats_ldasin_met_MEAN.txt"), sep="\t", row.names=FALSE)
                }
                saveList <- c(saveList, "stats_ldasin_met", "stats_ldasin_met_MEAN")
        } # end if modLdasin

        # Output stats
        if (exists("modLdasout") & ("ldasout.met" %in% unique(modLdasout[[1]]$fileGroup))) {
		message("MET: LDASOUT")
                # Initialize
                stats_ldasout_met <- data.frame()
                modLdasout_MET <- list(native=subset(modLdasout[["native"]], modLdasout[["native"]]$fileGroup=="ldasout.met"),
                                        snoday=subset(modLdasout[["snoday"]], modLdasout[["snoday"]]$fileGroup=="ldasout.met"),
                                        utcday=subset(modLdasout[["utcday"]], modLdasout[["utcday"]]$fileGroup=="ldasout.met"))
                runTagList <- unique(modLdasout_MET[["utcday"]]$tag)
                modLdasout_tmp.metd_ALL <- modLdasout_MET[["utcday"]]
		obsMetData.match <- obsMetData.dy
		overwriteDate_flag <- FALSE
                # Loop through runs
                for (j in 1:length(runTagList)) {
                        runTag <- runTagList[j]
                        # Subset
                        modLdasout_tmp.metd <- subset(modLdasout_tmp.metd_ALL, modLdasout_tmp.metd_ALL$tag==runTag)
                        # MET (daily)
                        # Full time period
                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="PrecTot", flxCol.mod="DEL_ACCPRCP", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "Precip"
                	results$seas <- "Full"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats, enddate=enddate_stats,
                      		          flxCol.obs="SnoDepmean_m", flxCol.mod="SNOWH_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SnowDepth"
                	results$seas <- "Full"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="SM1_mean", flxCol.mod="SOIL_M1_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilM1"
                	results$seas <- "Full"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="SM2_mean", flxCol.mod="SOIL_M2_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilM2"
                	results$seas <- "Full"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="SoilT1mean_K", flxCol.mod="SOIL_T1_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilT1"
                	results$seas <- "Full"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats, enddate=enddate_stats,
                        	        flxCol.obs="SoilT2mean_K", flxCol.mod="SOIL_T2_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilT2"
                	results$seas <- "Full"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	# Subset time period
                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                	flxCol.obs="PrecTot", flxCol.mod="DEL_ACCPRCP", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "Precip"
                	results$seas <- "Sub"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                        	        flxCol.obs="SnoDepmean_m", flxCol.mod="SNOWH_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SnowDepth"
                	results$seas <- "Sub"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                        	        flxCol.obs="SM1_mean", flxCol.mod="SOIL_M1_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilM1"
                	results$seas <- "Sub"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                        	        flxCol.obs="SM2_mean", flxCol.mod="SOIL_M2_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilM2"
                	results$seas <- "Sub"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                        	        flxCol.obs="SoilT1mean_K", flxCol.mod="SOIL_T1_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilT1"
                	results$seas <- "Sub"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                	results <- CalcVarStats(modLdasout_tmp.metd, obsMetMeta, obsMetData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                        	        flxCol.obs="SoilT2mean_K", flxCol.mod="SOIL_T2_mean", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                	results$tag <- runTag
                	results$var <- "SoilT2"
                	results$seas <- "Sub"
                	stats_ldasout_met <- plyr::rbind.fill(stats_ldasout_met, results)

                } # end loop
                # Mean across all sites
                stats_ldasout_met_MEAN <- aggregate(stats_ldasout_met[,1:57], by=list(stats_ldasout_met$tag, stats_ldasout_met$var, stats_ldasout_met$seas), mean)
		names(stats_ldasout_met_MEAN)[1:3] <- c("tag", "var", "seas")
                # Output
                if (writeStatsFile) {
                        # Change NAs to large negative for QGIS so data type is not affected
                        stats_ldasout_met_tmp <- stats_ldasout_met
                        stats_ldasout_met_tmp[is.na(stats_ldasout_met_tmp)]<-(-1e+30)
                        write.table(stats_ldasout_met_tmp, file=paste0(writeDir, "/stats_ldasout_met.txt"), sep="\t", row.names=FALSE)
                        stats_ldasout_met_MEAN_tmp <- stats_ldasout_met_MEAN
                        stats_ldasout_met_MEAN_tmp[is.na(stats_ldasout_met_MEAN_tmp)]<-(-1e+30)
                        write.table(stats_ldasout_met_MEAN_tmp, file=paste0(writeDir, "/stats_ldasout_met_MEAN.txt"), sep="\t", row.names=FALSE)
                }
                saveList <- c(saveList, "stats_ldasout_met", "stats_ldasout_met_MEAN")
	} # end if modLdasin

} # end if metProc

## -----------------------------------------------------------------------
# Calculate AMF Stats
if (amfProc) {
	message("Calculating Ameriflux stats...")
        # Forcing stats
        if (exists("modLdasin_AMF")) {
		message("AMF: LDASIN")
                # Initialize
                stats_ldasin_amf <- data.frame()
                runTagList <- unique(modLdasin_AMF[["native"]]$tag)
                modLdasin_tmp.amfh_ALL <- modLdasin_AMF[["native"]]
                # Loop through runs
                for (j in 1:length(runTagList)) {
                        runTag <- runTagList[j]
                        # Subset
                        modLdasin_tmp.amfh <- subset(modLdasin_tmp.amfh_ALL, modLdasin_tmp.amfh_ALL$tag==runTag)
                        # AMF (hourly)
                        # Full run
                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="Rg", flxCol.mod="SWFORC", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SWdown"
                        results$seas <- "Full"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="Rgl", flxCol.mod="LWFORC", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "LWdown"
                        results$seas <- "Full"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="RH", flxCol.mod="RelHum", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "RelHum"
                        results$seas <- "Full"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats, enddate=enddate_stats,
                                         flxCol.obs="WS", flxCol.mod="Wind", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Wind"
                        results$seas <- "Full"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        # Subset (e.g., spring)
                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="Rg", flxCol.mod="SWFORC", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SWdown"
                        results$seas <- "Sub"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="Rgl", flxCol.mod="LWFORC", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "LWdown"
                        results$seas <- "Sub"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="RH", flxCol.mod="RelHum", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "RelHum"
                        results$seas <- "Sub"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        results <- CalcVarStats(modLdasin_tmp.amfh, obsAmfMeta, obsAmfData, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                         flxCol.obs="WS", flxCol.mod="Wind", parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Wind"
                        results$seas <- "Sub"
                        stats_ldasin_amf <- plyr::rbind.fill(stats_ldasin_amf, results)

                        } # end modldasin loop
                # Mean across all sites
                stats_ldasin_amf_MEAN <- aggregate(stats_ldasin_amf[,1:57], by=list(stats_ldasin_amf$tag, stats_ldasin_amf$var, stats_ldasin_amf$seas), mean)
		names(stats_ldasin_amf_MEAN)[1:3] <- c("tag", "var", "seas")
                # Output
                if (writeStatsFile) {
                        # Change NAs to large negative for QGIS so data type is not affected
                        stats_ldasin_amf_tmp <- stats_ldasin_amf
                        stats_ldasin_amf_tmp[is.na(stats_ldasin_amf_tmp)]<-(-1e+30)
                        write.table(stats_ldasin_amf_tmp, file=paste0(writeDir, "/stats_ldasin_amf.txt"), sep="\t", row.names=FALSE)
                        stats_ldasin_amf_MEAN_tmp <- stats_ldasin_amf_MEAN
                        stats_ldasin_amf_MEAN_tmp[is.na(stats_ldasin_amf_MEAN_tmp)]<-(-1e+30)
                        write.table(stats_ldasin_amf_MEAN_tmp, file=paste0(writeDir, "/stats_ldasin_amf_MEAN.txt"), sep="\t", row.names=FALSE)
                }   
                saveList <- c(saveList, "stats_ldasin_amf", "stats_ldasin_amf_MEAN")
        } # end if modLdasin

        # Output stats
        if (exists("modLdasout") & ("ldasout.amf" %in% unique(modLdasout[[1]]$fileGroup))) {
		message("AMF: LDASOUT")
                # Initialize
                stats_ldasout_amf <- data.frame()
                modLdasout_AMF <- list(native=subset(modLdasout[["native"]], modLdasout[["native"]]$fileGroup=="ldasout.amf"),
                                        snoday=subset(modLdasout[["snoday"]], modLdasout[["snoday"]]$fileGroup=="ldasout.amf"),
                                        utcday=subset(modLdasout[["utcday"]], modLdasout[["utcday"]]$fileGroup=="ldasout.amf"))
                runTagList <- unique(modLdasout_AMF[["native"]]$tag)
                modLdasout_tmp.amf_ALL <- modLdasout_AMF[["native"]]
		obsAmfData.match <- obsAmfData
		overwriteDate_flag <- FALSE
                # Loop through runs
                for (j in 1:length(runTagList)) {
                        runTag <- runTagList[j]
                        # Subset
                        modLdasout_tmp.amf <- subset(modLdasout_tmp.amf_ALL, modLdasout_tmp.amf_ALL$tag==runTag)
                        # AMF (daily)
                        # Full time period
                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="RgNet", flxCol.mod="FSA", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SWnet"
                        results$seas <- "Full"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats, enddate=enddate_stats,
                                          flxCol.obs="RglNet", flxCol.mod="FIRA", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "LWnet"
                        results$seas <- "Full"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="Rn", flxCol.mod="Rnet", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Rnet"
                        results$seas <- "Full"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="LE", flxCol.mod="LH", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "LH"
                        results$seas <- "Full"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="H", flxCol.mod="HFX", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SH"
                        results$seas <- "Full"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_AMF[["utcday"]], obsAmfMeta, obsAmfData.d, stdate=stdate_stats, enddate=enddate_stats,
                                        flxCol.obs="H2O_mm", flxCol.mod="DEL_ET", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "ET"
                        results$seas <- "Full"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)


                        # Subset time period
                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="RgNet", flxCol.mod="FSA", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SWnet"
                        results$seas <- "Sub"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                          flxCol.obs="RglNet", flxCol.mod="FIRA", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "LWnet"
                        results$seas <- "Sub"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="Rn", flxCol.mod="Rnet", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "Rnet"
                        results$seas <- "Sub"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="LE", flxCol.mod="LH", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "LH"
                        results$seas <- "Sub"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_tmp.amf, obsAmfMeta, obsAmfData.match, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="H", flxCol.mod="HFX", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "SH"
                        results$seas <- "Sub"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                        results <- CalcVarStats(modLdasout_AMF[["utcday"]], obsAmfMeta, obsAmfData.d, stdate=stdate_stats_sub, enddate=enddate_stats_sub,
                                        flxCol.obs="H2O_mm", flxCol.mod="DEL_ET", overwriteDate=overwriteDate_flag, parallel=parallelFlag)
                        results$tag <- runTag
                        results$var <- "ET"
                        results$seas <- "Sub"
                        stats_ldasout_amf <- plyr::rbind.fill(stats_ldasout_amf, results)

                } # end loop
                # Mean across all sites
                stats_ldasout_amf_MEAN <- aggregate(stats_ldasout_amf[,1:57], by=list(stats_ldasout_amf$tag, stats_ldasout_amf$var, stats_ldasout_amf$seas), mean)
		names(stats_ldasout_amf_MEAN)[1:3] <- c("tag", "var", "seas")
                # Output
                if (writeStatsFile) {
                        # Change NAs to large negative for QGIS so data type is not affected
                        stats_ldasout_amf_tmp <- stats_ldasout_amf
                        stats_ldasout_amf_tmp[is.na(stats_ldasout_amf_tmp)]<-(-1e+30)
                        write.table(stats_ldasout_amf_tmp, file=paste0(writeDir, "/stats_ldasout_amf.txt"), sep="\t", row.names=FALSE)
                        stats_ldasout_amf_MEAN_tmp <- stats_ldasout_amf_MEAN
                        stats_ldasout_amf_MEAN_tmp[is.na(stats_ldasout_amf_MEAN_tmp)]<-(-1e+30)
                        write.table(stats_ldasout_amf_MEAN_tmp, file=paste0(writeDir, "/stats_ldasout_amf_MEAN.txt"), sep="\t", row.names=FALSE)
                }
                saveList <- c(saveList, "stats_ldasout_amf", "stats_ldasout_amf_MEAN")
        } # end if modLdasin

} # end if amfProc


## ------------------------------------------------------------------------
# Cleanup`
save(list=saveList, file=statsFileOut)

stopCluster(cl)
proc.time()

