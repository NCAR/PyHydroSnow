###################################################
##                GENERATE PLOTS                 ##
###################################################


################## General Setup ##################

dir.create(writePlotDir, showWarnings = FALSE)
source("PlotBasin.R")
source("PlotSnotel.R")
source("PlotMaps.R")
library(ggplot2)

lineColors <- scales::alpha(c("darkorange1", "dodgerblue", "olivedrab", "chocolate", "darkmagenta"), 0.8)
lineTyp <- 1
lineWd <- 2

# Get needed geo info
ncid <- ncdf4::nc_open(geoFile)
geoDX <- ncdf4::ncatt_get(ncid,varid=0,'DX')$value
geoNX <- ncdf4::ncatt_get(ncid,varid=0,'WEST-EAST_PATCH_END_UNSTAG')$value
geoNY <- ncdf4::ncatt_get(ncid,varid=0,'SOUTH-NORTH_PATCH_END_UNSTAG')$value
ncdf4::nc_close(ncid)
hydDX <- geoDX/aggfact

if (!exists("gageList") & exists("rtLinks")) {
	gageList<-subset(rtLinks[c("link","site_no")], rtLinks$gages!="")
	#names(link2gage)[names(link2gage)=="gages"]<-"site_no"
}

if (writeHtml) {
	library(knitr)
	library(pander)
	library(xtable)
	if (accflowPlot | hydroPlot | flowlsmPlot) {
		writeLines('```{r set-options, echo=FALSE, cache=FALSE}\noptions(width=1600)\nopts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, size="small")\n```', con=paste0(writePlotDir,"/plots_hydro.Rmd"))
		cat('# MODEL OUTPUT: HYDROLOGY\n', file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
	}
	if (accprecipPlot) {
                writeLines('```{r set-options, echo=FALSE, cache=FALSE}\noptions(width=1600)\nopts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, size="small")\n```', con=paste0(writePlotDir,"/plots_climate.Rmd"))
                cat('# MODEL OUTPUT: CLIMATE\n', file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
	}
	if (flowswePlot | indSwePlot) {
                writeLines('```{r set-options, echo=FALSE, cache=FALSE}\noptions(width=1600)\nopts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, size="small")\n```', con=paste0(writePlotDir,"/plots_snow.Rmd"))
                cat('# MODEL OUTPUT: SNOW\n', file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
	}
        if (strBiasMap | strCorrMap | snosweErrMap | snoprecipErrMap) {
                writeLines('```{r set-options, echo=FALSE, cache=FALSE}\noptions(width=1600)\nopts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, size="small")\n```', con=paste0(writePlotDir,"/plots_stats.Rmd"))
                cat('# MODEL OUTPUT: STATS\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
        }
}

## -----------------------------------------------------------------------
# Generate Plots

if (accprecipPlot | flowswePlot | flowlsmPlot) {
	modLdasout_BAS <- list(native=subset(modLdasout[["native"]], modLdasout[["native"]]$fileGroup=="ldasout.basgeo"),
                       snoday=subset(modLdasout[["snoday"]], modLdasout[["snoday"]]$fileGroup=="ldasout.basgeo"),
                       utcday=subset(modLdasout[["utcday"]], modLdasout[["utcday"]]$fileGroup=="ldasout.basgeo"))
}

if (indSwePlot) {
	modLdasout_SNO <- list(native=subset(modLdasout[["native"]], modLdasout[["native"]]$fileGroup=="ldasout.sno"),
                       snoday=subset(modLdasout[["snoday"]], modLdasout[["snoday"]]$fileGroup=="ldasout.sno"),
                       utcday=subset(modLdasout[["utcday"]], modLdasout[["utcday"]]$fileGroup=="ldasout.sno"))
}


## -----------------------------------------------------------------------
# Generate Plots

# Accumulated Flow
if (accflowPlot) {
message("Generating accumulated flow plots...")
# Setup
accflowList <- list()
if (is.null(accflowTags)) accflowTags <- unique(modFrxstout$tag)
for (i in 1:length(accflowTags)) {
        accflowList[[i]] <- subset(modFrxstout, modFrxstout$tag==accflowTags[i])
}
accflowColors <- lineColors[1:length(accflowList)]
accflowTypes <- rep(lineTyp, length(accflowList))
accflowWidths <- rep(lineWd, length(accflowList))
# Loop plots
for (n in names(gage2basinList)) {
        png(paste0(writePlotDir, "/accstrflow_", n, ".png"), width=2100, height=1350, res=225)
        PlotAccFlow(n, modDfs=accflowList,
                        obs=obsStrData.dy,
                        stdate=accflowStartDate,
                        enddate=accflowEndDate,
                        labMods=accflowTags,
                        labObs="Observed",
                        lnCols=accflowColors,
                        lnTyps=accflowTypes, lnWds=accflowWidths,
                        labTitle=paste0("Accumulated Flow: ", n, " (", obsStrMeta$site_name[obsStrMeta$site_no==n], ")"), obsCol="cumqvol_mm")
        #dev.off()
}
if (writeHtml) {
	cat('## Accumulated Flow Plots\n', file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
	for (n in names(gage2basinList)) {
		cat(paste0("```{r accflow_", n, ", fig.width = 12, fig.height = 6, out.width='700', out.height='350', echo=FALSE}\n"), 
			file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
		plottxt <- knitr::knit_expand(text='PlotAccFlow("{{n}}", modDfs=accflowList,
                        obs=obsStrData.dy,
                        stdate=accflowStartDate,
                        enddate=accflowEndDate,
                        labMods=accflowTags,
                        labObs="Observed",
                        lnCols=accflowColors,
                        lnTyps=accflowTypes, lnWds=accflowWidths,
                        labTitle=paste0("Accumulated Flow: ", "{{n}}", " (", obsStrMeta$site_name[obsStrMeta$site_no=="{{n}}"], ")"), obsCol="cumqvol_mm")\n')
		cat(plottxt, file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
		cat('```\n', file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
	}
}
}

# Hydrographs
if (hydroPlot) {
message("Generating hydrograph plots...")
# Setup
hydroList <- list()
if (reachRting) {
	if (is.null(hydroTags)) hydroTags <- unique(modChrtout$tag)
        if (exists("gageList")) {
        	gageNames <- unique(gageList$link)
        } else {
                gageNames <- unique(obsStrData$link)
        }
        idCol <- "link"
} else {
	if (is.null(hydroTags)) hydroTags <- unique(modFrxstout$tag)
        gageNames <- names(gage2basinList)
        idCol <- "site_no"
}
for (i in 1:length(hydroTags)) {
	if (!reachRting) {
        	hydroList[[i]] <- subset(modFrxstout, modFrxstout$tag==hydroTags[i])
	} else {
		hydroList[[i]] <- subset(modChrtout, modChrtout$tag==hydroTags[i])
	}
}
hydroColors <- lineColors[1:length(hydroList)]
hydroTypes <- rep(lineTyp, length(hydroList))
hydroWidths <- rep(lineWd, length(hydroList))
# Loop plots
for (n in gageNames) {
	if (idCol == "site_no") {
		siteId <- n
		plotTitle <- paste0("Streamflow: ", n, " (", obsStrMeta$site_name[obsStrMeta$site_no==n], ")")
	} else if (idCol =="link") {
		siteId <- subset(rtLinks$site_no, rtLinks$link==n)
		plotTitle <- paste0("Streamflow: ", subset(rtLinks$site_no, rtLinks$link==n), 
			" (", obsStrMeta$site_name[obsStrMeta$site_no==subset(rtLinks$site_no, rtLinks$link==n)], ")")
	}
        png(paste0(writePlotDir, "/hydrogr_", siteId, ".png"), width=2100, height=1350, res=225)
        PlotFlow(n, modDfs=hydroList,
                        obs=obsStrData,
                        labMods=hydroTags,
                        labObs="Observed",
                        lnCols=hydroColors,
                        lnWds=hydroWidths,
                        labTitle=plotTitle,
                        stdate=hydroStartDate, enddate=hydroEndDate, obsCol="q_cms", idCol=idCol)
        #dev.off()
}
if (writeHtml) {
        cat('## Hydrographs\n', file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
        for (n in gageNames) {
                cat(paste0("```{r hydro_", n, ", fig.width = 12, fig.height = 6, out.width='700', out.height='350', echo=FALSE}\n"), 
			file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
                plottxt <- knitr::knit_expand(text='plotTitle <- paste0("Streamflow: ", subset(rtLinks$site_no, rtLinks$link=={{n}}),   
                        " (", obsStrMeta$site_name[obsStrMeta$site_no==subset(rtLinks$site_no, rtLinks$link=={{n}})], ")");
			PlotFlow("{{n}}", modDfs=hydroList,
                        obs=obsStrData,
                        labMods=hydroTags,
                        labObs="Observed",
                        lnCols=hydroColors,
                        lnWds=hydroWidths,
                        labTitle=plotTitle,
                        stdate=hydroStartDate, enddate=hydroEndDate, obsCol="q_cms", idCol=idCol)\n')
                cat(plottxt, file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
                cat('```\n', file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
        }
}
}

if (hydroEnsPlot) {
message("Generating Ensemble hydrograph plots...")
# Setup
hydroList <- list()
if (reachRting) {
        if (is.null(hydroTags2)) hydroTags2 <- unique(modChrtout$tag)
	if (is.null(hydroEnsTags)) hydroEnsTags <- unique(modChrtout$enstag)
        if (exists("gageList")) {
                gageNames <- unique(gageList$site_no)
		gageNames <- gageNames[which(gageNames != "")]
        } else {
                gageNames <- unique(obsStrData$site_no)
		gageNames <- gageNames[which(gageNames != "")]
        }
        idCol <- "site_no"
	modDfsOut <- modChrtout
} else {
        if (is.null(hydroTags2)) hydroTags2 <- unique(modFrxstout$tag)
        if (is.null(hydroEnsTags)) hydroEnsTags <- unique(modFrxstout$enstag)
        gageNames <- names(gage2basinList)
        idCol <- "site_no"
	modDfsOut <- modFrxstout
}

# Loop over station gauges and create suite of ensemble plots
for (i in 1:length(hydroTags2)) {
        modelTag <- hydroTags2[i]
        for (n in gageNames) {
                if (!is.null(STRfile)) { # Make ensemble plots with observations
        		if (idCol == "site_no") {
                                siteId <- n
                                plotTitle <- paste0("Streamflow: ", n, " Model: ", modelTag)
                        } else if (idCol =="link") {
                                siteId <- subset(rtLinks$site_no, rtLinks$link==n)
                                plotTitle <- paste0("Streamflow: ", subset(rtLinks$site_no, rtLinks$link==n),
                                        " (", obsStrMeta$site_name[obsStrMeta$site_no==subset(rtLinks$site_no, rtLinks$link==n)], ") ", "Model: ", modelTag)
                        }
                        obsFlag <- 1 	
                } else { # Make ensemble plots with observationis
        		if (idCol == "site_no") {
                                siteId <- n
                                plotTitle <- paste0("Streamflow: ", n, " Model: ", modelTag)
                        } else if (idCol =="link") {
                                siteId <- subset(rtLinks$site_no, rtLinks$link==n)
                                plotTitle <- paste0("Streamflow: ", subset(rtLinks$site_no, rtLinks$link==n),
                                        " (", n, ") ", "Model: ", modelTag)
                        }
                        obsFlag <- 0 
                }
		# Make suit of plots
		if (obsFlag == 1){
			plotEnsFlowWObs(n, modDfs=modDfsOut,
		      		        obs=obsStrData,
			    	        labObs="Observed",
			    	        title=plotTitle,
			    	        startDate=hydroEnsStartDate,
			    	        endDate=hydroEnsEndDate,
			    	        outDir=writePlotDir)
				#dev.off()
		} else {
			plotEnsFlow(n, modDfs=modDfsOut,
                                    title=plotTitle,
                                    startDate=hydroEnsStartDate,
                                    endDate=hydroEnsEndDate,
                                    outDir=writePlotDir)
                        #dev.off()
		}
        }
}
}

# Ensemble basin SWE volume plots
if (basSnoEnsPlot) {
message("Generating basin SWE volume ensemble plots...")
modTags <- unique(modLdasout$native$tag)
basins <- unique(modLdasout$native$statArg)
for (i in 1:length(modTags)) {
	modelTag <- modTags[i]
	for (n in basins) {
		# Make plots
		plotTitle <- paste0("Basin: ",n," Model: ",modelTag)
		plotEnsSWE(n, modDfs=modLdasout$native,
			   title=plotTitle,
			   stDate=basSnowEnsStartDate,
			   endDate=basSnowEnsEndDate,
			   outDir=writePlotDir)
		#dev.off()
	}   
}
}

# Accumulated Precip
if (accprecipPlot) {
message("Generating accumulated precip plots...")
# Setup
accprecipList <- list()
if (is.null(accprecipTags)) accprecipTags <- unique(modLdasout_BAS[["native"]]$tag)
for (i in 1:length(accprecipTags)) {
        accprecipList[[i]] <- subset(modLdasout_BAS[["native"]], modLdasout_BAS[["native"]]$tag==accprecipTags[i])
}
accprecipColors <- lineColors[1:length(accprecipList)]
accprecipTypes <- rep(lineTyp, length(accprecipList))
accprecipWidths <- rep(lineWd, length(accprecipList))
# Loop plots
for (n in names(gage2basinList)) {
        png(paste0(writePlotDir, "/accprecip_", n, ".png"), width=2100, height=1350, res=225)
        PlotAccPrecip(n, modDfs=accprecipList,
                        stdate=accprecipStartDate,
                        enddate=accprecipEndDate,
                        labMods=accprecipTags,
                        lnCols=accprecipColors,
                        lnTyps=accprecipTypes, lnWds=accprecipWidths,
                        labTitle=paste0("Accumulated Precip: ", n, " (", obsStrMeta$site_name[obsStrMeta$site_no==n], ")"))
        #dev.off()
}
if (writeHtml) {
        cat('## Accumulated Basin-Mean Precip Plots\n', file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
        for (n in names(gage2basinList)) {
                cat(paste0("```{r accprecip_", n, ", fig.width = 12, fig.height = 6, out.width='700', out.height='350', echo=FALSE}\n"), 
				file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
                plottxt <- knitr::knit_expand(text='PlotAccPrecip("{{n}}", modDfs=accprecipList,
                        stdate=accprecipStartDate,
                        enddate=accprecipEndDate,
                        labMods=accprecipTags,
                        lnCols=accprecipColors,
                        lnTyps=accprecipTypes, lnWds=accprecipWidths,
                        labTitle=paste0("Accumulated Precip: ", "{{n}}", " (", obsStrMeta$site_name[obsStrMeta$site_no=="{{n}}"], ")"))\n')
                cat(plottxt, file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
                cat('```\n', file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
        }
}
}

# Flow and basin mean SWE
if (flowswePlot) {
message("Generating flow + basin SWE plots...")
# Setup
flowsweStrList <- list()
flowsweLsmList <- list()
if (is.null(flowsweTags)) flowsweTags <- unique(modLdasout_BAS[["native"]]$tag)
for (i in 1:length(flowsweTags)) {
        flowsweStrList[[i]] <- subset(modFrxstout, modFrxstout$tag==flowsweTags[i])
        flowsweLsmList[[i]] <- subset(modLdasout_BAS[["native"]], modLdasout_BAS[["native"]]$tag==flowsweTags[i])
}
flowsweColors <- lineColors[1:length(flowsweStrList)]
flowsweTypes <- rep(lineTyp, length(flowsweStrList))
flowsweWidths <- rep(lineWd, length(flowsweStrList))
# Loop plots
for (n in names(gage2basinList)) {
        png(paste0(writePlotDir, "/flowswe_", n, ".png"), width=2100, height=1350, res=225)
        PlotFlowSwe(n, modDfs=flowsweStrList,
                        lsmDfs=flowsweLsmList,
                        obs=obsStrData.dy,
                        labMods=flowsweTags,
                        lnCols=flowsweColors,
                        lnWds=flowsweWidths,
                        labTitle=paste0("Streamflow with Basin-Mean SWE: ", n),
                        stdate=flowsweStartDate, enddate=flowsweEndDate)
        #dev.off()
}
if (writeHtml) {
        cat('## Streamflow & Basin-mean SWE Plots\n', file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
        for (n in names(gage2basinList)) {
                cat(paste0("```{r flowswe_", n, ", fig.width = 12, fig.height = 6, out.width='700', out.height='350', echo=FALSE}\n"), 
			file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
                plottxt <- knitr::knit_expand(text='PlotFlowSwe("{{n}}", modDfs=flowsweStrList,
                        lsmDfs=flowsweLsmList,
                        obs=obsStrData.dy,
                        labMods=flowsweTags,
                        lnCols=flowsweColors,
                        lnWds=flowsweWidths,
                        labTitle=paste0("Streamflow with Basin-Mean SWE: ", "{{n}}"),
                        stdate=flowsweStartDate, enddate=flowsweEndDate)\n')
                cat(plottxt, file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
                cat('```\n', file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
        }
}
}

# Flow and basin mean LSM runoff
if (flowlsmPlot) {
message("Generating flow + basin runoff plots...")
# Setup
if (is.null(flowlsmTags)) {
        if (exists("modFrxstout")) {
                flowlsmTags <- unique(modFrxstout$tag)
                gageNames <- names(gage2basinList)
                idCol <- "site_no"
        } else if (exists("modChrtout")) {
                flowlsmTags <- unique(modChrtout$tag)
                if (!is.null(gageList)) {
                        gageNames <- unique(gageList$link)
                } else {
                        gageNames <- unique(obsStrData$link)
                }
                idCol <- "link"
        }
}
flowlsmColors <- lineColors[1:2]
flowlsmTypes <- rep(lineTyp, 2)
flowlsmWidths <- rep(lineWd, 2)
# Loop plots
for (i in 1:length(flowlsmTags)) {
        if (exists("modFrxstout")) {
                strDf <- subset(modFrxstout, modFrxstout$tag==flowlsmTags[i])
        } else if (exists("modChrtout")) {
                strDf <- subset(modChrtout, modChrtout$tag==flowlsmTags[i])
        } else {
                stop()
        }
        lsmDf <- subset(modLdasout_BAS[["native"]], modLdasout_BAS[["native"]]$tag==flowlsmTags[i])
	ts <- as.integer(difftime(lsmDf$POSIXct[2],lsmDf$POSIXct[1], units="secs"))
	for (n in gageNames) {
		ngage <- ifelse(idCol=="link", as.integer(subset(gageList$site_no, gageList$link==n)), n)
		ngageChar <- ifelse(nchar(as.character(ngage))==7, paste0("0", as.character(ngage)), as.character(ngage))
        	png(paste0(writePlotDir, "/flowlsm_", n, ".png"), width=2100, height=1350, res=225)
        	PlotFlowLsm(n, modDf=strDf, lsmDf=lsmDf, 
                        obs=obsStrData,
                        labMods=flowlsmTags,
                        labObs="Observed",
                        lnCols=flowlsmColors,
                        lnWds=flowlsmWidths,
                        labTitle=paste0("Streamflow: ", ngageChar, " (", obsStrMeta$site_name[obsStrMeta$site_no==ngageChar], ")"),
                        stdate=flowlsmStartDate, enddate=flowlsmEndDate, obsCol="q_cms", idCol=idCol,
			tsSecs=ts, areaSqKm=mskgeo.areaList[[as.character(ngage)]]*geoDX/1000, ngage=ngage)
        	#dev.off()
	}
}
if (writeHtml) {
for (i in 1:length(flowlsmTags)) {
        if (exists("modFrxstout")) {
                strDf <- subset(modFrxstout, modFrxstout$tag==flowlsmTags[i])
        } else if (exists("modChrtout")) {
                strDf <- subset(modChrtout, modChrtout$tag==flowlsmTags[i])
        } else {
                stop()
        }
        lsmDf <- subset(modLdasout_BAS[["native"]], modLdasout_BAS[["native"]]$tag==flowlsmTags[i])
        ts <- as.integer(difftime(lsmDf$POSIXct[2],lsmDf$POSIXct[1], units="secs"))
        cat(paste0('## Streamflow & Basin-mean LSM Runoff Plots:', i, '\n'), file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
        for (n in gageNames) {
                cat(paste0("```{r flowlsm_", n, ", fig.width = 12, fig.height = 6, out.width='700', out.height='350', echo=FALSE}\n"),
                        file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
                plottxt <- knitr::knit_expand(text='ngage <- ifelse(idCol=="link", 
					as.integer(subset(gageList$site_no, gageList$link=="{{n}}")), "{{n}}");
			ngageChar <- ifelse(nchar(as.character(ngage))==7, paste0("0", as.character(ngage)), as.character(ngage));
			PlotFlowLsm("{{n}}", modDf=strDf,
			lsmDf=lsmDf,
                        obs=obsStrData,
                        labMods=flowlsmTags,
                        labObs="Observed",
                        lnCols=flowlsmColors,
                        lnWds=flowlsmWidths,
                        labTitle=paste0("Streamflow: ", ngageChar, " (", obsStrMeta$site_name[obsStrMeta$site_no==ngageChar], ")"),
                        stdate=flowlsmStartDate, enddate=flowlsmEndDate, obsCol="q_cms", idCol=idCol,
			tsSecs=ts, areaSqKm=mskgeo.areaList[[as.character(ngage)]]*geoDX/1000, ngage=ngage)\n')
                cat(plottxt, file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
                cat('```\n', file=paste0(writePlotDir,"/plots_hydro.Rmd"), append=TRUE)
        }
}
}
}
   
# SWE
if (indSwePlot) {
message("Generating SWE plots...")
# Setup
sweList <- list()
if (is.null(sweTags)) sweTags <- unique(modLdasout_SNO[["native"]]$tag)
for (i in 1:length(sweTags)) {
        sweList[[i]] <- subset(modLdasout_SNO[["native"]], modLdasout_SNO[["native"]]$tag==sweTags[i])
}
sweColors <- lineColors[1:length(sweList)]
sweTypes <- rep(lineTyp, length(sweList))
sweWidths <- rep(lineWd, length(sweList))
# Loop plots
sites <- unique(modLdasout_SNO[["native"]]$statArg)
for (n in sites) {
  #png(paste0(writePlotDir, "/swe_", n, ".png"), width=2100, height=1350, res=225)
  # Logan modify for output file path to be a bit more specific
  png(paste0(writePlotDir, "/SNOTEL_SWE_TIMESERIES_",n,"_",strftime(sweStartDate,'%Y%m%d'),'_',strftime(sweEndDate,'%Y%m%d'),'.png'),
      height=1350, res=225)
  PlotSwe(n, modDfs=sweList,
                obs=obsSnoData, obsmeta=obsSnoMeta,
                labMods=sweTags,
                lnCols=sweColors,
                lnWds=sweWidths,
                precCol.obs="CumPrec_mm", precCol.mod="ACCPRCP",
                sweCol.obs="SWE_mm", sweCol.mod="SNEQV", fact=1, snowh=FALSE,
                labTitle="Accumulated Precipitation and SWE",
                stdate=sweStartDate, enddate=sweEndDate)
  #dev.off()
}
if (writeHtml) {
        cat('## Station SWE Plots\n', file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
        for (n in sites) {
                cat(paste0("```{r swe_", n, ", fig.width = 12, fig.height = 6, out.width='700', out.height='350', echo=FALSE}\n"), 
			file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
                plottxt <- knitr::knit_expand(text='PlotSwe("{{n}}", modDfs=sweList,
                	obs=obsSnoData, obsmeta=obsSnoMeta,
                	labMods=sweTags,
                	lnCols=sweColors,
                	lnWds=sweWidths,
                	precCol.obs="CumPrec_mm", precCol.mod="ACCPRCP",
                	sweCol.obs="SWE_mm", sweCol.mod="SNEQV", fact=1, snowh=FALSE,
                	labTitle="Accumulated Precipitation and SWE",
                	stdate=sweStartDate, enddate=sweEndDate)\n')
                	cat(plottxt, file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
                	cat('```\n', file=paste0(writePlotDir,"/plots_snow.Rmd"), append=TRUE)
        }
}
}

# MET
if (metPlot) {
	message("Generating MET plots...")
	# Setup
	modLdasin_MET <- subset(modLdasin[["utcday"]], modLdasin[["utcday"]]$fileGroup=="ldasin.met")
	if (is.null(metTags)) metTags <- unique(modLdasin_MET$tag)
	metSites <- unique(modLdasin_MET$statArg)
	for (i in metTags) {
		modLdasin_MET_TAG <- subset(modLdasin_MET, modLdasin_MET$tag==i)
		# Loop Sites
		for (n in metSites) {
  			# Temperature
  			png(paste0(writePlotDir, "/met_temp_", i, "_", n, ".png"), width=1350, height=2100, res=225)
  			PlotMet(obs=obsMetData.dy,
                        	mod=modLdasin_MET_TAG,
                        	site=n,
                        	obsVars=c("Tmean_K", "Tmax_K", "Tmin_K"),
                        	modVars=c("T2D_mean", "T2D_max", "T2D_min"),
                        	lnLabs=c("Mean Temp (C)", "Max Temp (C)", "Min Temp (C)"),
                        	title=paste0(obsMetMeta$site_name[obsMetMeta$site_id==n], ":\nDaily Temperature"),
                        	xLab="", adj=(-273.15), 
				stdate=metStartDate, enddate=metEndDate)
  			#dev.off()
			if (writeHtml) {
			        cat('## MET Station Plots\n', file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
                		cat(paste0("```{r met_", i, "_", n, ", fig.width = 13.5, fig.height = 21, out.width='1350', out.height='2100', echo=FALSE}\n"),
                        		file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
                		plottxt <- knitr::knit_expand(text='PlotMet(obs=obsMetData.dy,
                                	mod=modLdasin_MET_TAG,
                                	site="{{n}}",
                                	obsVars=c("Tmean_K", "Tmax_K", "Tmin_K"),
                                	modVars=c("T2D_mean", "T2D_max", "T2D_min"),
                                	lnLabs=c("Mean Temp (C)", "Max Temp (C)", "Min Temp (C)"),
                                	title=paste0(obsMetMeta$site_name[obsMetMeta$site_id=="{{n}}"], ":\nDaily Temperature"),
                                	xLab="", adj=(-273.15), 
                                	stdate=metStartDate, enddate=metEndDate)\n')
                        	cat(plottxt, file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
                        	cat('```\n', file=paste0(writePlotDir,"/plots_climate.Rmd"), append=TRUE)
        		}
  			# SW Radiation
  			png(paste0(writePlotDir, "/met_swrad_", i, "_", n, ".png"), width=1350, height=2100, res=225)
  			PlotMet(obs=obsMetData.dy,
                        	mod=modLdasin_MET_TAG,
                        	site=n,
                        	obsVars=c("SWRad_mean", "SWRad_max", "SWRad_min"),
                        	modVars=c("SWDOWN_mean", "SWDOWN_max", "SWDOWN_min"),
                        	lnLabs=c("Mean Rad (W/m2)", "Max Rad (W/m2)", "Min Rad (W/m2)"),
                        	title=paste0(obsMetMeta$site_name[obsMetMeta$site_id==n], ":\nDaily Shortwave Radiation"),
                        	xLab="",
				stdate=metStartDate, enddate=metEndDate)
  			#dev.off()
  			# Wind
  			png(paste0(writePlotDir, "/met_wind_", i, "_", n, ".png"), width=1350, height=2100, res=225)
  			PlotMet(obs=obsMetData.dy,
                        	mod=modLdasin_MET_TAG,
                        	site=n,
                        	obsVars=c("Wind_mean", "Wind_max", "Wind_min"),
                        	modVars=c("Wind_mean", "Wind_max", "Wind_min"),
                        	lnLabs=c("Mean Speed (m/s)", "Max Speed (m/s)", "Min Speed (m/s)"),
                        	title=paste0(obsMetMeta$site_name[obsMetMeta$site_id==n], ":\nDaily Wind Speed"),
                        	xLab="",
				stdate=metStartDate, enddate=metEndDate)
  			#dev.off()
  			# Humidity
  			png(paste0(writePlotDir, "/met_relhum_", i, "_", n, ".png"), width=1350, height=2100, res=225)
  			PlotMet(obs=obsMetData.dy,
                        	mod=modLdasin_MET_TAG,
                        	site=n,
                        	obsVars=c("RH_mean", "RH_max", "RH_min"),
                        	modVars=c("RelHum_mean", "RelHum_max", "RelHum_min"),
                        	lnLabs=c("Mean RH (0-1)", "Max RH (0-1)", "Min RH (0-1)"),
                        	title=paste0(obsMetMeta$site_name[obsMetMeta$site_id==n], ":\nRelative Humidity"),
                        	xLab="",
				stdate=metStartDate, enddate=metEndDate)
  			#dev.off()
  			# Pressure
  			png(paste0(writePlotDir, "/met_press_", i, "_", n, ".png"), width=1350, height=2100, res=225)
  			PlotMet(obs=obsMetData.dy,
                        	mod=modLdasin_MET_TAG,
                        	site=n,
                        	obsVars=c("SurfPressmean_Pa", "SurfPressmax_Pa", "SurfPressmin_Pa"),
                        	modVars=c("PSFC_mean", "PSFC_max", "PSFC_min"),
                        	lnLabs=c("Mean Press (kPa)", "Max Press (kPa)", "Min Press (kPa)"),
                        	title=paste0(obsMetMeta$site_name[obsMetMeta$site_id==n], ":\nSurface Pressure"),
                        	xLab="", mult=0.001,
				stdate=metStartDate, enddate=metEndDate)
  			#dev.off()
		}
	}
}

# Snow Basin Plots of Modeled vs SNODAS
if (snowBasinPlot) {
	library(ggplot2)
	library(reshape)
        library(scales)

	# Get number of basins to plot
	nBas <- length(mskgeo.nameList)

        # Convert POSIXct objects to Date objects as that's what they are in the Rdata set
        bDate <- as.Date(snowBasStartDate)
        eDate <- as.Date(snowBasEndDate)

	# Esablish date strings
        bDStr <- strftime(bDate,format="%Y%m%d")
	eDStr <- strftime(eDate,format="%Y%m%d")

	# Loop through basins
	for (n in 1:nBas) {
		bName <- mskgeo.nameList[[n]]
		#Total snow covered area
       		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,snow_area_km))
                snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,snow_area_km))
                pngFile <- paste0(writePlotDir,"/snow_area_km_",bDStr,"_",eDStr,"_",bName,".png")
		PlotBasSnoMetrics(n, modDfs=snowVarSub2,
				     var=1,
				     title=paste0("Region Area Covered by Snow (km^2) for Region: ",
				                  bName),
				     ylab="km^2",
			             xlab="Date",
				     fileOut=pngFile)

		#Fraction of basin covered by snow
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,snow_cover_fraction))
                snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,snow_cover_fraction))
		pngFile <- paste0(writePlotDir,"/snow_cover_fraction_",bDStr,"_",eDStr,"_",bName,".png")
		PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                                     var=2,
                                     title=paste0("Fraction of Region Covered by Snow for Region: ",
                                                  bName),
				     ylab="Fraction of Basin",
                                     xlab="Date",
				     fileOut=pngFile)

		#Snow volume (cubic meters)
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,snow_volume_cub_meters))
		snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,snow_volume_cub_meters))
		pngFile <- paste0(writePlotDir,"/snow_vol_cub_meters_",bDStr,"_",eDStr,"_",bName,".png")
		PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                                     var=3,
                                     title=paste0("Basin SWE Volume (m^3) for Region: ",
                                                  bName),
				     ylab="m^3",
                                     xlab="Date",
				     fileOut=pngFile)

		#Snow volume (acre feet)
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,snow_volume_acre_feet))
		snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,snow_volume_acre_feet))
		pngFile <- paste0(writePlotDir,"/snow_vol_af_",bDStr,"_",eDStr,"_",bName,".png")
		PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                                     var=4,
                                     title=paste0("Region SWE Volume (acre-feet) for Region: ",
                                                  bName),
				     ylab="acre-feet",
                                     xlab="Date",
				     fileOut=pngFile)

		#Mean snow line (meters)
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,mean_snow_line_meters))
		snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,mean_snow_line_meters))
		pngFile <- paste0(writePlotDir,"/snow_line_meters_",bDStr,"_",eDStr,"_",bName,".png")
		if (length(which(!is.na(snowVarSub2$mean_snow_line_meters))) > 0) { # Only plot if valid snow line present
			PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                        	             var=5,
                                 	      title=paste0("Mean Region Snow Line (meters) for Region: ",
                               	                   bName),
					     ylab="m",
                               	             xlab="Date",
					     fileOut=pngFile)
		}

		#Mean snow line (feet)
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,mean_snow_line_feet))
		snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,mean_snow_line_feet))
		pngFile <- paste0(writePlotDir,"/snow_line_feet_",bDStr,"_",eDStr,"_",bName,".png")
		if (length(which(!is.na(snowVarSub2$mean_snow_line_feet))) > 0){
			PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                        	             var=6,
                                	     title=paste0("Mean Region Snow Line (feet) for Region: ",
                                        	          bName),
					     ylab="ft",
                	                     xlab="Date",
					     fileOut=pngFile)
		}

		#Mean SWE (mm)
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,mean_swe_mm))
		snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,mean_swe_mm))
		pngFile <- paste0(writePlotDir,"/mean_swe_mm_",bDStr,"_",eDStr,"_",bName,".png")
		PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                                     var=7,
                                     title=paste0("Mean SWE (mm) for Region: ",
                                                  bName),
				     ylab="mm",
                                     xlab="Date",
				     fileOut=pngFile)

		#Max SWE (mm)
		snowVarSub1 <- subset(snowBasinData,Basin == bName,select=c(Basin,Date,product,max_swe_mm))
		snowVarSub2 <- subset(snowVarSub1,(Date >= bDate & Date <= eDate),select=c(Basin,Date,product,max_swe_mm))
		pngFile <- paste0(writePlotDir,"/max_swe_mm_",bDStr,"_",eDStr,"_",bName,".png")
		PlotBasSnoMetrics(n, modDfs=snowVarSub2,
                                     var=8,
                                     title=paste0("Maximum SWE (mm) for Region: ",
                                                  bName),
			             ylab="mm",
                                     xlab="Date",
                                     fileOut=pngFile)

	}

}

# MAPS

# Initialize for maps
if (strBiasMap | strCorrMap | snosweErrMap | snoprecipErrMap ) {
	library(ggplot2)
	library(ggmap)
	library(gridExtra)
	if (reachRting) {
		modStrout <- modChrtout
	} else {
		modStrout <- modFrxstout
	}
	# Setup date ranges
	stdate_stats_PRINT <- ifelse(is.null(stdate_stats), min(modStrout$POSIXct), stdate_stats)
	enddate_stats_PRINT <- ifelse(is.null(enddate_stats), max(modStrout$POSIXct), enddate_stats)
	stdate_stats_sub_PRINT <- ifelse(is.null(stdate_stats_sub), min(modStrout$POSIXct), stdate_stats_sub)
	enddate_stats_sub_PRINT <- ifelse(is.null(enddate_stats_sub), max(modStrout$POSIXct), enddate_stats_sub)
	statsDateList <- list("Full" = paste0(format(as.POSIXct(stdate_stats_PRINT, origin="1970-01-01 00:00.00 UTC", tz="UTC"), "%Y-%m-%d %H:%M"), 
			" to ", format(as.POSIXct(enddate_stats_PRINT, origin="1970-01-01 00:00.00 UTC", tz="UTC"), "%Y-%m-%d %H:%M")), 
			"Sub" = paste0(format(as.POSIXct(stdate_stats_sub_PRINT, origin="1970-01-01 00:00.00 UTC", tz="UTC"), "%Y-%m-%d %H:%M"), 
                        " to ", format(as.POSIXct(enddate_stats_sub_PRINT, origin="1970-01-01 00:00.00 UTC", tz="UTC"), "%Y-%m-%d %H:%M")))
	# Setup map
	geoMap <- SetupMap(geoFile)
}

# STRFLOW Bias Maps
if (strBiasMap) {
	message("Generating STRFLOW Bias error map...")
	# Setup
	if (is.null(strBiasTags)) strBiasTags <- unique(stats_str$tag)
	if (is.null(strBiasSeas)) strBiasSeas <- unique(stats_str$seas)
	if (writeHtml) {
        	cat('## Streamflow Bias Maps\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
		strBias.ggList <- list()
		strBias.tblList <- list()
	}
	for (i in strBiasTags) {
        	for (j in strBiasSeas) {
                	gg <- PlotMapErrors(geoMap, stats_str,
                        	statsTag=i, statsVar=NULL, statsSeas=j,
                        	plotTitle="Modeled Streamflow Bias at CODWR Gages",
				plotSubTitle=paste0(i, ", ", statsDateList[[j]]),
                        	sizeVar="t_mae", colorVar="t_bias",
                        	sizeLab="Mean Abs\nError (cms)", colorLab="Bias (%)",
				minThreshSize=0, maxThreshSize=100,
				minThreshCol=(-100), maxThreshCol=100,
				minPtsize=2, maxPtsize=8,
				colBreaks=c("#0571b0", "#92c5de", "#f7f7f7", "#f4a582", "#ca0020", "#800000"), 
                        	valBreaks=c(-Inf, -25, -10, 10, 25, 100, Inf))
                	ggplot2::ggsave(filename=paste0(writePlotDir, "/str_bias_map_", i, "_", j, ".png"),
                        	plot=gg, units="in", width=8, height=6, dpi=100)
			if (writeHtml) {
				strBias.ggList <- c(strBias.ggList, list(gg))
				if (statsMapTables) {
					tbltmp <- subset(stats_str, stats_str$tag==i & stats_str$seas==j)
					tbltmp <- tbltmp[order(tbltmp$site_no),]
                                	tbltmp <- data.frame(site_no=tbltmp$site_no, lat=tbltmp$lat, lon=tbltmp$lon, n=tbltmp$t_n, 
                                                bias=tbltmp$t_bias, mae=tbltmp$t_mae,
                                                corr=tbltmp$t_cor, daily_corr=tbltmp$dy_cor, mo_corr=tbltmp$mo_cor,
                                                nse=tbltmp$t_nse, daily_nse=tbltmp$dy_nse, mo_nse=tbltmp$mo_nse)
					strBias.tblList <- c(strBias.tblList, list(tbltmp))
				}
			}
		}
	}
	#i <- 1
	if (writeHtml) {
		for (i in 1:length(strBias.ggList)) {
		#while (i <= length(ggList)) {
			#if ( (i+1) <= length(ggList)) {
			# Map
			cat(paste0("```{r strbiasmap_", i, ", fig.width = 12, fig.height = 9, echo=FALSE}\n"), 
				file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			#plottxt <- knitr::knit_expand(text='grid.arrange(ggList[[{{i}}]], ggList[[{{i}}+1]], ncol=2)\n')
			plottxt <- knitr::knit_expand(text='strBias.ggList[[{{i}}]]\n')
			#} else {
			#	cat(paste0("```{r strbiasmap_", i, ", fig.width = 10, fig.height = 5, out.width='1000', out.height='500', echo=FALSE}\n"),   
                	#                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                	#        plottxt <- knitr::knit_expand(text='grid.arrange(ggList[[{{i}}]], ncol=1)\n')
			#}
			cat(plottxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			# Table
			if (statsMapTables) {
				cat(paste0("```{r strbiastbl_", i, ", fig.width = 8, fig.height = 6, out.width='800', out.height='600', echo=FALSE, results='asis'}\n"),
                                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
				#tbltxt <- knitr::knit_expand(text='pandoc.table(strBias.tblList[[{{i}}]], style = "simple", split.table=160)\n')
				tbltxt <- knitr::knit_expand(text='print(xtable(strBias.tblList[[{{i}}]]), type="html", comment=FALSE)\n')
				cat(tbltxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
				cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			}
			#i <- i + 2
		}
           }
}

# STRFLOW Correlation Maps
if (strCorrMap) {
	message("Generating STRFLOW Corr error map...")
	# Setup
	if (is.null(strCorrTags)) strCorrTags <- unique(stats_str$tag)
	if (is.null(strCorrSeas)) strCorrSeas <- unique(stats_str$seas)
	if (writeHtml) {
        	cat('## Streamflow Correlation Maps\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
        	strCorr.ggList <- list()
		strCorr.tblList <- list()
	}
	for (i in strCorrTags) {
        	for (j in strCorrSeas) {
                	gg <- PlotMapErrors(geoMap, stats_str,
                        	statsTag=i, statsVar=NULL, statsSeas=j,
                        	plotTitle="Modeled Streamflow Correlation at CODWR Gages",
				plotSubTitle=paste0(i, ", ", statsDateList[[j]]),
                        	sizeVar="dy_cor", colorVar="dy_cor",
                        	sizeLab="Correlation", colorLab="Correlation",
				colorLow="orange", colorMid="yellow", colorHigh="cyan4",
				minThreshSize=0, maxThreshSize=1,
                                minThreshCol=0, maxThreshCol=1,
				minPtsize=0.5, maxPtsize=6,
                                colBreaks=c("#f7f7f7", "#ffffcc", "#c2e699", "#78c679", "#238443"),
                                valBreaks=c(-1, 0.2, 0.4, 0.6, 0.8, 1.0))
                	ggplot2::ggsave(filename=paste0(writePlotDir, "/str_corr_map_", i, "_", j, ".png"),
                        	plot=gg, units="in", width=8, height=6, dpi=100)
                	if (writeHtml) {
                        	strCorr.ggList <- c(strCorr.ggList, list(gg))
				if (statsMapTables) {
                                	tbltmp <- subset(stats_str, stats_str$tag==i & stats_str$seas==j)
					tbltmp <- tbltmp[order(tbltmp$site_no),]
                                	tbltmp <- data.frame(site_no=tbltmp$site_no, lat=tbltmp$lat, lon=tbltmp$lon, n=tbltmp$t_n, 
						bias=tbltmp$t_bias, mae=tbltmp$t_mae,
						corr=tbltmp$t_cor, daily_corr=tbltmp$dy_cor, mo_corr=tbltmp$mo_cor, 
						nse=tbltmp$t_nse, daily_nse=tbltmp$dy_nse, mo_nse=tbltmp$mo_nse)
                                	strCorr.tblList <- c(strCorr.tblList, list(tbltmp))
                		}
			}
          	}
   	}
	if (writeHtml) {
        	for (i in 1:length(strCorr.ggList)) {
			# Map
                	cat(paste0("```{r strcorrmap_", i, ", fig.width = 8, fig.height = 6, out.width='800', out.height='600', echo=FALSE}\n"),
                                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        plottxt <- knitr::knit_expand(text='strCorr.ggList[[{{i}}]]\n')
                	cat(plottxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			# Table
			if (statsMapTables) {
                        	cat(paste0("```{r strcorrtbl_", i, ", fig.width = 8, fig.height = 6, out.width='800', out.height='600', echo=FALSE, results='asis'}\n"),
                                	file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        	#tbltxt <- knitr::knit_expand(text='pandoc.table(strCorr.tblList[[{{i}}]], style = "simple", split.table=160)\n')
				tbltxt <- knitr::knit_expand(text='print(xtable(strCorr.tblList[[{{i}}]]), type="html", comment=FALSE)\n')
                        	cat(tbltxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
				cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			}
                }
        }
}

# SNOTEL SWE Maps
if (snosweErrMap) {
	message("Generating SNOTEL SWE error map...")
	# Setup
	if (is.null(snosweErrTags)) snosweErrTags <- unique(stats_ldasout_sno$tag)
	if (is.null(snosweErrSeas)) snosweErrSeas <- unique(stats_ldasout_sno$seas)
        if (writeHtml) {
                cat('## SNOTEL SWE Error Maps\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                snosweErr.ggList <- list()
		snosweErr.tblList <- list()
        }
	for (i in snosweErrTags) {
		for (j in snosweErrSeas) {
			gg <- PlotMapErrors(geoMap, stats_ldasout_sno,
                        	statsTag=i, statsVar="SWE", statsSeas=j,
                        	plotTitle="Modeled SWE Errors at SNOTEL Stations",
				plotSubTitle=paste0(i, ", ", statsDateList[[j]]),
                        	sizeVar="t_mae", colorVar="t_msd",
                        	sizeLab="Mean Absolute Error (mm)", colorLab="Mean Signed Deviation (mm)")
			ggplot2::ggsave(filename=paste0(writePlotDir, "/sno_sweerr_map_", i, "_", j, ".png"), 
				plot=gg, units="in", width=9, height=6, dpi=100)
                        if (writeHtml) {
                                snosweErr.ggList <- c(snosweErr.ggList, list(gg))
                                tbltmp <- subset(stats_ldasout_sno, stats_ldasout_sno$tag==i & stats_ldasout_sno$seas==j & stats_ldasout_sno$var=="SWE")
				tbltmp <- tbltmp[order(tbltmp$site_id),]
                                tbltmp <- data.frame(site_no=tbltmp$site_id, site_name=tbltmp$site_name, 
						lat=tbltmp$lat, lon=tbltmp$lon, n=tbltmp$t_n, 
                                                bias=tbltmp$t_bias, mae=tbltmp$t_mae,
                                                corr=tbltmp$t_cor, daily_corr=tbltmp$dy_cor, mo_corr=tbltmp$mo_cor,
                                                nse=tbltmp$t_nse, daily_nse=tbltmp$dy_nse, mo_nse=tbltmp$mo_nse)
                                snosweErr.tblList <- c(snosweErr.tblList, list(tbltmp))
                        }
		}
	}
        if (writeHtml) {
                for (i in 1:length(snosweErr.ggList)) {
			# Map
                        cat(paste0("```{r snosweerrmap_", i, ", fig.width = 9, fig.height = 6, out.width='900', out.height='600', echo=FALSE}\n"),
                                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        plottxt <- knitr::knit_expand(text='snosweErr.ggList[[{{i}}]]\n')
                        cat(plottxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        # Table
                        cat(paste0("```{r snosweerrtbl_", i, ", fig.width = 8, fig.height = 6, out.width='800', out.height='600', echo=FALSE, results='asis'}\n"),
                                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        #tbltxt <- knitr::knit_expand(text='pandoc.table(snosweErr.tblList[[{{i}}]], style = "simple", split.table=160)\n')
			tbltxt <- knitr::knit_expand(text='print(xtable(snosweErr.tblList[[{{i}}]]), type="html", comment=FALSE)\n')
                        cat(tbltxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                }
        }
}

# SNOTEL Precip Maps
if (snoprecipErrMap) {
	message("Generating SNOTEL precip error map...")
	# Setup
	if (is.null(snoprecipErrTags)) snoprecipErrTags <- unique(stats_ldasout_sno$tag)
	if (is.null(snoprecipErrSeas)) snoprecipErrSeas <- unique(stats_ldasout_sno$seas)
        if (writeHtml) {
                cat('## SNOTEL Precipitation Error Maps\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                snoprecipErr.ggList <- list()
		snoprecipErr.tblList <- list()
        }
	for (i in snoprecipErrTags) {
        	for (j in snoprecipErrSeas) {
                	gg <- PlotMapErrors(geoMap, stats_ldasout_sno,
                        	statsTag=i, statsVar="Precip", statsSeas=j,
                        	plotTitle="Modeled Precipitation Errors at SNOTEL Stations",
				plotSubTitle=paste0(i, ", ", statsDateList[[j]]),
                        	sizeVar="t_mae", colorVar="t_msd",
                        	sizeLab="Mean Absolute Error (mm)", colorLab="Mean Signed Deviation (mm)")
                	ggplot2::ggsave(filename=paste0(writePlotDir, "/sno_preciperr_map_", i, "_", j, ".png"),
                        	plot=gg, units="in", width=9, height=6, dpi=100)
                        if (writeHtml) {
                                snoprecipErr.ggList <- c(snoprecipErr.ggList, list(gg))
                                tbltmp <- subset(stats_ldasout_sno, stats_ldasout_sno$tag==i & stats_ldasout_sno$seas==j & stats_ldasout_sno$var=="Precip")
				tbltmp <- tbltmp[order(tbltmp$site_id),]
                                tbltmp <- data.frame(site_no=tbltmp$site_id, site_name=tbltmp$site_name, 
                                                lat=tbltmp$lat, lon=tbltmp$lon, n=tbltmp$t_n,
                                                bias=tbltmp$t_bias, mae=tbltmp$t_mae,
                                                corr=tbltmp$t_cor, daily_corr=tbltmp$dy_cor, mo_corr=tbltmp$mo_cor,
                                                nse=tbltmp$t_nse, daily_nse=tbltmp$dy_nse, mo_nse=tbltmp$mo_nse)
                                snoprecipErr.tblList <- c(snoprecipErr.tblList, list(tbltmp))
                        }
                }
        }
        if (writeHtml) {
                for (i in 1:length(snoprecipErr.ggList)) {
                        cat(paste0("```{r snopreciperrmap_", i, ", fig.width = 9, fig.height = 6, out.width='900', out.height='600', echo=FALSE}\n"),
                                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        plottxt <- knitr::knit_expand(text='snoprecipErr.ggList[[{{i}}]]\n')
			cat(plottxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        # Table
                        cat(paste0("```{r snopreciperrtbl_", i, ", fig.width = 9, fig.height = 6, out.width='900', out.height='600', echo=FALSE, results='asis'}\n"),
                                file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                        #tbltxt <- knitr::knit_expand(text='pandoc.table(snoprecipErr.tblList[[{{i}}]], style = "simple", split.table=160)\n')
			tbltxt <- knitr::knit_expand(text='print(xtable(snoprecipErr.tblList[[{{i}}]]), type="html", comment=FALSE)\n')
                        cat(tbltxt, file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
			cat('```\n', file=paste0(writePlotDir,"/plots_stats.Rmd"), append=TRUE)
                }
        }
}

# Peak SWE Julian Day of Water Year
if (peakSweMap) {
	library(ggplot2)
	library(ggmap)
	library(ncdf4)
	# Setup date ranges
	yrPrev <- peakSweWY - 1
	dStart <- as.POSIXct(paste0(toString(yrPrev),"-10-01"),"%Y-%m-%d",tz = "UTC")
	dEnd <- as.POSIXct(paste0(toString(peakSweWY),"-09-01"),"%Y-%m-%d",tz = "UTC")
	# Setup map
	geoMap <- SetupMap(geoFile)

	message("Generating Peak SWE map....")
	# SNODAS First
	PlotPeakSweMap(geoMap, dStart, dEnd, geoNX, geoNY)
}

# Map of model vs SNODAS gridded errors
if (snodasErrorMap) {

	library(ncdf4)

	PlotSnodasGridMap(snowMapBegDate,snowMapEndDate,geoNX,geoNY)

}

# Plots of model vs obs vs SNODAS snow at points
if (snowPointScatter) {
	if (snotelScatter) {
		# SNOTEL points
		numPoints <- length(ptgeo.sno$id)
		
		modData <- modLdasout$utcday
		# Determine number of model tags to analyze
		tags <- unique(modData$tag)
		numTags <- length(tags)

		# Loop through points/tags and generate scatter plots
		for (i in 1:numPoints){
			pointId <- ptgeo.sno$id[i]

			print(pointId)
			# Set a default maximum value for plotting purposes
			maxSnow <- 500.0
			for (j in 1:numTags){
				tag <- tags[j]
			
				# model vs. obs scatter
				ind <- which(modData$statArg == pointId & modData$tag == tag & modData$POSIXct >= snowScatterBegDate &
					modData$POSIXct <= snowScatterEndDate)

				modDates <- modData$POSIXct[ind]
				modSNEQV <- modData$SNEQV_mean[ind]

				if (j == 1){
					maxSnow <- max(modSNEQV)
				}
				# Loop through found data and find corresponding SNOTEL and SNODAS data. Store in a temporary array for
				# use in the creation of a scatter plot
				numSteps <- length(ind)
			
				dateTmp <- c()
				modTmp <- c()
				obsTmp <- c()
				snodasTmp <- c()

				for (k in 1:numSteps){
					dateTmp2 <- modDates[k]

					ind <- which(obsSnoData$POSIXct == dateTmp2 & obsSnoData$site_id == pointId)
					obsTmp <- c(obsTmp, obsSnoData$SWE_mm[ind[1]])

					ind <- which(snodasout$utcday$statArg == pointId & snodasout$utcday$POSIXct == dateTmp2)
					snodasTmp <- c(snodasTmp, snodasout$utcday$SNEQV[ind])

					dateTmp <- c(dateTmp, as.numeric(strftime(dateTmp2,format="%m")))
					modTmp <- c(modTmp, modSNEQV[k])
					
				}

				# Filter any NA values that may have snuck in
				ind4 <- which(!is.na(modTmp) & !is.na(obsTmp) & !is.na(snodasTmp))
				if (length(ind4) == 0){
					break
				}
				obsTmp <- obsTmp[ind4]
				modTmp <- modTmp[ind4]
				snodasTmp <- snodasTmp[ind4]
				dateTmp <- dateTmp[ind4]

				# Create data frame to hold data
				dfTmp <- data.frame(matrix(NA, nrow=length(obsTmp), ncol=4))
                                names(dfTmp) <- c("Month","Model","SNOTEL","SNODAS")
				dfTmp$Month <- dateTmp
				dfTmp$SNOTEL <- obsTmp
				dfTmp$Model <- modTmp
				dfTmp$SNODAS <- snodasTmp

				# Reset numSteps in case any NA values were filtered out
				numSteps <- length(obsTmp)

				# Model vs. SNOTEL
				pathOut <- paste0(writePlotDir,"/MODEL_SNOTEL_SCATTER_",pointId,"_",tag,"_",
						strftime(snowScatterBegDate,format="%Y%m%d"),"_",
						strftime(snowScatterEndDate,format="%Y%m%d"),".png")

				title <- paste0('SNOTEL ',pointId,' ',tag,' vs. SNOTEL: ',
						strftime(snowScatterEndDate,format="%Y"),' WY')
				xlab <- 'SNOTEL SWE (mm)'
				ylab <- 'Model SWE (mm)'

				# Linear Regression Coefficient Calculations
				lmOut <- lm(Model ~ SNOTEL, dfTmp)
				slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
				cc <- format(round(cor(dfTmp$Model,dfTmp$SNOTEL),3),nsmall=2)

				gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=SNOTEL,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
					ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
					theme(plot.title = element_text(size=20)) +
					theme(axis.title.x = element_text(size=20)) + 
					theme(axis.title.y = element_text(size=20)) +  
					ggplot2::labs(colour="Month") + 
					theme(legend.title = element_text(size=15)) + 
					theme(legend.text = element_text(size=15)) + 
					ggplot2::geom_abline(intercept = 0, slope = 1) + 
					coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
					annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
						colour="darkred", family="serif", fontface="italic", size = 7) +
					annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
					annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                         colour="darkred", family="serif", fontface="italic", size = 7)
				ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

				# Model vs. SNODAS
				pathOut <- paste0(writePlotDir,"/MODEL_SNODAS_SCATTER_",pointId,"_",tag,"_",
                                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")

				title <- paste0('SNOTEL ',pointId,' ',tag,' vs. SNODAS: ',
                                                strftime(snowScatterEndDate,format="%Y"),' WY')
				xlab <- 'SNODAS SWE (mm)'
                                ylab <- 'Model SWE (mm)'

				# Linear Regression Coefficient Calculations
				lmOut <- lm(Model ~ SNODAS, dfTmp)
				slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
				icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
				cc <- format(round(cor(dfTmp$Model,dfTmp$SNODAS),3),nsmall=2)

                                gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=SNODAS,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
					ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + 
					theme(plot.title = element_text(size=20)) +
					theme(axis.title.x = element_text(size=20)) +
                                        theme(axis.title.y = element_text(size=20)) +
					ggplot2::labs(colour="Month") +
					theme(legend.title = element_text(size=15)) +
                                        theme(legend.text = element_text(size=15)) +
					ggplot2::geom_abline(intercept = 0, slope = 1) + 
					coord_cartesian(xlim = c(0, maxSnow),ylim = c(0, maxSnow)) +
                                	annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) + 
					annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                         colour="darkred", family="serif", fontface="italic", size = 7)
				ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

			        # SNODAS vs. SNOTEL
                                pathOut <- paste0(writePlotDir,"/SNODAS_SNOTEL_SCATTER_",pointId,"_",tag,"_",
                                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")

				title <- paste0('SNOTEL ',pointId,' ',' vs. SNOTEL : ',
                                                strftime(snowScatterEndDate,format="%Y"),' WY')
				xlab <- 'SNOTEL SWE (mm)'
                                ylab <- 'SNODAS SWE (mm)'

				# Linear Regression Coefficient Calculations
				lmOUt <- lm(SNODAS ~ SNOTEL, dfTmp)
				slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
				cc <- format(round(cor(dfTmp$SNODAS, dfTmp$SNOTEL),3),nsmall=2)

                                gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=SNOTEL,y=SNODAS)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
					ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
					theme(plot.title = element_text(size=20)) +
					theme(axis.title.x = element_text(size=20)) +
                                        theme(axis.title.y = element_text(size=20)) +
					ggplot2::labs(colour="Month") +
					theme(legend.title = element_text(size=15)) +
                                        theme(legend.text = element_text(size=15)) +
					ggplot2::geom_abline(intercept = 0, slope = 1) + 
					coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) + 
                                	annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) + 
					annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                         colour="darkred", family="serif", fontface="italic", size = 7)
				ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)
	
				rm(dfTmp)

			}
		}
	}
	# MET points
	if (metScatter) {
		# MET points
		numPoints <- length(ptgeo.met$id)
		
		modData <- modLdasout$utcday
                # Determine number of model tags to analyze
                tags <- unique(modData$tag)
                numTags <- length(tags)

		# Establish temporary arrays to hold data for aggegation plots
                obsTmp <- c()
                modTmp <- c()
                snodasTmp <- c()
                dateTmp2 <- c()
		tagTmp <- c()
                # Loop through points/tags and generate scatter plots
                for (i in 1:numPoints){
                        pointId <- ptgeo.met$id[i]

			# Set a default maximum value for plotting purposes
                        maxSnow <- 1000.0
                        for (j in 1:numTags){
                                tag <- tags[j]

                                # model vs. obs scatter
				ind <- which(obsMetData.dy$site_id == pointId & obsMetData.dy$POSIXct >= snowScatterBegDate &
					obsMetData.dy$POSIXct <= snowScatterEndDate & 
					!is.nan(obsMetData.dy$SnoDep_mean))
				obsDates <- obsMetData.dy$POSIXct[ind]
				obsSNOWH <- obsMetData.dy$SnoDep_mean[ind]

                                if (j == 1){
                                        maxSnow <- max(obsSNOWH)
                                }
                                # Loop through found data and find corresponding model and SNODAS data. Store in a temporary array for
                                # use in the creation of a scatter plot
                                numSteps <- length(ind)

				dfTmp <- data.frame(matrix(NA, nrow=numSteps, ncol=4))
                                names(dfTmp) <- c("Month","Model","HydroMet","SNODAS")

                                dfTmp$Month <- NA
                                dfTmp$Model <- NA
                                dfTmp$HydroMet <- NA
                                dfTmp$SNODAS <- NA
                                for (k in 1:numSteps){
                                        dateTmp <- obsDates[k]
                                        dfTmp$Month[k] <- as.numeric(strftime(dateTmp,format="%m"))
					dfTmp$HydroMet[k] <- obsSNOWH[k]

					ind <- which(modData$statArg == pointId & modData$tag == tag & 
						modData$POSIXct == dateTmp)
					dfTmp$Model[k] <- modData$SNOWH_mean[ind[1]]*1000.0

                                        ind2 <- which(snodasout$utcday$statArg == pointId & snodasout$utcday$POSIXct == dateTmp)
                                        dfTmp$SNODAS[k] <- snodasout$utcday$SNOWH[ind2[1]]

					#Store data to temporary arrays
					obsTmp <- c(obsTmp, dfTmp$HydroMet[k])
					modTmp <- c(modTmp, dfTmp$Model[k])
					snodasTmp <- c(snodasTmp, dfTmp$SNODAS[k])
					dateTmp2 <- c(dateTmp2, as.numeric(strftime(dateTmp,format="%m")))
					tagTmp <- c(tagTmp, j)
                                }

				ind4 <- which(!is.na(obsTmp) & !is.na(modTmp) & !is.na(snodasTmp))
				if (length(ind4) == 0){
					break
				}
				obsTmp <- obsTmp[ind4]
				modTmp <- modTmp[ind4]
				snodasTmp <- snodasTmp[ind4]
				dateTmp2 <- dateTmp2[ind4]
				tagTmp <- tagTmp[ind4]

				# Model vs. HydroMet
                                pathOut <- paste0(writePlotDir,"/MODEL_MET_SCATTER_",pointId,"_",tag,"_",
                                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")

                                title <- paste0('HydroMet ',pointId,' ',tag,' vs. HydroMet: ',
                                                strftime(snowScatterEndDate,format="%Y"),' WY')
                                xlab <- 'HydroMet Snow Depth (mm)'
                                ylab <- 'Model Snow Depth (mm)'

				# Linear Regression Coefficient Calculations
				lmOut <- lm(Model ~ HydroMet, dfTmp)
                                slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
				cc <- format(round(cor(dfTmp$Model, dfTmp$HydroMet),3),nsmall=2)

                                gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=HydroMet,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
                                        ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
					theme(plot.title = element_text(size=20)) +
                                        theme(axis.title.x = element_text(size=20)) +
                                        theme(axis.title.y = element_text(size=20)) +
                                        ggplot2::labs(colour="Month") +
					theme(legend.title = element_text(size=15)) +
                                        theme(legend.text = element_text(size=15)) +
                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                        coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
					annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                         colour="darkred", family="serif", fontface="italic", size = 7)
                                ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

				# Model vs. SNODAS
                                pathOut <- paste0(writePlotDir,"/MODEL_SNODAS_SCATTER_",pointId,"_",tag,"_",
                                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")

                                title <- paste0('HydroMet ',pointId,' ',tag,' vs. SNODAS: ',
                                                strftime(snowScatterEndDate,format="%Y"),' WY')
                                xlab <- 'SNODAS Snow Depth (mm)'
                                ylab <- 'Model Snow Depth (mm)'

                                # Linear Regression Coefficient Calculations
				lmOut <- lm(Model ~ SNODAS, dfTmp)
                                slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
				cc <- format(round(cor(dfTmp$Model, dfTmp$SNODAS),3),nsmall=2)

                                gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=SNODAS,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
                                        ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
					theme(plot.title = element_text(size=20)) +
                                        theme(axis.title.x = element_text(size=20)) +
                                        theme(axis.title.y = element_text(size=20)) +
                                        ggplot2::labs(colour="Month") +
					theme(legend.title = element_text(size=15)) +
                                        theme(legend.text = element_text(size=15)) +
                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                        coord_cartesian(xlim = c(0, maxSnow),ylim = c(0, maxSnow)) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) + 
					annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                         colour="darkred", family="serif", fontface="italic", size = 7)
                                ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

				# SNODAS vs. HydroMet
                                pathOut <- paste0(writePlotDir,"/SNODAS_MET_SCATTER_",pointId,"_",tag,"_",
                                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")

                                title <- paste0('HydroMet ',pointId,' ',' vs. SNODAS: ',
                                                strftime(snowScatterEndDate,format="%Y"),' WY')
                                xlab <- 'HydroMet Snow Depth (mm)'
                                ylab <- 'SNODAS Snow Depth (mm)'

                                # Linear Regression Coefficient Calculations
				lmOut <- lm(SNODAS ~ HydroMet, dfTmp)
                                slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
				cc <- format(round(cor(dfTmp$SNODAS, dfTmp$HydroMet),3),nsmall=2)

                                gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=HydroMet,y=SNODAS)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
                                        ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
					theme(plot.title = element_text(size=20)) +
                                        theme(axis.title.x = element_text(size=20)) +
                                        theme(axis.title.y = element_text(size=20)) +
                                        ggplot2::labs(colour="Month") +
					theme(legend.title = element_text(size=15)) +
                                        theme(legend.text = element_text(size=15)) +
                                        ggplot2::geom_abline(intercept = 0, slope = 1) +
                                        coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
                                        annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                colour="darkred", family="serif", fontface="italic", size = 7) +
					annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                         colour="darkred", family="serif", fontface="italic", size = 7)
                                ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

                                rm(dfTmp)

			} # Done with model groups
		} # Done looping through observation points

		# Create aggregated scatter plots based on temporary arrays.
		for (i in 1:numTags) {
			indTmp <- which(tagTmp == i)
			maxSnow <- 1250.0

			tag <- tags[i]
			# Create scatter plots of aggregated observations/model/SNODAS
			numAll <- length(indTmp)
			dfTmp <- data.frame(matrix(NA, nrow=numAll, ncol=4))
                        names(dfTmp) <- c("Month","Model","Observation","SNODAS")
                        dfTmp$Month <- dateTmp2[indTmp]
                        dfTmp$Model <- modTmp[indTmp]
                        dfTmp$Observation <- obsTmp[indTmp]
                        dfTmp$SNODAS <- snodasTmp[indTmp]

			# Plot model vs. observations
                        pathOut <- paste0(writePlotDir,"/MODEL_MET_SCATTER_",tag,"_",
                        	strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                        	strftime(snowScatterEndDate,format="%Y%m%d"),".png")
                        title <- paste0(tag,' vs. Hydro-Met: ',
                        strftime(snowScatterEndDate,format="%Y"),' WY')
                        xlab <- 'Hydro-Met Snow Depth (mm)'
                        ylab <- 'Model Snow Depth (mm)'
	
			# Linear Regression Coefficient Calculations
			lmOut <- lm(Model ~ Observation, dfTmp)
                        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
                        cc <- format(round(cor(dfTmp$Observation,dfTmp$Model),3),nsmall=3)

                        gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=Observation,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
                                ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                                theme(plot.title = element_text(size=20)) +
                                theme(axis.title.x = element_text(size=20)) +
                                theme(axis.title.y = element_text(size=20)) +
                                ggplot2::labs(colour="Month") +
                                theme(legend.title = element_text(size=15)) +
                                theme(legend.text = element_text(size=15)) +
                                ggplot2::geom_abline(intercept = 0, slope = 1) +
                                coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                         colour="darkred", family="serif", fontface="italic", size = 7) +
                                annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                         colour="darkred", family="serif", fontface="italic", size = 7) +
                                annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                         colour="darkred", family="serif", fontface="italic", size = 7)
                        ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

			# Plot model vs. SNOTEL
			pathOut <- paste0(writePlotDir,"/MODEL_SNODAS_MET_SCATTER_",tag,"_",
                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")
                        title <- paste0(tag,' vs. SNODAS at Hydro-Met Stations: ',
                        strftime(snowScatterEndDate,format="%Y"),' WY')
                        xlab <- 'SNODAS Snow Depth (mm)'
                        ylab <- 'Model Snow Depth (mm)'

                        # Linear Regression Coefficient Calculations
			lmOut <- lm(Model ~ SNODAS, dfTmp)
                        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
                        cc <- format(round(cor(dfTmp$SNODAS,dfTmp$Model),3),nsmall=3)

                        gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=SNODAS,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
                                ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                                theme(plot.title = element_text(size=20)) +
                                theme(axis.title.x = element_text(size=20)) +
                                theme(axis.title.y = element_text(size=20)) +
                                ggplot2::labs(colour="Month") +
                                theme(legend.title = element_text(size=15)) +
                                theme(legend.text = element_text(size=15)) +
                                ggplot2::geom_abline(intercept = 0, slope = 1) +
                                coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                         colour="darkred", family="serif", fontface="italic", size = 7) +
                                annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                         colour="darkred", family="serif", fontface="italic", size = 7) +
                                annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                         colour="darkred", family="serif", fontface="italic", size = 7)
                        ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

			# Plot Hydro-Met vs. SNODAS
			pathOut <- paste0(writePlotDir,"/SNODAS_MET_SCATTER_",tag,"_",
                                strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                strftime(snowScatterEndDate,format="%Y%m%d"),".png")
                        title <- paste0('SNODAS vs. Hydro-Met: ',
                        strftime(snowScatterEndDate,format="%Y"),' WY')
                        xlab <- 'Hydro-Met Snow Depth (mm)'
                        ylab <- 'SNODAS Snow Depth (mm)'

                        # Linear Regression Coefficient Calculations
			lmOut <- lm(SNODAS ~ Observation, dfTmp)
                        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
                        cc <- format(round(cor(dfTmp$Observation,dfTmp$SNODAS),3),nsmall=3)

                        gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=Observation,y=SNODAS)) + ggplot2::geom_point(aes(colour = factor(Month)),size=5) +
                                ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                                theme(plot.title = element_text(size=20)) +
                                theme(axis.title.x = element_text(size=20)) +
                                theme(axis.title.y = element_text(size=20)) +
                                ggplot2::labs(colour="Month") +
                                theme(legend.title = element_text(size=15)) +
                                theme(legend.text = element_text(size=15)) +
                                ggplot2::geom_abline(intercept = 0, slope = 1) +
                                coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                         colour="darkred", family="serif", fontface="italic", size = 7) +
                                annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                         colour="darkred", family="serif", fontface="italic", size = 7) +
                                annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                         colour="darkred", family="serif", fontface="italic", size = 7)
                        ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

			rm(dfTmp)

		}
	} # Done with hydro-met scatter plots
	# If performing over basin, number of basins
        # Aggregate point analysis to basin (region) scale based on mask file.
	if (basinScatter) {
		numBas <- length(mskgeo.nameList)

		if (snotelScatter) {
			numPoints <- length(ptgeo.sno$id)
                }
		if (metScatter) {
			numPoints <- length(ptgeo.met$id)
		}

		modData <- modLdasout$utcday
                # Determine number of model tags to analyze
                tags <- unique(modData$tag)
                numTags <- length(tags)

		for (i in 1:numBas) {
			basId <- mskgeo.nameList[[i]]
			maxSnow <- 500.0

			print(basId)
			for (j in 1:numTags){
				tag <- tags[j]

				# Establish temporary arrays to hold data
				obsTmp <- c()
				modTmp <- c()
				snodasTmp <- c()
				dateTmp <- c()
				# Loop through observation points
				# If region tag is true, calculate aggregate statistics
				for (k in 1:numPoints) {
					if (snotelScatter) {
						pointId <- ptgeo.sno$id[k]
						regionTagList <- ptgeo.sno$region[[k]]
					}
					if (metScatter) {
						pointId <- ptgeo.met$id[k]
						regionTagList <- ptgeo.met$region[[k]]
					}

					print(pointId)
					indTmp <- which(names(regionTagList) == basId)
					if (regionTagList[[indTmp]]) { # Observation data is within this region
						if (snotelScatter) {
							ind <- which(obsSnoData$site_id == pointId & obsSnoData$POSIXct >= snowScatterBegDate &
                                        			obsSnoData$POSIXct <= snowScatterEndDate) 
							obsDates <- obsSnoData$POSIXct[ind]
							obsSNOW <- obsSnoData$SWE_mm[ind]
						}
						if (metScatter) {
							ind <- which(obsMetData.dy$site_id == pointId & obsMetData.dy$POSIXct >= snowScatterBegDate &
                                        		obsMetData.dy$POSIXct <= snowScatterEndDate) 
							obsDates <- obsMetData.dy$POSIXct[ind]
                                			obsSNOW <- obsMetData.dy$SnoDep_mean[ind]
						}

						numSteps <- length(ind)
						if (numSteps > 0){
							for (m in 1:numSteps){
								tmpDate <- obsDates[m]
								tmpDate2 <- as.numeric(strftime(tmpDate,format="%m"))
								tmpObs <- obsSNOW[m]
								ind2 <- which(modData$statArg == pointId & modData$tag == tag &
										modData$POSIXct == tmpDate)
								if (snotelScatter) {
									tmpMod <- modData$SNEQV_mean[ind2[1]]
								}
								if (metScatter) {
									tmpMod <- modData$SNOWH_mean[ind2]
								}

								ind3 <- which(snodasout$utcday$statArg == pointId & snodasout$utcday$POSIXct == tmpDate)
								if (snotelScatter) {
									tmpSnodas <- snodasout$utcday$SNEQV[ind3[1]]
								}
								if (metScatter) {
									tmpSnodas <- snodasout$utcday$SNOWH[ind3]
								}

								obsTmp <- c(obsTmp, tmpObs)
								modTmp <- c(modTmp, tmpMod)
								snodasTmp <- c(snodasTmp, tmpSnodas)
								dateTmp <- c(dateTmp, tmpDate2)
							} # Done looping through time steps in model
							# Filter out NA values that may have snuck in
							ind4 <- which(!is.na(obsTmp) & !is.na(modTmp) & !is.na(snodasTmp))
							if (length(ind4) == 0){
								break
							}
							obsTmp <- obsTmp[ind4]
							modTmp <- modTmp[ind4]
							snodasTmp <- snodasTmp[ind4]
							dateTmp <- dateTmp[ind4]
						} # Done with checking for numSteps > 0
					} # Done with region TRUE/FALSE check

				} # Done looping through observation points

				# Create output data frame based on size of points found within basin.
				if (length(obsTmp) > 0) {
					numAll <- length(obsTmp)
					dfTmp <- data.frame(matrix(NA, nrow=numAll, ncol=4))
					names(dfTmp) <- c("Month","Model","Observation","SNODAS")
					dfTmp$Month <- dateTmp
					dfTmp$Model <- modTmp
					dfTmp$Observation <- obsTmp
					dfTmp$SNODAS <- snodasTmp

					if (j == 1){
						maxSnow <- max(obsTmp)
					}

					# Plot model vs. observations
					pathOut <- paste0(writePlotDir,"/MODEL_OBS_SCATTER_",basId,"_",tag,"_",
							strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                	strftime(snowScatterEndDate,format="%Y%m%d"),".png")
					if (snotelScatter) {
						title <- paste0(basId,' ',tag,' vs. SNOTEL: ',
                                                	strftime(snowScatterEndDate,format="%Y"),' WY')
                                		xlab <- 'SNOTEL SWE (mm)'
                                		ylab <- 'Model SWE (mm)'
					}
					if (metScatter) {
						title <- paste0('HydroMet ',basId,' ',tag,' vs. HydroMet:: ',
                                               	strftime(snowScatterEndDate,format="%Y"),' WY')
                                		xlab <- 'HydroMet Snow Depth (mm)'
                                		ylab <- 'Model Snow Depth (mm)'
					}

                                	# Linear Regression Coefficient Calculations
					lmOut <- lm(Model ~ Observation, dfTmp)
                                	slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                	icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
					cc <- format(round(cor(dfTmp$Observation,dfTmp$Model),3),nsmall=3)

                                	gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=Observation,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=2) +
                                        	ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                                        	theme(plot.title = element_text(size=20)) +
                                        	theme(axis.title.x = element_text(size=20)) +
                                        	theme(axis.title.y = element_text(size=20)) +
                                        	ggplot2::labs(colour="Month") +
                                        	theme(legend.title = element_text(size=15)) +
                                        	theme(legend.text = element_text(size=15)) +
                                        	ggplot2::geom_abline(intercept = 0, slope = 1) +
                                        	coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                        	annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                	colour="darkred", family="serif", fontface="italic", size = 7) +
                                        	annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                	colour="darkred", family="serif", fontface="italic", size = 7) + 
						annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                        colour="darkred", family="serif", fontface="italic", size = 7)
                                	ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

					 # Plot model vs. SNODAS
                                        pathOut <- paste0(writePlotDir,"/MODEL_SNODAS_SCATTER_",basId,"_",tag,"_",
                                                        strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                        strftime(snowScatterEndDate,format="%Y%m%d"),".png")
                                        if (snotelScatter) {
                                                title <- paste0(basId,' ',tag,' vs. SNODAS: ',
                                                        strftime(snowScatterEndDate,format="%Y"),' WY')
                                                xlab <- 'SNODAS SWE (mm)'
                                                ylab <- 'Model SWE (mm)'
                                        }
                                        if (metScatter) {
                                               title <- paste0('HydroMet ',basId,' ',tag,' vs. SNODAS:: ',
                                                       strftime(snowScatterEndDate,format="%Y"),' WY')
                                               xlab <- 'SNODAS Snow Depth (mm)'
                                               ylab <- 'Model Snow Depth (mm)'
                                        }

                                        # Linear Regression Coefficient Calculations
					lmOut <- lm(Model ~ SNODAS, dfTmp)
                                        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
					cc <- format(round(cor(dfTmp$SNODAS,dfTmp$Model),3),nsmall=3)

                                        gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=SNODAS,y=Model)) + ggplot2::geom_point(aes(colour = factor(Month)),size=2) +
                                                ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                                                theme(plot.title = element_text(size=20)) +
                                                theme(axis.title.x = element_text(size=20)) +
                                                theme(axis.title.y = element_text(size=20)) +
                                                ggplot2::labs(colour="Month") +
                                                theme(legend.title = element_text(size=15)) +
                                                theme(legend.text = element_text(size=15)) +
                                                ggplot2::geom_abline(intercept = 0, slope = 1) +
                                                coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                                annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                        colour="darkred", family="serif", fontface="italic", size = 7) +
                                                annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                        colour="darkred", family="serif", fontface="italic", size = 7) + 
						annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
                                                        colour="darkred", family="serif", fontface="italic", size = 7)
                                        ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

					# Plot SNODAS vs. Observation
                                        pathOut <- paste0(writePlotDir,"/SNODAS_OBSERVATION_SCATTER_",basId,"_",tag,"_",
                                                        strftime(snowScatterBegDate,format="%Y%m%d"),"_",
                                                        strftime(snowScatterEndDate,format="%Y%m%d"),".png")
                                        if (snotelScatter) {
                                                title <- paste0('SNOTEL ',basId,' vs. SNODAS: ',
                                                        strftime(snowScatterEndDate,format="%Y"),' WY')
                                                xlab <- 'SNOTEL SWE (mm)'
                                                ylab <- 'SNODAS SWE (mm)'
                                        }
                                        if (metScatter) {
                                               title <- paste0('HydroMet ',basId,' vs. SNODAS:: ',
                                                       strftime(snowScatterEndDate,format="%Y"),' WY')
                                               xlab <- 'HydroMet Snow Depth (mm)'
                                               ylab <- 'SNODAS Snow Depth (mm)'
                                        }

                                        # Linear Regression Coefficient Calculations
					lmOut <- lm(SNODAS ~ Observation, dfTmp)
                                        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
                                        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
					cc <- format(round(cor(dfTmp$Observation,dfTmp$SNODAS),3),nsmall=3)

                                        gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=Observation,y=SNODAS)) + ggplot2::geom_point(aes(colour = factor(Month)),size=2) +
                                                ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
                                                theme(plot.title = element_text(size=20)) +
                                                theme(axis.title.x = element_text(size=20)) +
                                                theme(axis.title.y = element_text(size=20)) +
                                                ggplot2::labs(colour="Month") +
                                                theme(legend.title = element_text(size=15)) +
                                                theme(legend.text = element_text(size=15)) +
                                                ggplot2::geom_abline(intercept = 0, slope = 1) +
                                                coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
                                                annotate("text", x= (0.8*maxSnow), y = (0.3*maxSnow), label = paste0("Slope: ",toString(slope)),
                                                        colour="darkred", family="serif", fontface="italic", size = 7) +
                                                annotate("text", x= (0.8*maxSnow), y = (0.2*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                                                        colour="darkred", family="serif", fontface="italic", size = 7) + 
						annotate("text", x= (0.8*maxSnow), y = (0.1*maxSnow), label = paste0("Correlation: ",toString(cc)),
							colour="darkred", family="serif", fontface="italic", size = 7)
                                        ggplot2::ggsave(filename=pathOut,plot=gg, units="in", width=8, height=6, dpi=100)

				} # Done checking if data was found within basin
			} # Done looping through model groups 
		} # Done looping through basins
	} # Done basin scatter plots
	

}
# SNOTEL accumulated precipitation against modeled accumulated precipitation plots.
if (snotelAccPcpPlot) {
	numPoints <- length(ptgeo.sno$id)
	modData <- modLdasout$native
	
	bDStr = strftime(snotelAccPcpBegDate,format="%Y%m%d")
	eDStr = strftime(snotelAccPcpEndDate,format="%Y%m%d")

	tags <- unique(modLdasout$native$tag)
	numTags <- length(tags)
	# Loop through points and generate time series plots of accumulated precipitation
	for (i in 1:numPoints){
		pointId <- ptgeo.sno$id[i]

		# Subset observed SNOTEL cumulative precipitation and subtract out first value to get
		# cumulative from beginning of period
		snotelTmp <- subset(obsSnoData, site_id == pointId & POSIXct >= snotelAccPcpBegDate &
                                        POSIXct <= snotelAccPcpEndDate)
		
		print(pointId)
		ind <- which(modData$statArg == pointId & modData$POSIXct >= snotelAccPcpBegDate &
					modData$POSIXct <= snotelAccPcpEndDate)

		modDates <- unique(modData$POSIXct[ind])
		maxPcp <- max(modData$ACCPRCP[ind])
		numSteps <- length(modDates)

		snotelSum <- c()
		for (j in 1:numSteps) {
			if (j == 1) {
				snotelSum <- c(snotelSum, 0.0)
				indSnotel <- which(snotelTmp$POSIXct == modDates[j])
				baseValue <- snotelTmp$CumPrec_mm[indSnotel[1]]
                        } else {
				indSnotel <- which(snotelTmp$POSIXct == modDates[j])
				if (length(indSnotel) == 0) {
					snotelSum <- c(snotelSum, snotelSum[j-1])
				} else {
					diff <- (snotelTmp$CumPrec_mm[indSnotel[1]] - baseValue) - snotelSum[j-1]
					if (is.na(diff)) {
						snotelSum <- c(snotelSum, snotelSum[j-1])
					} else {
						if (diff >= 0.0) {
							snotelSum <- c(snotelSum, (snotelSum[j-1] + diff))
						} else { 
							snotelSum <- c(snotelSum, snotelSum[j-1])
							# Reset base value for new water year, or if precip is reset.
							baseValue <- 0.0 - snotelSum[j-1] 
						}
					}
				}
			}
			if (is.na(baseValue)){
				baseValue <- 0.0
			}
		}
		indNDV <- which(is.na(snotelTmp$CumPrec_mm))
		snotelSum[indNDV] <- NA
	
		# Create data frame to hold data for plotting
		dfTmp <- data.frame(matrix(NA, nrow=numSteps*(numTags+1),ncol=3))
		names(dfTmp) <- c("POSIXct","tag","ACC_PCP")
		dfTmp$POSIXct <- as.POSIXct('1900-01-01','%Y-%m-%d')

	 	count <- 1
		for (j in 1:numSteps){
			dateTmp <- modDates[j]
			print(dateTmp)
			dfTmp$POSIXct[count] <- dateTmp
			dfTmp$tag[count] <- "SNOTEL"
			dfTmp$ACC_PCP[count] <- snotelSum[j] 
			count <- count + 1
			for (k in 1:numTags){
				tag <- tags[k]
				dfTmp$POSIXct[count] <- dateTmp
				dfTmp$tag[count] <- tag
				indTmp1 <- which(modData$statArg == pointId & modData$POSIXct == dateTmp &
					         modData$tag == tag)
				indTmp2 <- which(modData$statArg == pointId & modData$POSIXct == snotelAccPcpBegDate &
					         modData$tag == tag)
				dfTmp$ACC_PCP[count] <- modData$ACCPRCP[indTmp1] - modData$ACCPRCP[indTmp2]	
				count <- count + 1
			}
		}

		title <- paste0("SNOTEL Site: ",pointId," Accumulated Precipitation")
		# Create time series plot
		fileOut <- paste0(writePlotDir,"/SNOTEL_",pointId,"_ACCPCP_",bDStr,"_",eDStr,".png")
		gg <- ggplot2::ggplot(dfTmp,ggplot2::aes(x=POSIXct,y=ACC_PCP,color=tag)) + ggplot2::geom_line() + 
		      ggplot2::ggtitle(title) + ggplot2::xlab('Date') + ggplot2::ylab('Accumulated Precipitation (mm)')
		# Save figure
		ggsave(filename = fileOut, plot = gg)
	}
 
}
# Output HTML
if (writeHtml) {
	if (accflowPlot | hydroPlot | flowlsmPlot) {
		knit2html(paste0(writePlotDir,"/plots_hydro.Rmd"), paste0(writePlotDir,"/plots_hydro.html"))
		file.remove("plots_hydro.md")
	}
	if (accprecipPlot) {	
		knit2html(paste0(writePlotDir,"/plots_climate.Rmd"), paste0(writePlotDir,"/plots_climate.html"))
		file.remove("plots_climate.md")
	}
	if (flowswePlot | indSwePlot) {	
		knit2html(paste0(writePlotDir,"/plots_snow.Rmd"), paste0(writePlotDir,"/plots_snow.html"))
		file.remove("plots_snow.md")
	}
	if (strBiasMap | strCorrMap | snosweErrMap | snoprecipErrMap) {	
		knit2html(paste0(writePlotDir,"/plots_stats.Rmd"), paste0(writePlotDir,"/plots_stats.html"))
		file.remove("plots_stats.md")
	}
	unlink("figure", recursive=TRUE)
}

## ------------------------------------------------------------------------
# Cleanup

proc.time()    

