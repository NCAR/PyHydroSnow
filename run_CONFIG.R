###################################################
##  Main Control Script to process model output  ##
###################################################

## RUN (do not change anything below this line)

library(rwrfhydro)
library(data.table)
library(scales)

if (!is.null(maskFile)){
	load(maskFile)
}
source("util_FUNC.R")

# If reach routing was specified but no rtlinks object exists,
# read it in from the routeLinksFile.
if (reachRting == TRUE){
	if (is.null(routeLinkFile)){
		if (is.null(rtLinks)){
			stop(paste('ERROR: No route link file specified and rtLinks object does not exist'))
	  	}
	} else {
		rtLinks <- ReadRouteLink(routeLinkFile)
	}
}	

# If subsetting of basins has been enabled, subset basins/frxst points
# immediately before anything else is done.
if (exists("subSet") & exists("frxstPts") & !is.null(subSet)){
  listSubBasin <- subsetBasins(basinSub,
			       mskgeo.nameList, 
 		             frxstPts, 
			       basin2gageList, 
			       gage2basinList,
                        mskgeo.areaList, 
			       mskgeo.countInds, 
			       mskgeo.List, 
			       mskgeo.maxInds,
                        mskgeo.minInds, 
			       mskhyd.areaList, 
			       mskhyd.countInds,
                        mskhyd.List, 
			       mskhyd.maxInds, 
			       mskhyd.minInds,
                        mskhyd.nameList,
			       stid2gageList)
  mskgeo.nameList <- listSubBasin[[1]]
  frxstPts <- listSubBasin[[2]]
  basin2gageList <- listSubBasin[[3]]
  gage2basinList <- listSubBasin[[4]]
  mskgeo.areaList <- listSubBasin[[5]]
  mskgeo.countInds <- listSubBasin[[6]]
  mskgeo.List <- listSubBasin[[7]]
  mskgeo.maxInds <- listSubBasin[[8]]
  mskgeo.minInds <- listSubBasin[[9]]
  mskhyd.areaList <- listSubBasin[[10]]
  mskhyd.countInds <- listSubBasin[[11]]
  mskhyd.List <- listSubBasin[[12]]
  mskhyd.maxInds <- listSubBasin[[13]]
  mskhyd.minInds <- listSubBasin[[14]]
  mskhyd.nameList <- listSubBasin[[15]]
  stid2gageList <- listSubBasin[[16]]
}

if (exists("subSet") & !exists("frxstPts") & !exists("mskhyd.nameList") & exists("mskgeo.nameList") & !is.null(subSet)){
  listSubBasin <- subsetRegions(subSet,
                                mskgeo.nameList,
                                basin2gageList,
                                mskgeo.areaList,
                                mskgeo.countInds,
                                mskgeo.List,
                                mskgeo.maxInds,
                                mskgeo.minInds)
  mskgeo.nameList <- listSubBasin[[1]]
  basin2gageList <- listSubBasin[[2]]
  mskgeo.areaList <- listSubBasin[[3]]
  mskgeo.countInds <- listSubBasin[[4]]
  mskgeo.List <- listSubBasin[[5]]
  mskgeo.maxInds <- listSubBasin[[6]]
  mskgeo.minInds <- listSubBasin[[7]]
}

# Read in snow data from model + database of observations
if (readPointSnow){
    # Points only
    if (!readBasinSnow){
        if (!readSnodas){
            # Model + Observations at observation points
            source("read_SNOW_POINTS_Database.R")
        } else if (readSnodas){
            # Model + SNODAS at observation points
            source("read_SNOW_SNODAS_POINTS_Database.R")        
        } 
    # Points + Basins
    } else if (readBasinSnow){
        if (!readSnodas){
            # Model + Observations at observation points and Model aggregated
            # to regions
            source("read_SNOW_POINTS_Database_Regions.R")        
        } else if (readSnodas){
            # Model + Observations + SNODAS at observation points and Model +
            # SNODAS aggregated to regions
            source("read_SNOW_SNODAS_POINTS_Database_Regions.R")
        }
    }
}

# Read in streamflow data from model + database of observations
if (readStreamFrxst){
    source("read_FRXST_PTS_Database.R")
} else if (readStreamChrtout){
    source("read_CHRTOUT_Database.R")
}

if (pointSnowAnalysis){
    if (!is.null(modReadFilePath) & exists("ptgeo.sno")){
        if (file.exists(modReadFilePath)){
            load(modReadFilePath)  
            obsSnoData <- subset(obsSnoData, obsSnoData$site_id %in% ptgeo.sno$id)      
        } else {
            stop(paste("Input model read file specified but does not exist:", modReadFilePath))        
        }
    }
}

if (regionSnowAnalysis){
    if (!is.null(modReadFilePath) & exists("mskgeo.List")){
        if (file.exists(modReadFilePath)){
            load(modReadFilePath)
            basSnoData <- subset(basSnoData, basSnoData$site_id %in% mskgeo.nameList)
        } else {
            stop(paste("Input model read file specified but does not exist:", modReadFilePath))
        }
    }
}

if (hydroPlot){
    if (!is.null(modReadFilePath) & (hydroPlot | hydroEnsPlot)){
        # Regular forecast point configuration
        if ( !reachRting & exists("stid2gageList")){
            if (is.list(stid2gageList)){
                gageList <- data.frame(st_id=names(stid2gageList), site_no=unlist(stid2gageList), stringsAsFactors=FALSE)
            } else {
                gageList <- stid2gageList            
            }
        } else if (reachRting) {
            # Reach-based routing - NHD (NWM)
            if (exists("statsLink2Gage") & !is.null(statsLink2Gage)) {
                gageList <- statsLink2gage[,c("link","site_no")]                
            } else {
                gageList <- subset(rtLinks[,c("link","site_no")], !(rtLinks$site_no == ''))            
            }
        }

        # Load observation data
        load(modReadFilePath)
        if ( !is.null(gageList) ) { 
            obsStrData <- subset(obsStrData, obsStrData$site_no %in% unique(gageList$site_no))
            obsStrMeta <- subset(obsStrMeta, obsStrMeta$site_no %in% unique(gageList$site_no))
        } else {
            obsStrData <- obsStrData
            obsStrMeta <- obsStrMeta
        }
        if ( reachRting & !is.null(gageList) ) {
            obsStrData <- plyr::join(obsStrData, gageList, by="site_no")
        }
    }
}

# Read in 
# Read in basin Snow/SNODAS
if (readSnodas & readBasinSnodas) {
        # Load necessary mask data to perform analysis
        source("read_BASIN_SNOW.R")
}

# Read in point SNODAS
if (readSnodas & (readSnoSnodas | readAmfSnodas | readMetSnodas)) {
	# Load necessary mask data to perform reads
	source("read_SNODAS.R")
}

# Stats Calculations
if (calcStats & (strProc | snoProc | amfProc | metProc)) {
	message("Calculating stats")
	if (is.null(modReadFileOut)) {
		if (file.exists(modReadFileIn)) {
			load(modReadFileIn)
		}
	} else {
		if (is.null(modReadFileIn)) {
			if (file.exists(modReadFileOut)) {
				load(modReadFileOut)
			}
		} else {
			if (file.exists(modReadFileIn)) {
				load(modReadFileIn)
			}
		}
	}
	if (metProc) {
        	if (is.null(forcReadFileOut)) {
                	if (file.exists(forcReadFileIn)) {
                        	load(forcReadFileIn)
                	}
        	} else {
                	if (is.null(forcReadFileIn)) {
                        	if (file.exists(forcReadFileOut)) {
                                	load(forcReadFileOut)
                        	}
                	} else {
                        	if (file.exists(forcReadFileIn)) {
                                	load(forcReadFileIn)
                        	}
                	}
        	}
	}
	source("calc_PERFSTATS.R")
}

# Basin snow analysis 
if (calcStats & basSnoProc) {
	# Load basin snow file 
	load(snodasReadFileOut)
	# Load model output (necessary for total accumulated streamflow).
	source("calc_BASIN_SNOW.R")
}

# Plots
if (createPlots) {
	if (accflowPlot | hydroPlot | hydroEnsPlot | accprecipPlot | 
			flowswePlot | flowlsmPlot | indSwePlot | 
			strBiasMap | strCorrMap | 
			snosweErrMap | snoprecipErrMap) {
        	message("Generating plots")
		if (strBiasMap | strCorrMap | snosweErrMap | snoprecipErrMap) {
			load(statsFileOut)
		}
        	if (is.null(modReadFileOut)) {
                	if (file.exists(modReadFileIn)) {
                        	load(modReadFileIn)
                	}
        	} else {
                	if (is.null(modReadFileIn)) {
                        	if (file.exists(modReadFileOut)) {
                                	load(modReadFileOut)
                        	}
                	} else {
                        	if (file.exists(modReadFileIn)) {
                                	load(modReadFileIn)
                        	}
                	}
        	}
	}
	if (basSnoEnsPlot) {
		if (file.exists(modReadFileIn)) {
			load(modReadFileIn)
		}
	}
	if (metPlot) {
                if (is.null(forcReadFileOut)) {
                        if (file.exists(forcReadFileIn)) {
                                load(forcReadFileIn)
                        }
                } else {
                        if (is.null(forcReadFileIn)) {
                                if (file.exists(forcReadFileOut)) {
                                        load(forcReadFileOut)
                                }
                        } else {
                                if (file.exists(forcReadFileIn)) {
                                        load(forcReadFileIn)
                                }
                        }
                }
        }
	if (snowBasinPlot) {
		if (file.exists(snowBasDataFile)) {
			load(snowBasDataFile)
		}
	}
	if (snowPointScatter) {
		# Load model data in for points
		if (file.exists(modReadFileIn)){
			load(modReadFileIn)
		}

		# Load necessary files for SNOTEL analysis
		if (!is.null(SNOfile) & snotelScatter) {
			if (file.exists(SNOfile)){
				load(SNOfile)
			}
			if (file.exists(snodasSNOfile)){
				load(snodasSNOfile)
			}
		}

		# Load necessary files for Hydro-met analysis
		if (metScatter) {
			if (file.exists(METfile)) {
				load(METfile)
			}
			if (file.exists(snodasMETfile)){
				load(snodasMETfile)
			}
		}

		# Load necessary files for basin-agreggation analysis
		if (!is.null(SNOfile) & basinScatter){
			if (file.exists(SNOfile)){
				load(SNOfile)
			}
			if (file.exists(snodasSNOfile)){
				load(snodasSNOfile)
			}
		}
	}
	if (snotelAccPcpPlot) {
		if (file.exists(modReadFileIn)) {
			load(modReadFileIn)
		}
		if (file.exists(SNOfile)) {
			load(SNOfile)
		}
	}

        source("calc_PLOTS.R")
}

# EXIT
#quit("no")

