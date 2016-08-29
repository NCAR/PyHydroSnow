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

# Compose list of gages to analyze/read
if ( !reachRting & exists("stid2gageList")){
    # frxst points
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

# If subsetting has been enabled, subset mask parameters here. This will vary
# depending on whether points, regions, or gages have been identified 
# for subsetting.
if (!is.null(subSet)){
    # Subset reach-based gages
    if (length(which(unique(subSet$type) == 1)) == 1){
        listSub <- subSetReachPts(subSet,gageList)
        gageList <- listSub[[1]]
    }
    # Subset FRXST points with associated basin mskgeo/mskhyd areas
    if (length(which(unique(subSet$type) == 2)) == 1){
        listSub <- subSetBasins(mskgeo.nameList,
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
                                stid2gageList,
                                subSet)
        mskgeo.nameList <- listSub[[1]]
        frxstPts <- listSub[[2]]
        basin2gageList <- listSub[[3]]
        gage2basinList <- listSub[[4]]
        mskgeo.areaList <- listSub[[5]]
        mskgeo.countInds <- listSub[[6]]
        mskgeo.List <- listSub[[7]]
        mskgeo.maxInds <- listSub[[8]]
        mskgeo.minInds <- listSub[[9]]
        mskhyd.areaList <- listSub[[10]]
        mskhyd.countInds <- listSub[[11]]
        mskhyd.List <- listSub[[12]]
        mskhyd.maxInds <- listSub[[13]]
        mskhyd.minInds <- listSub[[14]]
        mskhyd.nameList <- listSub[[15]]
        stid2gageList <- listSub[[16]]
        # Reset gageList
        gageList <- data.frame(st_id=names(stid2gageList), site_no=unlist(stid2gageList), stringsAsFactors=FALSE)
    }
    # Subset regions only, assuming no associated streamflow gages 
    # with these (eco-regions with no basin2gageList,gage2basinList,
    # or stid2gageList)
    if (length(which(unique(subSet$type) == 3)) == 1){
        listSub <- subSetRegions(mskgeo.nameList,
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
                                 subSet)
        mskgeo.nameList <- listSub[[1]]
        mskgeo.areaList <- listSub[[2]]
        mskgeo.countInds <- listSub[[3]]
        mskgeo.List <- listSub[[4]]
        mskgeo.maxInds <- listSub[[5]]
        mskgeo.minInds <- listSub[[6]]
        mskhyd.areaList <- listSub[[7]]
        mskhyd.countInds <- listSub[[8]]
        mskhyd.List <- listSub[[9]]
        mskhyd.maxInds <- listSub[[10]]
        mskhyd.minInds <- listSub[[11]]
        mskhyd.nameList <- listSub[[12]]
    }
    # Subset snow points.
    if (length(which(unique(subSet$type) == 4)) == 1){
        listSub <- subSetPoints(ptgeo.sno,subSet)
        ptgeo.sno <- listSub[[1]]
    }
}
 
# Break geospatial meta-data to the different processors based on the
# MPI size/rank information passed in. If ran on single processor, do 
# not call function to break up basins/regions/points. 
if (size > 1){
    # Split up gageList for reach-based routing
    if (!is.null(gageList) & is.null(frxstPts)){
        listMpi <- mpiGageList(size,rank,gageList)    
        gageList <- listMpi[[1]]
    }
    # Split up forecast points and associated basin masks.
    if (!is.null(frxstPts)){
        listMpi <- mpiFrxst(size,rank,frxstPts,basin2gageList,gage2basinList,
                            stid2gageList,mskgeo.areaList,mskgeo.countInds,
                            mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                            mskhyd.areaList,mskhyd.countInds,mskhyd.List,
                            mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList,
                            mskgeo.nameList,gageList)    
        frxstPts <- listMpi[[1]]
        basin2gageList <- listMpi[[2]]
        gage2basinList <- listMpi[[3]]
        stid2gageList <- listMpi[[4]]
        mskgeo.areaList <- listMpi[[5]]
        mskgeo.countInds <- listMpi[[6]]
        mskgeo.List <- listMpi[[7]]
        mskgeo.maxInds <- listMpi[[8]]
        mskgeo.minInds <- listMpi[[9]]
        mskhyd.areaList <- listMpi[[10]]
        mskhyd.countInds <- listMpi[[11]]
        mskhyd.List <- listMpi[[12]]
        mskhyd.maxInds <- listMpi[[13]]
        mskhyd.minInds <- listMpi[[14]]
        mskhyd.nameList <- listMpi[[15]]
        mskgeo.nameList <- listMpi[[16]]  
        gageList <- listMpi[[17]]
    }
    # Split up regions
    if (!is.null(mskgeo.List) & is.null(frxstPts)){
        listMpi <- mpiRegions(size,rank,mskgeo.areaList,mskgeo.countInds,
                              mskgeo.List,mskgeo.maxInds,mskgeo.minInds,
                              mskhyd.areaList,mskhyd.countInds,mskhyd.List,
                              mskhyd.maxInds,mskhyd.minInds,mskhyd.nameList,
                              mskgeo.nameList)
    }
    mskgeo.areaList <- listMpi[[1]]
    mskgeo.countInds <- listMpi[[2]]
    mskgeo.List <- listMpi[[3]]
    mskgeo.maxInds <- listMpi[[4]]
    mskgeo.minInds <- listMpi[[5]]
    mskhyd.areaList <- listMpi[[6]]
    mskhyd.countInds <- listMpi[[7]]
    mskhyd.List <- listMpi[[8]]
    mskhyd.maxInds <- listMpi[[9]]
    mskhyd.minInds <- listMpi[[10]]
    mskhyd.nameList <- listMpi[[11]]
    mskgeo.nameList <- listMpi[[12]]

    # Split up points
    if (!is.null(ptgeo.sno)){
        listMpi <- mpiPts(size,rank,ptgeo.sno)
        ptgeo.sno <- listMpi[[1]]
    }
}

print(frxstPts)
stop('YAY')
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

