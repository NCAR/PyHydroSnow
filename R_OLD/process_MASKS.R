###################################################
##               Generate masks                  ##
###################################################

##################### Setup #######################

## Specify the low-resolution geogrid file
geoFile <- '/glade/p/ral/RHAP/adugger/FRNG_MASTER_15Min_LONG_16Cores/DOMAIN/AD_KS_151117/geo_em.d02_151117.nc'

## Select which masks/points to create:

# Basin masks:
createBasMask <- TRUE
    # Specify the mask file
    maskFile <- '/glade/p/ral/RHAP/adugger/FRNG_MASTER_15Min_LONG_16Cores/DOMAIN/AD_KS_151117/Fulldom_hires_netcdf_file_151117.nc'
    # If relevant, specify variable name
    maskVar <- "basn_msk"
    # Specify the aggregation factor between the input mask grid and geogrid
    aggfact <- 10
    # Reverse the y direction from N->S to S->N?
    ns2sn <- TRUE

# Point masks:
    # Ameriflux points
    createAmfMask <- TRUE
    # SNOTEL points
    createSnoMask <- TRUE
    # MET station points
    createMetMask <- FALSE
    # MET station sites (must include columns: id, lat, lon)
    metSites <- read.table("/glade/p/ral/RHAP/adugger/Upper_RioGrande/OBS/MET/met.URG.sites4msk.txt", header=TRUE, sep="\t")

# Streamflow gages:
    # Specify the FRXST points lookup table
    # (must include columns: id, lat, lon, gageid, source, basnids)
    # Example: id         lon       lat    gageid  source    basnids 
    #           9  -106.33292  37.37602  ALATERCO   CODWR    9,10,12
    #          49  -106.56661  38.86029  09107000    USGS         49
    frxstPts <- "/glade/p/ral/RHAP/adugger/FRNG_MASTER_15Min_LONG_16Cores/DOMAIN/AD_KS_151117/frxstptsMeta.Rdata"

# Link to gage:
createRouteLink <- TRUE
	# If running reach-based routing, specify routelink file:
	rtLinkFile <- "/glade/p/ral/RHAP/adugger/FRNG_MASTER_15Min_LONG_16Cores/DOMAIN/Route_Link_2.nc"	

# Specify the .Rdata file to create
maskFileOut <- '/glade/p/ral/RHAP/adugger/FRNG_MASTER_15Min_LONG_16Cores/DOMAIN/AD_KS_151117/frn_4basns_MASKS.Rdata'

###################################################################################################
## Run 

library(rwrfhydro)
saveList <- c()

if (is.null(maskVar)) maskVar <- "basn_msk"
if (is.null(aggfact)) aggfact <- 1
gage2basinList <- NULL

if (file.exists(frxstPts)) load(frxstPts)

## Create gage lookup tables
if (!is.null(frxstPts)) {
	frxstPts <- subset(frxstPts, frxstPts$gageid!="")
	stid2gageList <- as.list(frxstPts$gageid)
	names(stid2gageList) <- frxstPts$id
	stid2sourceList <- as.list(frxstPts$source)
        names(stid2sourceList) <- frxstPts$id
	frxstPts$basnids <- strsplit(frxstPts$basnids, split=",")
        gage2basinList <- as.list(frxstPts$basnids)
        names(gage2basinList) <- frxstPts$gageid
	saveList <- c(saveList, "frxstPts", "stid2gageList", "gage2basinList")
}

## Create masks for point obs
if (createAmfMask) {
    ptgeo.amf <- GetGeogridIndex(amfMeta, geoFile, id="id_txt")
    ptgeo.amf <- subset(ptgeo.amf, !is.na(ptgeo.amf$sn))
    saveList <- c(saveList, "ptgeo.amf")
}
if (createSnoMask) {
    ptgeo.sno <- GetGeogridIndex(snotelMeta, geoFile, id="site_id")
    ptgeo.sno <- subset(ptgeo.sno, !is.na(ptgeo.sno$sn))
        saveList <- c(saveList, "ptgeo.sno")
}
if (createMetMask) {
    ptgeo.met <- GetGeogridIndex(metSites, geoFile, id="id")
    ptgeo.met <- subset(ptgeo.met, !is.na(ptgeo.met$sn))
        saveList <- c(saveList, "ptgeo.met")
}

## Create HYDROGRID basin mask objects

if (createBasMask) {
    # Initialize - GEO
    mskgeo.List <- list()
    mskgeo.nameList <- list()
    mskgeo.areaList <- list()
    mskgeo.minInds <- data.frame(x=integer(0), y=integer(0))
    mskgeo.maxInds <- data.frame(x=integer(0), y=integer(0))
    mskgeo.countInds <- data.frame(x=integer(0), y=integer(0))
    if (aggfact > 1) {
        # Initialize - HYDRO
        mskhyd.List <- list()
        mskhyd.nameList <- list()
        mskhyd.areaList <- list()
        mskhyd.minInds <- data.frame(x=integer(0), y=integer(0))
        mskhyd.maxInds <- data.frame(x=integer(0), y=integer(0))
        mskhyd.countInds <- data.frame(x=integer(0), y=integer(0))
    }
    # Open basn_msk variable

    ncid <- tryCatch(suppressWarnings(ncdf4::nc_open(maskFile)),  
        error=function(cond) {message(cond); return(c())})
    if (length(ncid)>0) {
        mskvarAll <- ncdf4::ncvar_get(ncid, maskVar)
        ncdf4::nc_close(ncid)
    } else {
        stop("Input mask must be NetCDF format.")
    }
    mskvarAll[which(is.na(mskvarAll))] <- -9999
    if (ns2sn) {
        # Reverse y-direction for N->S hydro grids to S->N
        mskvarAll <- mskvarAll[,order(ncol(mskvarAll):1)]
    }
    # Get unique ID list
    if (is.null(gage2basinList)) {
        mskidList <- na.exclude(unique(c(mskvarAll)))
        mskidList <- subset(mskidList, mskidList>=0)
        mskidList <- mskidList[order(mskidList)]
        gage2basinList <- as.list(mskidList)
        names(gage2basinList) <- mskidList
    }
    # Reverse the list
    basin2gageList <- list()
    for (i in unique(unlist(gage2basinList))) {
        idlist <- c()
        for (j in names(gage2basinList)) {
            if (i %in% gage2basinList[[j]]) idlist <- c(idlist, j)
        }
        basin2gageList[[paste0(i)]] <- idlist
	saveList <- c(saveList, "basin2gageList")
    }
    # Loop through basin masks
    if (length(gage2basinList) > 0) {
        for (i in 1:length(gage2basinList)) {
            print(paste0("Basin: ", names(gage2basinList)[i]))
            # Subset to basinID
            mskvar <- mskvarAll
            mskvar[which(!(mskvar %in% gage2basinList[[i]]))] <- 0.0
            mskvar[which(mskvar %in% gage2basinList[[i]])] <- 1.0
            ##Reverse y-direction for N->S hydro grids to S->N
            ##mskvar <- mskvar[,order(ncol(mskvar):1)]
            # Resample the high-res grid to the low-res LSM
            if (aggfact > 1) {
                # Calculate cell indices
                mskhyd.minInds[i,1:2] <- apply(which(mskvar==1, arr.ind=TRUE), MARGIN=2, FUN=min)
                mskhyd.maxInds[i,1:2] <- apply(which(mskvar==1, arr.ind=TRUE), MARGIN=2, FUN=max)
                mskhyd.countInds[i,1:2] <- mskhyd.maxInds[i,1:2] - mskhyd.minInds[i,1:2] + 1
                # Calculate basin area as a cell count
                basarea <- sum(mskvar)
                mskhyd.List[[i]] <- mskvar[mskhyd.minInds[i,1]:mskhyd.maxInds[i,1], mskhyd.minInds[i,2]:mskhyd.maxInds[i,2]]
                mskhyd.nameList[[i]] <- names(gage2basinList)[i]
                mskhyd.areaList[[i]] <- basarea
                # Resample the high-res grid to the low-res LSM
                mskvar <- raster::as.matrix(raster::aggregate(raster::raster(mskvar), fact=aggfact, fun=mean))
            }
            # Calculate cell indices
            mskgeo.minInds[i,1:2] <- apply(which(mskvar>0, arr.ind=TRUE), MARGIN=2, FUN=min)
            mskgeo.maxInds[i,1:2] <- apply(which(mskvar>0, arr.ind=TRUE), MARGIN=2, FUN=max)
            mskgeo.countInds[i,1:2] <- mskgeo.maxInds[i,1:2] - mskgeo.minInds[i,1:2] + 1
            # Calculate basin area as a cell count
            basarea <- sum(mskvar)
            mskgeo.List[[i]] <- mskvar[mskgeo.minInds[i,1]:mskgeo.maxInds[i,1], mskgeo.minInds[i,2]:mskgeo.maxInds[i,2]]
            mskgeo.nameList[[i]] <- names(gage2basinList)[i]
            mskgeo.areaList[[i]] <- basarea
        } 
        # Add names/IDs
        # HYDRO
        if (aggfact > 1) {
            names(mskhyd.List) <- unlist(mskhyd.nameList)
            names(mskhyd.areaList) <- unlist(mskhyd.nameList)
            mskhyd.minInds$id <- unlist(mskhyd.nameList)
            mskhyd.maxInds$id <- unlist(mskhyd.nameList)
            mskhyd.countInds$id <- unlist(mskhyd.nameList)
            saveList <- c(saveList, "mskhyd.nameList", "mskhyd.areaList", "mskhyd.List",
                            "mskhyd.minInds", "mskhyd.maxInds", "mskhyd.countInds")
        }
        # GEO
        names(mskgeo.List) <- unlist(mskgeo.nameList)
        names(mskgeo.areaList) <- unlist(mskgeo.nameList)
        mskgeo.minInds$id <- unlist(mskgeo.nameList)
        mskgeo.maxInds$id <- unlist(mskgeo.nameList)
        mskgeo.countInds$id <- unlist(mskgeo.nameList)
        # Add to save object list
        saveList <- c(saveList, "mskgeo.nameList", "mskgeo.areaList", "mskgeo.List",
                            "mskgeo.minInds", "mskgeo.maxInds", "mskgeo.countInds")
        } else {
            warning("No basins specified in the high-res domain file.")
        }
}

if (createRouteLink) {
	rtLinks <- GetNcdfFile(rtLinkFile, variables=c("time"), exclude=TRUE, quiet=TRUE)
	#rtLinks <- subset(rtLinks, !(stringr::str_trim(rtLinks$gages)==""))
	rtLinks$gages <- stringr::str_trim(rtLinks$gages)
	saveList <- c(saveList, "rtLinks")
}

# Save all relevant objects
save(list=saveList, file=maskFileOut)

quit(save="no")
