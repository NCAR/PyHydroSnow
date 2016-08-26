###################################################
##  Main Control Script to process model output  ##
###################################################

################## General Setup ##################

## Specify the high-resolution routing domain file
hydFile <- NULL 

geoFile <- NULL 

## Specify the aggregation factor between hydrogrid and geogrid
aggfact <- NULL 

## Specify location of .Rdata file containing pre-processed mask objects
maskFile <- NULL 

## Specify whether the model run used NHD reach-based routing (otherwise gridded routing assumed)
# If TRUE, mask file should also contain rtLinks dataframe.
reachRting <- FALSE 
routeLinkFile <- NULL
statsLink2GageFile <- NULL

## Specify text file to subset points/regions/basins for analysis/plotting.
subSet <- NULL

## Temp directory to write intermediate files
tmpDir <- NULL 

# Specify MPI related variables passed in from Python
size <- NULL
rank <- NULL

################## Observations ###################

## Path to streamflow observations database
streamDB <- NULL

## Path to snow observations database
snowDB <- NULL
 
################ Model Output Reads ###############

# Select what snow aggregations to read:
readBasinSnow <- FALSE
readPointSnow <- FALSE

# Select what streamflow config to read:
readStreamFrxst <- FALSE
readStreamChrtout <- FALSE

# Specific to CHRTOUT reads
readChrtOut_GAGES <- FALSE
#readLink2gage <- NULL

## If TRUE, specify the following to read in model output:
# Specify the model run output directory or directories
modPathList <- NULL 

# Specify tags to identify the model run or runs (should be 1:1 with number of model output directories)
modTagList <- NULL

# Specify ensemble information
readEnsemble <- FALSE

ensembleList <- NULL

ensembleTagList <- NULL

# Specify modeling resolution in km. This is used in aggregation 
# functions.
resMod <- NULL

# Specify the output .Rdata file to create OR to be read in
modReadFilePathSnow <- NULL
modReadFilePathStream <- NULL
 
# Specify start and end dates for reading data:
readModStart <- NULL 
readModEnd <- NULL 

#################### SNODAS ########################

## Read SNODAS SWE data?
readSnodas <- FALSE

# Specify path to the regridded SNODAS NetCDF data
snodasPathList <- NULL

# Specify start and end dates for reading data:
readSnodasStart <- NULL 
readSnodasEnd <- NULL 

################## Analysis Options #################

# Determine which scale to run analysis on:
pointSnowAnalysis <- FALSE
regionSnowAnalysis <- FALSE

# Plot flag
plotFlag <- FALSE

# Flag for running plotting/statistics to include SNODAS:
snodasFlag <- FALSE

# Specify beginning/ending dates for analysis
analysisStartDate <- NULL
analysisEndDate <- NULL

# Specify output for stats/plotting
analysisOutPath <- NULL
plotDir <- NULL

## Specify padding for observations. This is used 
## for plotting where there's a desire to padd the
## the beginning of the plot with observed values.
padSteps <- 0

## Generate hydrographs?
hydroPlot <- FALSE

## Generate ensemble hydrographs?
hydroEnsPlot <- FALSE

# Specify if to apply bias correction
hydroBiasCorr <- 0

# Specify if to apply baseflow correction
hydroEnsBaseFlowCorr <- 0 

###########################################################################################
## RUN (do not change anything below this line)

source("run_CONFIG.R")
