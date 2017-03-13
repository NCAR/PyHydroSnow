# Program to open up gridded model and SNODAS fields, create 
# difference grids and bias grids. These will be stored
# in NetCDF format and GeoTiff.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

# Load necessary libraries
library(rwrfhydro)

# Process command line arguments.
args <- commandArgs(trailingOnly = TRUE)
sourceFile <- args[1]

# Source temporary R file. This will load up options necessary
# to run analysis.
source(sourceFile)

# Establish time information
dUnits <- "days"
diff <- difftime(dateEnd,dateStart,units=dUnits)
nSteps <- diff <- as.numeric(diff)
dt <- 24*3600

bDStr <- strftime(dateStart,'%Y%m%d',tz='UTC')
eDStr <- strftime(dateEnd,'%Y%m%d',tz='UTC')

# Loop through each day, read in gridded SWE from the models,
# and SNODAS. Calculate grids of differences, etc. Output 
# to NetCDF format, and also GeoTIFF.
for (day in 0:nSteps){
   for (tag in 1:length(modTags)){
      modTag <- modTags[tag]
      tmpPath <- modPaths[[tag]]
      snowPath <- paste0(modPaths[[tag]],"/",strftime(dCurrent,"%Y%m%d"),
                         "0000.LDASOUT_DOMAIN1")
      snodasFilePath <- paste0(snodasPath,"/SNODAS_REGRIDDED_",
                               strftime(dCurrent,"%Y%m%d"),".nc")
      outPathNC <- paste0(jobDir,'/SPATIAL_SWE_ANALYSIS_',modTag,'_',
                          strftime(dCurrent,"%Y%m%d"),".nc")
      outPathTif1 <- paste0(jobDir,'/SPATIAL_SWE_ANALYSIS_',modTag,'_',
                            strftime(dCurrent,"%Y%m%d"),"_MODEL.tif")
      outPathTif2 <- paste0(jobDir,'/SPATIAL_SWE_ANALYSIS_',modTag,'_',
                            strftime(dCurrent,"%Y%m%d"),"_SNODAS.tif")
      outPathTif3 <- paste0(jobDir,'/SPATIAL_SWE_ANALYSIS_',modTag,'_',
                            strftime(dCurrent,"%Y%m%d"),"_DIFF.tif")

      # Open up files (if they exist)
      if(file.exists(snowPath)){
         if(file.exists(snodasFilePath)){
            id <- nc_open(snowPath)
            sweModel <- ncvar_get(id,'SNEQV')
            nc_close(id)

            id <- nc_open(snodasFilePath)
            sweSnodas <- ncvar_get(id,'SNEQV')
            nc_close(id)

            # Calculate index values for where missing/valid data are at.
            indNDV <- which(sweModel<0.0 | is.na(sweModel) | sweSnodas < 0.0 | is.na(sweSnodas))
            indValid <- which(sweModel>=0.0 & sweSnodas>=0.0 & !is.na(sweModel) & !is.na(sweSnodas))

            sweMode[indNDV] <- -9999.0
            sweSnodas[indNDV] <- -9999.0

            sweDiff <- sweModel - sweSnodas
            sweDiff[indNDV] <- -9999.0

            nxOut <- dim(sweModel)[1]
            nyOut <- dim(sweModel)[2]
            
            # Create output NetCDF
            varList <- list()
            varList[[1]] <- list(name='SWE_MODEL',
                            longname='Modeled SWE',
                            units='mm',precision='float',
                            missing=-9999.0,
                            dimensionList=list(x=list(name='west_east',values=1:nxOut,
                                               units='Degrees East',
                                               unlimited=FALSE,
                                               create_dimvar=FALSE),
                            y=list(name='south_north',values=1:nyOut,
                                   units='Degrees East',
                                   unlimited=FALSE,
                                   create_dimvar=FALSE)),
                            data=sweModel)
	    varList[[2]] <- list(name='SWE_SNODAS',
                            longname='SNODAS SWE',
                            units='mm',precision='float',
                            missing=-9999.0,
                            dimensionList=list(x=list(name='west_east',values=1:nxOut,
                                               units='Degrees East',
                                               unlimited=FALSE,
                                               create_dimvar=FALSE),
                            y=list(name='south_north',values=1:nyOut,
                                   units='Degrees East',
                                   unlimited=FALSE,
                                   create_dimvar=FALSE)),
                            data=sweSnodas)
            varList[[3]] <- list(name='SWE_DIFF',
                            longname='Modeled SWE Minus SNODAS',
                            units='mm',precision='float',
                            missing=-9999.0,
                            dimensionList=list(x=list(name='west_east',values=1:nxOut,
                                               units='Degrees East',
                                               unlimited=FALSE,
                                               create_dimvar=FALSE),
                            y=list(name='south_north',values=1:nyOut,
                                   units='Degrees East',
                                   unlimited=FALSE,
                                   create_dimvar=FALSE)),
                            data=sweDiff)

            globalAttList <- list()
            globalAttList[[1]] <- list(name='Institution',value='NCAR-RAL',precision="text")

            MkNcdf(varList,filename=outPathNC,globalAttList=globalAttList)

            # Make TIF files 
            ExportGeogrid(outPathNC,'SWE_MODEL',outPathTif1,inCoordFile=geoFile)
            ExportGeogrid(outPathNC,'SWE_SNODAS',outPathTif2,inCoordFile=geoFile)
            ExportGeogrid(outPathNC,'SWE_DIFF',outPathTif3,inCoordFile=geoFile)
         }
      }
   }
}
