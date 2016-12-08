# Program to calculate basin-aggregated statistics and plots.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory.

# Load necessary libraries
library(ggplot2)

# Process command line arguments.
args <- commandArgs(trailingOnly = TRUE)
sourceFile <- args[1]

# Source temporary R file. This will load up options necessary
# to run analysis.
source(sourceFile)

# Load input file containing daily snow obs, model values and
# possibly SNODAS values.
load(inFile)

# Establish time information
dUnits <- "days"
diff <- difftime(dateEnd,dateStart,units=dUnits)
nSteps <- diff <- as.numeric(diff)
dt <- 24*3600

bDStr <- strftime(dateStart,'%Y%m%d',tz='UTC')
eDStr <- strftime(dateEnd,'%Y%m%d',tz='UTC')

numBasins <- unique(snowBasinData$Basin)
basins <- unique(snowBasinData$Basin)
numTags <- unique(snowBasinData$product)
tags <- unique(snowBasinData$product)
# Loop through each basin available and calculate plots.
for(basin in 1:numBasins){
   basinTmp <- basins[basin]
   outPath1 <- paste0(jobDir,'/SWE_VOLUME_',basinTmp,'_',bDStr,'_',eDStr,'.png')
   dfTmp <- subset(snowBasinData,Basin == basinTmp)
   gg <- ggplot(dfTmp,aes(x=Date,y=snow_volume_acre_feet,color=product)) +
         geom_line() + 
         xlab('Date') + 
         ylab('SWE Volume (acre-feet)
   ggsave(filename=outPath1,plot=gg,units="in", width=8, height=6, dpi=100)
}
