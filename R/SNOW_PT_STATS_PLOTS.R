# Program to read in point model/analysis/obs values, and 
# return a set of statistics for every time step.

# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory

# Load necessary libraries
library(data.table)
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

# SWE stats
# Calculate unique number of "tags". Possible combinations may include
# SNODAS.
tags <- unique(sweOutPts$tag)
print(tags)
tags <- tags[which(tags != "Obs")]
print(tags)
numTags <- length(tags)

# Calculate number of unique reporting stations.
stns <- unique(sweOutPts$uniqueId)

# Create output data frame containing all stats for each date.
sweStats <- data.frame(matrix(NA,ncol=5,nrow=length(stns)*numTags*nSteps))
names(sweStats) <- c("uniqueId","POSIXct","tag","bias","diff")

sweStats$POSIXct <- as.Date(as.POSIXct('1900-01-01'),tz='UTC')

count <- 1
# Loop through each time step and calculate statistics
for (day in 0:nSteps){
   dCurrent <- dateStart + dt*day
   print(dCurrent)
   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')

   # Loop through each tag and calculate stats.
   for (tag in 1:numTags){
      modTag <- tags[tag]
      obsTmp <- sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == 'Obs']
      modTmp <- sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == modTag]
      idsObs <- unique(obsTmp$uniqueId)
      idsMod <- unique(modTmp$uniqueId)
      # Subset only where we have both obs and model values
      modTmp <- modTmp[uniqueId == idsObs]
      lenMod <- length(modTmp$uniqueId)
      if(lenMod > 0 & length(obsTmp$uniqueId) > 0){
         bInd <- count
         eInd <- count + lenMod
         sweStats$POSIXct[bInd:eInd] <- dCurrent
         sweStats$uniqueId[bInd:eInd] <- idsObs
         sweStats$tag[bInd:eInd] <- modTag
         sweStats$bias[bInd:eInd] <- (modTmp$value_mm - obsTmp$value_mm)/obsTmp$value_mm * 100.0
         sweStats$diff[bInd:eInd] <- modTmp$value_mm - obsTmp$value_mm
         count <- count + lenMod
      }
   }
}

# Remove any unecssary NA values.
sweStats <- subset(sweStats,!is.na(bias))

# Convert output to data table.
sweStats <- as.data.table(sweStats)

# Save output
outFile <- paste0(jobDir,'/SN_PT_STATS_',strftime(dateStart,'%Y%m%d',tz='UTC'),
                  '_',strftime(dateEnd,'%Y%m%d',tz='UTC'),'Rdata')
save(sweStats,file=outFile)

# Loop through each unique ID and generate timeseries plots 
for (station in 1:length(stns)){
   tablePlot <- sweOutPts[uniqueId == stns[station]]

   title <- paste0('SWE Timseries For: ',stns[station])
   gg <- ggplot(tablePlot,aes(x=POSIXct,y=value_mm,color=tag)) + geom_line() + 
         ggtitle(title) + xlab('Date') + ylab('SWE (mm)') 
   outPath <- paste0(jobDir,'/SWE_',stns[station],'_',strftime(dateStart,'%Y%m%d',tz='UTC'),
                     ,'_',strftime(dateEnd,'%Y%m%d',tz='UTC'),'.png')
   ggsave(filename=fileOutPath,plot=gg)
}
