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

# TEMPORARY
tags <- tags[which(tags != "SNODAS")]
print(tags)
numTags <- length(tags)
# TEMPORARY

# Calculate number of unique reporting stations.
stns <- unique(sweOutPts$uniqueId)

# Create output data frame containing all stats for each date.
sweStats <- data.frame(matrix(NA,ncol=5,nrow=length(stns)*numTags*nSteps))
names(sweStats) <- c("uniqueId","POSIXct","tag","bias","diff")

sweStats$POSIXct <- as.Date(as.POSIXct('1900-01-01'),tz='UTC')

count <- 1
# Loop through each time step and calculate statistics
#for (day in 0:nSteps){
#   dCurrent <- dateStart + dt*day
#   print(dCurrent)
#   dStr1 <- strftime(dCurrent,'%Y-%m-%d',tz='UTC')
#
#   # Loop through each tag and calculate stats.
#   for (tag in 1:numTags){
#      modTag <- tags[tag]
#      obsTmp <- sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == 'Obs']
#      modTmp <- sweOutPts[strftime(POSIXct,'%Y-%m-%d',tz='UTC') == dStr1 & tag == modTag]
#      idsObs <- unique(obsTmp$uniqueId)
#      idsMod <- unique(modTmp$uniqueId)
#      print(length(idsObs))
#      print(length(idsMod))
#      # Subset only where we have both obs and model values
#      #modTmp <- modTmp[uniqueId == idsObs]
#      lenMod <- length(modTmp$uniqueId)
#      if(lenMod > 0 & length(obsTmp$uniqueId) > 0){
#         bInd <- count
#         eInd <- count + lenMod
#         sweStats$POSIXct[bInd:eInd] <- dCurrent
#         sweStats$uniqueId[bInd:eInd] <- idsObs
#         sweStats$tag[bInd:eInd] <- modTag
#         sweStats$bias[bInd:eInd] <- (modTmp$value_mm - obsTmp$value_mm)/obsTmp$value_mm * 100.0
#         sweStats$diff[bInd:eInd] <- modTmp$value_mm - obsTmp$value_mm
#         count <- count + lenMod
#      }
#   }
#}

# Remove any unecssary NA values.
#sweStats <- subset(sweStats,!is.na(bias))

# Convert output to data table.
#sweStats <- as.data.table(sweStats)

# Save output
#outFile <- paste0(jobDir,'/SN_PT_STATS_',strftime(dateStart,'%Y%m%d',tz='UTC'),
#                  '_',strftime(dateEnd,'%Y%m%d',tz='UTC'),'Rdata')
#save(sweStats,file=outFile)

# Loop through each unique ID and generate timeseries plots 
#for (station in 1:length(stns)){
#   tablePlot <- sweOutPts[uniqueId == stns[station]]
#
#   title <- paste0('SWE Timseries For: ',stns[station])
#   gg <- ggplot(tablePlot,aes(x=POSIXct,y=value_mm,color=tag)) + geom_line() + 
#         ggtitle(title) + xlab('Date') + ylab('SWE (mm)') 
#   outPath <- paste0(jobDir,'/SWE_',stns[station],'_',strftime(dateStart,'%Y%m%d',tz='UTC'),
#                     '_',strftime(dateEnd,'%Y%m%d',tz='UTC'),'.png')
#   ggsave(filename=outPath,plot=gg)
#}

# Create scatter plots of observed SWE vs. Modeled SWE values for by basin/region, if they exist.
regions <- unique(sweOutPts$region)
if(length(regions[!is.na(regions)]) != 0){
   # Regions exist in dataset, perform scatter plotting.
   for(r in 1:length(regions)){
      regionTmp <- regions[r]
      for(t in 1:numTags){
         tagTmp <- tags[t]
         dtTmp <- subset(sweOutPts,region == regionTmp)
         dtTmp2 <- subset(dtTmp,tag == tagTmp)
         snodasTmp <- subset(dtTmp, tag == 'SNODAS')
         obsTmp <- subset(dtTmp,tag == 'Obs')
         print('-------------')
         print(dtTmp2)
         print('-------------')
         print(obsTmp)
         dtTmp2 <- as.data.frame(dtTmp2)
         dtTmp2[['Obs']] <- obsTmp$value_mm
         outFile <- paste0(jobDir,'/SWE_SCATTER_REGION_',regionTmp,'_',tagTmp,'_',
                           strftime(dateStart,'%Y%m%d',tz='UTC'),'_',
                           strftime(dateEnd,'%Y%m%d',tz='UTC'),'.png')

         title <- paste0(tagTmp,' In-Situ SWE Observations for: ',strftime(dateStart,'%Y-%m-%d',tz='UTC'),
                         ' to: ',strftime(dateEnd,'%Y-%m-%d',tz='UTC'))
         xLab <- 'Observed SWE (mm)'
         yLab <- 'Simulated SWE (mm)'

         if(length(dtTmp2$value_mm) == 0){
            next
         }
         lmOut <- lm(value_mm ~ Obs,dtTmp2)
         slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
         icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
         cc <- format(round(cor(dtTmp2$value_mm,dtTmp2$Obs),3),nsmall=2)

	 print('alksdjf')
         maxCheck1 <- max(dtTmp2$value_mm)
         maxCheck2 <- max(dtTmp2$Obs)
         if(maxCheck1 > maxCheck2){
            maxSnow <- maxCheck1
         }else{
            maxSnow <- maxCheck2
         }

         gg <- ggplot2::ggplot(dtTmp2,ggplot2::aes(x=Obs,y=value_mm)) +
         ggplot2::geom_point(alpha = 0.2) +
         ggplot2::ggtitle(title) +
         ggplot2::xlab(xLab) +
         ggplot2::ylab(yLab) +
         theme(plot.title = element_text(size=16)) +
         theme(axis.title.x = element_text(size=20)) +
         theme(axis.title.y = element_text(size=20)) +
         ggplot2::geom_abline(intercept = 0, slope = 1) +
         coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
         annotate("text", x= (0.2*maxSnow), y = (0.95*maxSnow), label = paste0("Slope: ",toString(slope)),
                  colour="darkred", family="serif", fontface="italic", size = 7) +
         annotate("text", x= (0.2*maxSnow), y = (0.9*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                  colour="darkred", family="serif", fontface="italic", size = 7) +
         annotate("text", x= (0.2*maxSnow), y = (0.85*maxSnow), label = paste0("Correlation: ",toString(cc)),
                  colour="darkred", family="serif", fontface="italic", size = 7)
         ggplot2::ggsave(filename=outFile,plot=gg, units="in", width=8, height=6, dpi=100)

	 print('TESTT')
	 # Scatter plots of SNODAS against Gamma
         dtTmp2 <- as.data.frame(snodasTmp)
         dtTmp2[['Obs']] <- obsTmp$value_mm
         outFile <- paste0(jobDir,'/SWE_SCATTER_REGION_',regionTmp,'_SNODAS_',
                           strftime(dateStart,'%Y%m%d',tz='UTC'),'_',
                           strftime(dateEnd,'%Y%m%d',tz='UTC'),'.png')

         title <- paste0(tagTmp,' In-Situ SWE Observations for: ',strftime(dateStart,'%Y-%m-%d',tz='UTC'),
                         ' to: ',strftime(dateEnd,'%Y-%m-%d',tz='UTC'))
         xLab <- 'Observed SWE (mm)'
         yLab <- 'SNODAS SWE (mm)'

         if(length(dtTmp2$value_mm) == 0){
            next
         }
         lmOut <- lm(value_mm ~ Obs,dtTmp2)
         slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
         icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
         cc <- format(round(cor(dtTmp2$value_mm,dtTmp2$Obs),3),nsmall=2)

         maxCheck1 <- max(dtTmp2$value_mm)
         maxCheck2 <- max(dtTmp2$Obs)
         if(maxCheck1 > maxCheck2){
            maxSnow <- maxCheck1
         }else{
            maxSnow <- maxCheck2
         }

         gg <- ggplot2::ggplot(dtTmp2,ggplot2::aes(x=Obs,y=value_mm)) +
         ggplot2::geom_point(alpha = 0.2) +
        ggplot2::ggtitle(title) +
         ggplot2::xlab(xLab) +
         ggplot2::ylab(yLab) +
         theme(plot.title = element_text(size=16)) +
         theme(axis.title.x = element_text(size=20)) +
         theme(axis.title.y = element_text(size=20)) +
         ggplot2::geom_abline(intercept = 0, slope = 1) +
         coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
         annotate("text", x= (0.2*maxSnow), y = (0.95*maxSnow), label = paste0("Slope: ",toString(slope)),
                  colour="darkred", family="serif", fontface="italic", size = 7) +
         annotate("text", x= (0.2*maxSnow), y = (0.9*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                  colour="darkred", family="serif", fontface="italic", size = 7) +
         annotate("text", x= (0.2*maxSnow), y = (0.85*maxSnow), label = paste0("Correlation: ",toString(cc)),
                  colour="darkred", family="serif", fontface="italic", size = 7)
         ggplot2::ggsave(filename=outFile,plot=gg, units="in", width=8, height=6, dpi=100)
      }
   }
}

# Create scatter plot of observed SWE vs. Modeled SWE values for everywhere.
for(t in 1:numTags){
	tagTmp <- tags[t]
	dtTmp <- subset(sweOutPts,tag == tagTmp)
	snodasTmp <- subset(sweOutPts, tag == 'SNODAS')
	obsTmp <- subset(sweOutPts,tag == 'Obs')
	dtTmp2 <- as.data.frame(dtTmp)
	dtTmp2[['Obs']] <- obsTmp$value_mm
	outFile <- paste0(jobDir,'/SWE_SCATTER_ALL_',tagTmp,'_',
                           strftime(dateStart,'%Y%m%d',tz='UTC'),'_',
                           strftime(dateEnd,'%Y%m%d',tz='UTC'),'.png')

        title <- paste0(tagTmp,' In-Situ SWE Observations for: ',strftime(dateStart,'%Y-%m-%d',tz='UTC'),
                        ' to: ',strftime(dateEnd,'%Y-%m-%d',tz='UTC'))
        xLab <- 'Observed SWE (mm)'
        yLab <- 'Simulated SWE (mm)'

        if(length(dtTmp2$value_mm) == 0){
           next
        }
        lmOut <- lm(value_mm ~ Obs,dtTmp2)
        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
        cc <- format(round(cor(dtTmp2$value_mm,dtTmp2$Obs),3),nsmall=2)

	print(cc)
	testData <- cor.test(as.vector(dtTmp2$value_mm),as.vector(dtTmp2$Obs),method='pearson')
	print(testData)
        maxCheck1 <- max(dtTmp2$value_mm)
        maxCheck2 <- max(dtTmp2$Obs)
        if(maxCheck1 > maxCheck2){
           maxSnow <- maxCheck1
        }else{
           maxSnow <- maxCheck2
        }

        gg <- ggplot2::ggplot(dtTmp2,ggplot2::aes(x=Obs,y=value_mm)) +
        ggplot2::geom_point(alpha = 0.2) +
        ggplot2::ggtitle(title) +
        ggplot2::xlab(xLab) +
        ggplot2::ylab(yLab) +
        theme(plot.title = element_text(size=16)) +
        theme(axis.title.x = element_text(size=20)) +
        theme(axis.title.y = element_text(size=20)) +
        ggplot2::geom_abline(intercept = 0, slope = 1) +
        coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
        annotate("text", x= (0.2*maxSnow), y = (0.95*maxSnow), label = paste0("Slope: ",toString(slope)),
                 colour="darkred", family="serif", fontface="italic", size = 7) +
        annotate("text", x= (0.2*maxSnow), y = (0.9*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                 colour="darkred", family="serif", fontface="italic", size = 7) +
        annotate("text", x= (0.2*maxSnow), y = (0.85*maxSnow), label = paste0("Correlation: ",toString(cc)),
                 colour="darkred", family="serif", fontface="italic", size = 7)
        ggplot2::ggsave(filename=outFile,plot=gg, units="in", width=8, height=6, dpi=100)

	# SNODAS scatter
        snodasTmp <- subset(sweOutPts, tag == 'SNODAS')
        dtTmp2 <- as.data.frame(snodasTmp)
        dtTmp2[['Obs']] <- obsTmp$value_mm
        outFile <- paste0(jobDir,'/SWE_SCATTER_ALL_SNODAS_',
                           strftime(dateStart,'%Y%m%d',tz='UTC'),'_',
                           strftime(dateEnd,'%Y%m%d',tz='UTC'),'.png')

        title <- paste0(tagTmp,' In-Situ SWE Observations for: ',strftime(dateStart,'%Y-%m-%d',tz='UTC'),
                        ' to: ',strftime(dateEnd,'%Y-%m-%d',tz='UTC'))
        xLab <- 'Observed SWE (mm)'
        yLab <- 'SNODAS SWE (mm)'

        if(length(dtTmp2$value_mm) == 0){
           next
        }
        lmOut <- lm(value_mm ~ Obs,dtTmp2)
        slope <- format(round(lmOut$coefficients[[2]],2),nsmall=2)
        icpt <- format(round(lmOut$coefficients[[1]],2),nsmall=2)
        cc <- format(round(cor(dtTmp2$value_mm,dtTmp2$Obs),3),nsmall=2)

        print(cc)
        testData <- cor.test(as.vector(dtTmp2$value_mm),as.vector(dtTmp2$Obs),method='pearson')
        print(testData)
        maxCheck1 <- max(dtTmp2$value_mm)
        maxCheck2 <- max(dtTmp2$Obs)
        if(maxCheck1 > maxCheck2){
           maxSnow <- maxCheck1
        }else{
           maxSnow <- maxCheck2
        }

        gg <- ggplot2::ggplot(dtTmp2,ggplot2::aes(x=Obs,y=value_mm)) +
        ggplot2::geom_point(alpha = 0.2) +
        ggplot2::ggtitle(title) +
        ggplot2::xlab(xLab) +
        ggplot2::ylab(yLab) +
        theme(plot.title = element_text(size=16)) +
        theme(axis.title.x = element_text(size=20)) +
        theme(axis.title.y = element_text(size=20)) +
        ggplot2::geom_abline(intercept = 0, slope = 1) +
        coord_cartesian(xlim = c(0, maxSnow),ylim = c(0,maxSnow)) +
        annotate("text", x= (0.2*maxSnow), y = (0.95*maxSnow), label = paste0("Slope: ",toString(slope)),
                 colour="darkred", family="serif", fontface="italic", size = 7) +
        annotate("text", x= (0.2*maxSnow), y = (0.9*maxSnow), label = paste0("Intercept: ",toString(icpt)),
                 colour="darkred", family="serif", fontface="italic", size = 7) +
        annotate("text", x= (0.2*maxSnow), y = (0.85*maxSnow), label = paste0("Correlation: ",toString(cc)),
                 colour="darkred", family="serif", fontface="italic", size = 7)
        ggplot2::ggsave(filename=outFile,plot=gg, units="in", width=8, height=6, dpi=100)
}
