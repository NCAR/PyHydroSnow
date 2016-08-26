PlotSwe <- function(n, modDfs, obs, obsmeta,
                        labMods=NULL,
                        labObs="Observed",
                        labTitle="Accumulated Precipitation and SWE",
                        lnCols=NULL, lnWds=NULL, lnTyps=NULL,
                        stdate=NULL,
                        enddate=NULL,
                        precCol.obs="CumPrec_mm", precCol.mod="ACCPRCP",
                        sweCol.obs="SWE_mm", sweCol.mod="SNEQV", 
			fact=1, snowh=FALSE, stdate_prec=NULL) {
  # Parse type of input for model data (dataframe or list of multiple dataframes)
  if (is.data.frame(modDfs)) {
        str1 <- modDfs
        strcnt <- 1
  } else if (is.list(modDfs)) {
        str1 <- modDfs[[1]]
        strcnt <- length(modDfs)
  } else {
        stop("modDfs must be a dataframe or a list of dataframes")
  }
  if (snowh) {
        ylab <- "Snow Depth (mm)"
        leglab <- "Snow Depth"
  } else {
        ylab <- "SWE (mm)"
        leglab <- "SWE" }
  # Subset by dates
  if (is.null(stdate)) stdate <- min(str1$POSIXct)
  if (is.null(enddate)) enddate <- max(str1$POSIXct)
  str1 <- subset(str1, str1$statArg==n & str1$POSIXct>=stdate & str1$POSIXct<=enddate)
  obs <- subset(obs, obs$site_id==n & obs$POSIXct>=stdate & obs$POSIXct<=enddate)
  if (!(is.null(stdate_prec))) {
	str1_prec <- subset(str1, str1$statArg==n & str1$POSIXct>=stdate_prec & str1$POSIXct<=enddate)
	obs_prec <- subset(obs, obs$site_id==n & obs$POSIXct>=stdate_prec & obs$POSIXct<=enddate)
  } else {
	str1_prec <- str1
	obs_prec <- obs	
  }
  # Calculate summary stats
  precBias <- round(((str1_prec[nrow(str1_prec),precCol.mod]-str1_prec[1,precCol.mod]) - 
			(obs_prec[nrow(obs_prec),precCol.obs]-obs_prec[1,precCol.obs])) / 
			(obs_prec[nrow(obs_prec),precCol.obs]-obs_prec[1,precCol.obs]) * 100, 1)
  tmp <- plyr::join(str1, obs, by="POSIXct", type="inner")
  sweBias <- round(sum(tmp[,sweCol.mod]*fact-tmp[,sweCol.obs], na.rm=TRUE)/sum(tmp[,sweCol.obs], na.rm=TRUE) * 100, 1)
  # Calculate maximum y val for plot limits
  ymax_swe <- max(max(str1[,sweCol.mod], na.rm=TRUE)*fact, max(obs[,sweCol.obs], na.rm=TRUE), na.rm=TRUE)
  ymax_prec <- max(str1_prec[nrow(str1_prec),precCol.mod]-str1_prec[1,precCol.mod],
                obs_prec[nrow(obs_prec),precCol.obs]-obs_prec[1,precCol.obs], na.rm=TRUE)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (stri in modDfs[2:length(modDfs)]) {
                stri <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
		if (!(is.null(stdate_prec))) {
   		     stri_prec <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate_prec & stri$POSIXct<=enddate)
  		} else {
        	     stri_prec <- stri
  		}
                precBias <- c(precBias,
                                round(((stri_prec[nrow(stri_prec),precCol.mod]-stri_prec[1,precCol.mod]) - (obs_prec[nrow(obs_prec),precCol.obs]-obs_prec[1,precCol.obs])) /
                                        (obs_prec[nrow(obs_prec),precCol.obs]-obs_prec[1,precCol.obs]) * 100, 1))
                tmp <- plyr::join(stri, obs, by="POSIXct", type="inner")
                sweBias <- c(sweBias, round(sum(tmp[,sweCol.mod]*fact-tmp[,sweCol.obs], na.rm=TRUE)/sum(tmp[,sweCol.obs], na.rm=TRUE) * 100, 1))
                ymax_swe <- max(ymax_swe, max(stri[,sweCol.mod], na.rm=TRUE)*fact, na.rm=TRUE)
		ymax_prec <- max(ymax_prec, (stri_prec[nrow(stri_prec),precCol.mod]-stri_prec[1,precCol.mod]),  na.rm=TRUE)
                }
        }
  # Set colors, widths, types
  if (is.null(lnCols)) lnCols <- sample(colours(), strcnt)
  if (is.null(lnWds)) lnWds <- rep(1, strcnt)
  if (is.null(lnTyps)) lnTyps <- rep(1, strcnt)
  # Set labels
  if (is.null(labMods)) labMods <- paste0("Model", 1:strcnt)
  # Create plot
  par(mar=c(5,4,4,5)+.1)
  plot(str1$POSIXct, str1[,sweCol.mod]*fact, typ='l', ylim=c(0, ymax_swe),
        xlim=c(stdate, enddate), col=lnCols[1], lty=2, lwd=lnWds[1],
        xlab="", ylab=ylab,
        cex.axis=1.2, cex.lab=1.2)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (j in 2:length(modDfs)) {
                stri <- modDfs[[j]]
                stri <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                lines(stri$POSIXct, stri[,sweCol.mod]*fact, col=lnCols[j], lty=2, lwd=lnWds[j])
                }
        }
  lines(obs$POSIXct, obs[,sweCol.obs], col='black', lwd=2, lty=2)
  par(new=TRUE)
  plot(str1_prec$POSIXct, str1_prec[,precCol.mod]-str1_prec[1,precCol.mod], typ='l', 
        col=lnCols[1], lty=1, lwd=lnWds[1], ylim=c(0, ymax_prec), 
        xaxt="n", yaxt="n", xlab="", ylab="")
  axis(4)
  mtext("Precip (mm)", side=4, line=3)
  if (!is.data.frame(modDfs) & is.list(modDfs) & length(modDfs)>1) {
        for (j in 2:length(modDfs)) {
                stri <- modDfs[[j]]
                stri <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate & stri$POSIXct<=enddate)
                if (!(is.null(stdate_prec))) {
                     stri_prec <- subset(stri, stri$statArg==n & stri$POSIXct>=stdate_prec & stri$POSIXct<=enddate)
                } else {
                     stri_prec <- stri
                }
                lines(stri_prec$POSIXct, stri_prec[,precCol.mod]-stri_prec[1,precCol.mod], col=lnCols[j], lty=1, lwd=lnWds[j])
                }
        }
  lines(obs_prec$POSIXct, obs_prec[,precCol.obs]-obs_prec[1,precCol.obs], col='black', lwd=2, lty=1)
  if (snowh) {
        title(paste0(labTitle, ": Met", n, " ", obsmeta$site_name[obsmeta$site_id==n]), cex.main=1.2)
  } else {
        title(paste0(labTitle, ": ",
               obsmeta$site_name[obsmeta$site_id==n],"(",n, ") in ",
               obsmeta$county[obsmeta$site_id==n], " County, Elev ",
               obsmeta$elev[obsmeta$site_id==n], " ft"), cex.main=1.2)
  }
  legend("topleft", c(labMods[1:strcnt], labObs, "", "Accumulated Precip", leglab),
                lty=c(rep(1,strcnt),1,1,1,2), lwd=c(lnWds[1:strcnt],2,1,1,1),
                col=c(lnCols[1:strcnt], 'black', 'white', 'grey50', 'grey50'), cex=1.2,
                bg="white")
  txtblk <- ""
  for (i in 1:strcnt) {
        txtblk <- paste0(txtblk, " Model", i, ": Prec bias ", precBias[i], "%, SWE bias ", sweBias[i], "%")
        #print(txtblk)
        }
  mtext(txtblk, side=3, line=0.0, cex=1.0)
}


PlotMet <- function(obs=obsMetData, mod, 
			site, 
			obsVars, modVars, 
			lnLabs, title, xLab="POSIXct", 
			adj=0, mult=1, cols=1,
			stdate=NULL, enddate=NULL) {
  plotList <- c()
  obs <- subset(obs, obs$site_id==site)
  obs$tmp <- "Observed"
  mod <- subset(mod, mod$statArg==site)
  mod$tmp <- "Model"
  # Subset by dates
  if (is.null(stdate)) stdate <- min(mod$POSIXct)
  if (is.null(enddate)) enddate <- max(mod$POSIXct)
  mod <- subset(mod, mod$POSIXct>=stdate & mod$POSIXct<=enddate)
  obs <- subset(obs, obs$POSIXct>=stdate & obs$POSIXct<=enddate)
  # Loop through vars
  for (i in 1:length(obsVars)) {
	p <- ggplot2::ggplot() + 
		geom_line(data=obs, aes_string(x="POSIXct", y=paste0(obsVars[i], "*", as.character(mult), "+", as.character(adj)), color="tmp"), linetype=i) + 
		geom_line(data=mod, aes_string(x="POSIXct", y=paste0(modVars[i], "*", as.character(mult), "+", as.character(adj)), color="tmp"), linetype=i) + 
		xlim(min(obs$POSIXct), max(obs$POSIXct)) +
		scale_color_discrete(name=element_blank())
	if (i == 1) {
		p <- p + labs(y=lnLabs[i], x=element_blank(), title=title) + 
			theme(plot.title = element_text(size=14, face="bold", vjust=2))
	} else if (i == length(obsVars)) {
		p <- p + labs(y=lnLabs[i], x=xLab)
	} else {
		p <- p + labs(y=lnLabs[i], x=element_blank())
	}
	assign(paste0("p",i), p)
	plotList <- c(plotList, paste0("p",i))
}
# Generate panel plot
library(grid)
numPlots = length(plotList)
layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
if (numPlots==1) {
    print(plotList[1])
} else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(get(plotList[i]), vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

