#Setup libraries
# dir.create('rLibs')
# 
install.packages(c("EGRETci","usgsEGRET","smwrGraphs","reshape2"),
                 repo="file:packages",type="source",
                 dependencies=c("Depends","Imports"), lib='rLibs')
# install.packages(c("EGRETci","usgsEGRET","smwrGraphs","reshape2","smwrQW"),
#                  repo="file:inst/condor/packages",type="source",
#                  dependencies=c("Depends","Imports"), lib='rLibs')
args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(args[1])+1 #Should be 1

suppressPackageStartupMessages(library(EGRETci, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(usgsEGRET, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(smwrGraphs, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(reshape2, quietly = TRUE, lib.loc = 'rLibs'))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# mainPath <- "D:/LADData/NAWQATrends/CondorPractice/"
# setwd(mainPath)

istatNames <- c("Min1Day","Min7Days","Min30Days","Median","Mean","Max30Days","Max7Days","Max1Day")

infoDataTotal <- readRDS("infoData.rds")
sampleDataTotal <- readRDS("sampleData.rds")
flowDataTotal <- readRDS("flowData.rds")


###########################
# for (i in 8:nrow(infoDataTotal)){
###########################

  flowSite <- infoDataTotal$Gage_number[i]
  sampleSite <- infoDataTotal$Site_no[i]
  parameter <- infoDataTotal$paramShortName[i]
  shortName <- infoDataTotal$Site_no[i]
  
  Sample <- sampleDataTotal[sampleDataTotal$Site_no == sampleSite & 
                              sampleDataTotal$parameter == parameter,
                            c("date","ConcLow","ConcHigh","Uncen")]
  names(Sample)[names(Sample) == "date"] <- c("dateTime")
  Sample <- Sample[order(Sample$dateTime),]
  Sample <- populateSampleColumns(Sample)
  
  Sample$waterYear <- as.integer(Sample$DecYear)
  Sample$waterYear[Sample$Month %in% c(10:12)] <- Sample$waterYear[Sample$Month %in% c(10:12)]+1
  
  
  flowFile <- flowDataTotal[flowDataTotal$site == sampleSite, c("dateTime", "value")]
  Daily <- populateDaily(flowFile,1,interactive=FALSE)
  
  INFO <- infoDataTotal[i,]
  INFO$param.units <- INFO$param_units
  
  Daily$waterYear <- as.integer(Daily$DecYear)
  Daily$waterYear[Daily$Month %in% c(10:12)] <- Daily$waterYear[Daily$Month %in% c(10:12)]+1
  
  yearPoints <- c(INFO$trend_72_12_start, INFO$trend_82_12_start, 
                  INFO$trend_92_12_start, INFO$trend_02_12_start,INFO$trend_end)
  yearPoints <- yearPoints[c(INFO$trend_72_12, INFO$trend_82_12, 
                             INFO$trend_92_12, INFO$trend_02_12,TRUE)]
  
  minWaterYear <- min(Sample$waterYear, na.rm=TRUE)
  maxWaterYear <- max(Sample$waterYear, na.rm=TRUE)
  
  # Daily <- Daily[Daily$waterYear >= minWaterYear & Daily$waterYear <= maxWaterYear,]
  eList <- mergeReport(INFO, Daily, Sample, interactive = FALSE)
  eList$Sample <- eList$Sample[!is.na(eList$Sample$Q),]
  
  eList <- modelEstimation(eList, windowY = INFO$windowY, windowQ = INFO$windowQ, 
                           windowS = INFO$windowS, minNumObs = INFO$minNumObs, 
                           edgeAdjust = INFO$edgeAdjust, verbose=FALSE)
  
  if(!is.na(INFO$blank_start1)){
    eList <- blankTime(eList, INFO$blank_start1, INFO$blank_end1)
  }
  
  if(!is.na(INFO$blank_start2)){
    eList <- blankTime(eList, INFO$blank_start2, INFO$blank_end2)
  }
  
  if(!is.na(INFO$blank_start3)){
    eList <- blankTime(eList, INFO$blank_start3, INFO$blank_end3)
  }
  

  tableResultData <- suppressWarnings(tableResults(eList, verbose = FALSE))
  tableResultData$Site_no <- rep(INFO$Site_no, length=nrow(tableResultData))
  tableResultData$param_nm <- rep(INFO$param_nm, length=nrow(tableResultData))
  tableResultData <- tableResultData[!is.na(tableResultData[,3]) & !is.na(tableResultData[,4]) & 
                                       !is.na(tableResultData[,5]) & !is.na(tableResultData[,6]),]
  write.csv(tableResultData, "tableResults.csv", row.names=FALSE)

  fluxTable <- tableChangeSingle(eList, flux = TRUE, yearPoints = yearPoints, verbose=FALSE)
  fluxTable$Site_no <- rep(INFO$Site_no, length=nrow(fluxTable))
  fluxTable$param_nm <- rep(INFO$param_nm, length=nrow(fluxTable))
  
  concTable <- tableChangeSingle(eList, flux = FALSE, yearPoints = yearPoints, verbose=FALSE)
  concTable$Site_no <- rep(INFO$Site_no, length=nrow(concTable))
  concTable$param_nm <- rep(INFO$param_nm, length=nrow(concTable))
  
  write.csv(fluxTable, "tableChangeFlux.csv", row.names=FALSE)
  write.csv(concTable, "tableChangeConc.csv", row.names=FALSE)

  setPDF(basename="multiPlotDataOverview",layout = "landscape")
    suppressWarnings(multiPlotDataOverview(eList, USGSstyle=TRUE))
  graphics.off()

  setPDF(basename="fluxBiasMulti",layout = "landscape")
    eList <- fluxBiasMulti(eList, USGSstyle = TRUE, rResid=TRUE)
  graphics.off()
  
  saveRDS(eList, file="eList.rds")
  
  setPDF(basename = "PlotHist")
  layoutResponse <- setLayout(num.rows=2)
  AA.gr <- setGraph(1, layoutResponse)
  plotFluxHist(eList,customPar=TRUE, yearStart = yearPoints[1],yearEnd = yearPoints[length(yearPoints)],
               USGSstyle=TRUE, margin=AA.gr,cap=FALSE) 
  AA.gr <- setGraph(2, layoutResponse)
  plotConcHist(eList,customPar=TRUE, yearStart = yearPoints[1],yearEnd = yearPoints[length(yearPoints)], 
               USGSstyle=TRUE, margin=AA.gr,cap=TRUE)
  graphics.off()

  for(istat in 1:8){
    subSeries <- printSeries(eList, istat, verbose=FALSE)
    subChange <- tableFlowChange(eList, istat, yearPoints=yearPoints, verbose=FALSE)
    subSeries$flowStatistic <- rep(istatNames[istat],length=nrow(subSeries))
    subChange$flowStatistic <- rep(istatNames[istat],length=nrow(subChange))
    if(1 == istat){
      fullSeries <- subSeries
      fullChange <- subChange
    } else {
      fullSeries <- rbind(fullSeries, subSeries)
      fullChange <- rbind(fullChange, subChange)
    }
  }
  longDF <- fullSeries[,c("years","qActual","flowStatistic")]
  longDF$type <- rep("qActual", length=nrow(longDF))
  longDF2 <- fullSeries[,c("years","qSmooth","flowStatistic")]
  longDF2$type <- rep("qSmooth", length=nrow(longDF2))
  names(longDF) <- c("years","value","flowStatistic","type")
  names(longDF2) <- c("years","value","flowStatistic","type")
  longDF <- rbind(longDF, longDF2)
  
  fullSeries <- dcast(longDF, ... ~ flowStatistic + type)
  rm(longDF, longDF2,subSeries, subChange)
  
  fullSeries$Site_no <- rep(INFO$Site_no, length=nrow(fullSeries))
  fullSeries$param_nm <- rep(INFO$param_nm, length=nrow(fullSeries))
  fullChange$Site_no <- rep(INFO$Site_no, length=nrow(fullChange))
  fullChange$param_nm <- rep(INFO$param_nm, length=nrow(fullChange))
  
  write.csv(fullSeries, "flowStatistics.csv", row.names=FALSE)
  write.csv(fullChange, "tableFlowChange.csv", row.names=FALSE)

  setPDF(basename="plotQTimeDaily",layout = "landscape")
    plotQTimeDaily(eList,USGSstyle=TRUE)
  graphics.off()
  
  pdf("plot15.pdf",heigh=10,width=8)
    plot15(eList, yearStart=yearPoints[1], yearEnd=yearPoints[length(yearPoints)])
  dev.off()

  bootOut <- data.frame(matrix(NA, ncol=29))
  names(bootOut) <- c("rejectC","pValC","estC","lowC","upC","lowC50","upC50","lowC95","upC95",    
                      "likeCUp","likeCDown","rejectF","pValF","estF","lowF","upF","lowF50","upF50",  
                      "lowF95","upF95","likeFUp","likeFDown","baseConc","baseFlux","iBoot",
                      "yearStart","yearEnd","Site_no","param_nm")
  
  errorMessages <- ""
  
  eList$Sample$Uncen[eList$Sample$Uncen == 0 & !is.na(eList$Sample$ConcLow)] <- 1
  eList$INFO$minNumUncen <- 0.5
  
  if(INFO$trend_72_12){
    caseSetUp <- suppressMessages(trendSetUp(eList, 
                            year1=INFO$trend_72_12_start, 
                            year2=INFO$trend_end, 
                            nBoot = INFO$nBoot, 
                            bootBreak = INFO$bootBreak, 
                            blockLength = INFO$blockLength))

    eBoot_72 <- wBT(eList, caseSetUp, fileName ="trend_72_12.txt")
    
    bo <- eBoot_72$bootOut
    bo$yearStart <- INFO$trend_72_12_start
    bo$yearEnd <- INFO$trend_end
    bo$Site_no <- INFO$Site_no
    bo$param_nm <-INFO$param_nm
    bootOut <- rbind(bootOut,bo)
    suppressMessages(saveEGRETci(eList, eBoot_72, caseSetUp, fileName = "trend_72_12"))

    saveRDS(eBoot_72, paste0("eBoot_72","_",INFO$constitAbbrev,"_",sampleSite,".rds"))

  }
  
  if(INFO$trend_82_12){
    caseSetUp <- suppressMessages(trendSetUp(eList, 
                            year1=INFO$trend_82_12_start, 
                            year2=INFO$trend_end, 
                            nBoot = INFO$nBoot, 
                            bootBreak = INFO$bootBreak, 
                            blockLength = INFO$blockLength))


    eBoot_82 <- wBT(eList, caseSetUp, fileName ="trend_82_12.txt")

    bo <- eBoot_82$bootOut
    bo$yearStart <- INFO$trend_82_12_start
    bo$yearEnd <- INFO$trend_end
    bo$Site_no <- INFO$Site_no
    bo$param_nm <-INFO$param_nm
    bootOut <- rbind(bootOut,bo)
    suppressMessages(saveEGRETci(eList, eBoot_82, caseSetUp, fileName = "trend_82_12"))

    saveRDS(eBoot_82, paste0("eBoot_82","_",INFO$constitAbbrev,"_",sampleSite,".rds"))

  }
  
  if(INFO$trend_92_12){
    caseSetUp <- suppressMessages(trendSetUp(eList, 
                            year1=INFO$trend_92_12_start, 
                            year2=INFO$trend_end, 
                            nBoot = INFO$nBoot, 
                            bootBreak = INFO$bootBreak, 
                            blockLength = INFO$blockLength))


    eBoot_92 <- wBT(eList, caseSetUp, fileName ="trend_92_12.txt")

    suppressMessages(saveEGRETci(eList, eBoot_92, caseSetUp, fileName = "trend_92_12"))
    bo <- eBoot_92$bootOut
    bo$yearStart <- INFO$trend_92_12_start
    bo$yearEnd <- INFO$trend_end
    bo$Site_no <- INFO$Site_no
    bo$param_nm <-INFO$param_nm
    bootOut <- rbind(bootOut,bo)

    saveRDS(eBoot_92, paste0("eBoot_92","_",INFO$constitAbbrev,"_",sampleSite,".rds"))

    
  }
  
  if(INFO$trend_02_12){
    caseSetUp <- suppressMessages(trendSetUp(eList, 
                            year1=INFO$trend_02_12_start, 
                            year2=INFO$trend_end, 
                            nBoot = INFO$nBoot, 
                            bootBreak = INFO$bootBreak, 
                            blockLength = INFO$blockLength))
 
    eBoot_02 <- wBT(eList, caseSetUp, fileName ="trend_02_12.txt")

    bo <- eBoot_02$bootOut
    bo$yearStart <- INFO$trend_02_12_start
    bo$yearEnd <- INFO$trend_end
    bo$Site_no <- INFO$Site_no
    bo$param_nm <-INFO$param_nm
    bootOut <- rbind(bootOut,bo)
    suppressMessages(saveEGRETci(eList, eBoot_02, caseSetUp, fileName = "trend_02_12"))

    saveRDS(eBoot_02, paste0("eBoot_02","_",INFO$constitAbbrev,"_",sampleSite,".rds"))

  }
  
  bootOut <- na.omit(bootOut)
  names(bootOut)[names(bootOut) == "lowC"] <- "lowC90"
  names(bootOut)[names(bootOut) == "upC"] <- "upC90"
  names(bootOut)[names(bootOut) == "lowF"] <- "lowF90"
  names(bootOut)[names(bootOut) == "upF"] <- "upF90"
  
  biasOut <- fluxBiasStat(eList$Sample)
  bootOut$bias1 <- rep(as.numeric(biasOut[1]), nrow(bootOut))
  bootOut$bias2 <- rep(as.numeric(biasOut[2]), nrow(bootOut))
  bootOut$bias3 <- rep(as.numeric(biasOut[3]), nrow(bootOut))
  
  residuals <- log(eList$Sample$ConcHigh)-eList$Sample$yHat
  bootOut$residualModeSkewness <- (mean(residuals) - Mode(residuals))/sd(residuals)
  bootOut$residualMedianSkewness <- 3*(mean(residuals) - median(residuals))/sd(residuals)
  bootOut$extrapolationMetric <- max(eList$Daily$ConcDay, na.rm = TRUE)/(2* max(eList$Sample$ConcHigh, na.rm = TRUE))
  
  max2resid <- order(-abs(residuals))[1:2]
  biasOutTrunc <- fluxBiasStat(eList$Sample[-max2resid,])
  bootOut$bias1Trunc <- rep(as.numeric(biasOutTrunc[1]), nrow(bootOut))
  bootOut$bias2Trunc <- rep(as.numeric(biasOutTrunc[2]), nrow(bootOut))
  bootOut$bias3Trunc <- rep(as.numeric(biasOutTrunc[3]), nrow(bootOut))
  
  write.csv(bootOut, file="bootOut.csv",row.names=FALSE)
  
  write.csv(data.frame(as.character(errorMessages), stringsAsFactors = FALSE),
            file="errorMessages.csv",row.names=FALSE)

  setPDF(basename="plotSDLogQ")
    layoutInfo <- setLayout(width=5, height=5)
    layoutStuff <- setGraph(1, layoutInfo)
    plotSDLogQ(eList,USGSstyle=TRUE, window=8)
  graphics.off()

  nBoot = INFO$nBoot
  bootBreak = INFO$bootBreak
  blockLength = INFO$blockLength
  
  eList$INFO$minNumUncen <- 0.5
  eList$INFO$minNumObs <- 5
  
  CIAnnualResults <- ciCalculations(eList,
                                    nBoot=nBoot, 
                                    widthCI = 90, 
                                    blockLength=blockLength)
  
  saveRDS(CIAnnualResults, file="CIAnnualResults.rds")
  write.csv(CIAnnualResults, file = "CIAnnualResults.csv", row.names = FALSE)
  
  #######################
  fluxUSGS <- function(CIAnnualResults, eList, yearStart, yearEnd, ...){
    nBoot <- attr(CIAnnualResults, "nBoot")
    blockLength <- attr(CIAnnualResults, "blockLength")
    probs <- attr(CIAnnualResults, "probs")
    
    widthCI <- (max(probs) - min(probs))*100
    
    CIAnnualResults <- CIAnnualResults[CIAnnualResults$Year >= yearStart & CIAnnualResults$Year <= yearEnd,]
    
    localAnnualResults <- setupYears(paStart = eList$INFO$paStart, paLong = eList$INFO$paLong,
                                     localDaily = eList$Daily)
    periodName <- setSeasonLabel(localAnnualResults)
    title3 <- paste(widthCI,"% CI on FN Concentration, Replicates =",nBoot,"Block=",blockLength,"days")
    
    title <- paste(eList$INFO$shortName, " ", eList$INFO$paramShortName, 
                   "\n", periodName, "\n",title3)
    
    numYears <- length(localAnnualResults$DecYear)
    subAnnualResults<-localAnnualResults[localAnnualResults$DecYear>=yearStart & localAnnualResults$DecYear <= yearEnd,]
    
    annConc <- subAnnualResults$Conc
    concMax <- 1.05*max(c(CIAnnualResults$FNConcHigh,annConc), na.rm=TRUE)
    
    title3<-"\nFlux Estimates (dots) & Flow Normalized Flux (solid line) & 90% Confidence Intervals (dashed line)"
    
    subAnnualResults$Date <- as.Date(paste0(as.character(as.integer(subAnnualResults$DecYear)),"-04-01"))
    col="black"
    col <- list(points=col)
    
    fluxUnit <- fluxConst[shortCode=3][[1]]
    unitFactorReturn <- fluxUnit@unitFactor
    ylabel <- paste("Flux in",fluxUnit@unitUSGS)
    
    annFlux<-unitFactorReturn*subAnnualResults$Flux
    fnFlux<-unitFactorReturn*subAnnualResults$FNFlux
    xInfo <- generalAxis(x=subAnnualResults$DecYear, minVal=yearStart, maxVal=yearEnd,padPercent=0)  
    combinedY <- c(annFlux,fnFlux)
    yInfo <- generalAxis(x=combinedY, minVal=0, maxVal=NA, padPercent=5)
    
    fluxMax <- 1.05*max(c(CIAnnualResults$FNFluxHigh*unitFactorReturn,annFlux), na.rm=TRUE)
    
    currentPlot <- colorPlot(subAnnualResults$Date, annFlux, color=rep("points",length(annFlux)),
                             Plot=list(what="points",color=col),
                             yaxis.range=c(0,fluxMax), ytitle=ylabel,
                             xaxis.range=c(as.Date(paste0(xInfo$bottom,"-01-01")),as.Date(paste0(xInfo$top,"-01-01"))), ...)
    addXY(subAnnualResults$Date, fnFlux, Plot=list(color="black"))
    xMid <- mean(currentPlot$xax$range)
    yTop <- 0.9*diff(currentPlot$yax$range)+min(currentPlot$yax$range)
    
    names(INFO) <- gsub("\\.","_",names(INFO))
    names(INFO) <- tolower(names(INFO))
    
    title<- title3 
    
    addTitle(title, Justification = "center")
    # addCaption(paste(INFO$shortname," (", INFO$site_no ,") ",INFO$paramshortname, ", ",periodName))
    
    addXY(as.Date(paste0(as.integer(CIAnnualResults$Year),"-04-01")), CIAnnualResults$FNFluxLow*unitFactorReturn, Plot=list(color="black", type="dashed"))
    addXY(as.Date(paste0(as.integer(CIAnnualResults$Year),"-04-01")), CIAnnualResults$FNFluxHigh*unitFactorReturn, Plot=list(color="black", type="dashed"))
  }
  
  concUSGS <- function(CIAnnualResults, eList, yearStart, yearEnd, ...){
    nBoot <- attr(CIAnnualResults, "nBoot")
    blockLength <- attr(CIAnnualResults, "blockLength")
    probs <- attr(CIAnnualResults, "probs")
    
    CIAnnualResults <- CIAnnualResults[CIAnnualResults$Year >= yearStart & CIAnnualResults$Year <= yearEnd,]
    
    widthCI <- (max(probs) - min(probs))*100
    
    localAnnualResults <- setupYears(paStart = eList$INFO$paStart, paLong = eList$INFO$paLong,
                                     localDaily = eList$Daily)
    periodName <- setSeasonLabel(localAnnualResults)
    title3 <- paste(widthCI,"% CI on FN Concentration, Replicates =",nBoot,"Block=",blockLength,"days")
    
    numYears <- length(localAnnualResults$DecYear)
    subAnnualResults<-localAnnualResults[localAnnualResults$DecYear>=yearStart & localAnnualResults$DecYear <= yearEnd,]
    
    annConc <- subAnnualResults$Conc
    concMax <- 1.05*max(c(CIAnnualResults$FNConcHigh,annConc), na.rm=TRUE)
    
    localAnnualResults <- setupYears(paStart=eList$INFO$paStart,paLong=eList$INFO$paLong, localDaily = eList$Daily)
    
    periodName<-setSeasonLabel(localAnnualResults=localAnnualResults)
    title3<-"\nMean Concentration (dots) & Flow Normalized Concentration (solid line) & 90% Confidence Intervals (dashed line)" 
    
    xInfo <- generalAxis(x=localAnnualResults$DecYear, minVal=yearStart, maxVal=yearEnd, padPercent=0)
    
    combinedY <- c(localAnnualResults$Conc,localAnnualResults$FNConc[localAnnualResults$DecYear>xInfo$bottom & localAnnualResults$DecYear<xInfo$top])
    yInfo <- generalAxis(x=combinedY, minVal=0, maxVal=concMax, padPercent=5, 
                         units=eList$INFO$param.units)
    
    localAnnualResults$Date <- as.Date(paste0(as.character(as.integer(localAnnualResults$DecYear)),"-04-01"))
    col <- list(points="black")
    
    currentPlot <- colorPlot(localAnnualResults$Date, localAnnualResults$Conc, rep("points",nrow(localAnnualResults)),
                             Plot=list(what="points",color=col),
                             yaxis.range=c(yInfo$bottom,yInfo$top), ytitle=yInfo$label,
                             xaxis.range=c(as.Date(paste0(xInfo$bottom,"-01-01")),as.Date(paste0(xInfo$top,"-01-01"))), ...)
    
    xMid <- mean(currentPlot$xax$range)
    yTop <- 0.9*diff(currentPlot$yax$range)+min(currentPlot$yax$range)
    
    addXY(localAnnualResults$Date, localAnnualResults$FNConc, Plot=list(color="black"))
    
    INFO <- eList$INFO
    names(INFO) <- gsub("\\.","_",names(INFO))
    names(INFO) <- tolower(names(INFO))
    
    title<-title3 
    
    addTitle(title, Justification = "center")
    addCaption(paste(INFO$shortname," (", INFO$site_no ,") ",INFO$paramshortname, ", ",periodName))
    addXY(as.Date(paste0(as.integer(CIAnnualResults$Year),"-04-01")), CIAnnualResults$FNConcLow, Plot=list(color="black", type="dashed"))
    addXY(as.Date(paste0(as.integer(CIAnnualResults$Year),"-04-01")), CIAnnualResults$FNConcHigh, Plot=list(color="black", type="dashed"))
    
  }

  if(INFO$trend_92_12){
    setPDF(basename = "combo_92")
    AA.lo <- setLayout(num.rows=2)
    AA.gr <- setGraph(1, AA.lo)
    AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_92_12_start, INFO$trend_end, margin=AA.gr)
    AA.gr <- setGraph(2, AA.lo)
    concUSGS(CIAnnualResults, eList, INFO$trend_92_12_start, INFO$trend_end, margin=AA.gr)
    graphics.off()
  }
  
  if(INFO$trend_82_12){
    setPDF(basename = "combo_82")
    AA.lo <- setLayout(num.rows=2)
    AA.gr <- setGraph(1, AA.lo)
    AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_82_12_start, INFO$trend_end, margin=AA.gr)
    AA.gr <- setGraph(2, AA.lo)
    concUSGS(CIAnnualResults, eList, INFO$trend_82_12_start, INFO$trend_end, margin=AA.gr)
    graphics.off()  

  }
  
  if(INFO$trend_72_12){
    setPDF(basename = "combo_72")
    AA.lo <- setLayout(num.rows=2)
    AA.gr <- setGraph(1, AA.lo)
    AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_72_12_start, INFO$trend_end, margin=AA.gr)
    AA.gr <- setGraph(2, AA.lo)
    concUSGS(CIAnnualResults, eList, INFO$trend_72_12_start, INFO$trend_end, margin=AA.gr)
    graphics.off()
  }
  
  if(INFO$trend_02_12){
    setPDF(basename = "combo_02")
    AA.lo <- setLayout(num.rows=2)
    AA.gr <- setGraph(1, AA.lo)
    AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_02_12_start, INFO$trend_end, margin=AA.gr)
    AA.gr <- setGraph(2, AA.lo)
    concUSGS(CIAnnualResults, eList, INFO$trend_02_12_start, INFO$trend_end, margin=AA.gr)
    graphics.off()  
  }
  

  files <-  list.files() 
  filesWeDontWant <- c("trendsNOCondor.R","condor.sub","simple.sh","packages.zip",
                       "flowData.RData","infoData.RData","subData.RData","unzip",
                       "rLibs","packages","condor_exec.exe","config.R","auth.R")
  filesWeWant <- files[!(files %in% filesWeDontWant)]
  zip(zipfile="trends.zip", files=filesWeWant)

###########################
# }
###########################