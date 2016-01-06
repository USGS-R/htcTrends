#Setup libraries
# dir.create('rLibs')
# 
install.packages(c("EGRETci","usgsEGRET","smwrGraphs"),
                 repo="file:packages",type="source",
                 dependencies=c("Depends","Imports"), lib='rLibs')
# install.packages(c("EGRETci","usgsEGRET","smwrGraphs","reshape2","sbtools"),
#                  repo="file:inst/condor/packages",type="source",
#                  dependencies=c("Depends","Imports"), lib='rLibs')
args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(args[1])+1 #Should be 1

suppressPackageStartupMessages(library(EGRETci, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(usgsEGRET, quietly = TRUE, lib.loc = 'rLibs'))
# suppressPackageStartupMessages(library(sbtools, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(smwrGraphs, quietly = TRUE, lib.loc = 'rLibs'))

infoDataTotal <- readRDS("infoData.rds")
sampleDataTotal <- readRDS("sampleData.rds")
flowDataTotal <- readRDS("flowData.rds")

for(i in 1:2222){
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

}
flowFile <- flowDataTotal[flowDataTotal$site == sampleSite, c("dateTime", "value")]
Daily <- populateDaily(flowFile,1,interactive=FALSE)

INFO <- infoDataTotal[i,]
INFO$param.units <- INFO$param_units

eList <- mergeReport(INFO, Daily, Sample, interactive = FALSE)
eList$Sample <- eList$Sample[!is.na(eList$Sample$Q),]

eList <- modelEstimation(eList, windowY = INFO$windowY, windowQ = INFO$windowQ, 
                         windowS = INFO$windowS, minNumObs = INFO$minNumObs, 
                         minNumUncen = INFO$minNumUncen, edgeAdjust = INFO$edgeAdjust, verbose=FALSE)

if(!is.na(INFO$blank_start1)){
  eList <- blankTime(eList, INFO$blank_start1, INFO$blank_end1)
}

if(!is.na(INFO$blank_start2)){
  eList <- blankTime(eList, INFO$blank_start2, INFO$blank_end2)
}

if(!is.na(INFO$blank_start3)){
  eList <- blankTime(eList, INFO$blank_start3, INFO$blank_end3)
}

nBoot = INFO$nBoot
bootBreak = INFO$bootBreak
blockLength = INFO$blockLength

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
filesWeWant <- c("CIAnnualResults.csv","CIAnnualResults.rds")

# source("auth.R")
# topFolderID <- "5679a0e9e4b0da412f4fc2b7" #Phase II
# x <- query_item_identifier(type='naqwa', scheme = 'dataII', 
#                            key = paste(INFO$paramShortName, INFO$Site_no, sep="_"))
# 
# item_append_files(x$id, files = "CIAnnualResults.csv")

if(INFO$trend_92_12){
  setPDF(basename = "combo_92")
  AA.lo <- setLayout(num.rows=2)
  AA.gr <- setGraph(1, AA.lo)
  AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_92_12_start, INFO$trend_end, margin=AA.gr)
  AA.gr <- setGraph(2, AA.lo)
  concUSGS(CIAnnualResults, eList, INFO$trend_92_12_start, INFO$trend_end, margin=AA.gr)
  graphics.off()  
  # item_append_files(x$id, files = "combo_92.pdf")
  filesWeWant <- c(filesWeWant, "combo_92.pdf")
}

if(INFO$trend_82_12){
  setPDF(basename = "combo_82")
  AA.lo <- setLayout(num.rows=2)
  AA.gr <- setGraph(1, AA.lo)
  AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_82_12_start, INFO$trend_end, margin=AA.gr)
  AA.gr <- setGraph(2, AA.lo)
  concUSGS(CIAnnualResults, eList, INFO$trend_82_12_start, INFO$trend_end, margin=AA.gr)
  graphics.off()  
  # item_append_files(x$id, files = "combo_82.pdf")
  filesWeWant <- c(filesWeWant, "combo_82.pdf")
}

if(INFO$trend_72_12){
  setPDF(basename = "combo_72")
  AA.lo <- setLayout(num.rows=2)
  AA.gr <- setGraph(1, AA.lo)
  AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_72_12_start, INFO$trend_end, margin=AA.gr)
  AA.gr <- setGraph(2, AA.lo)
  concUSGS(CIAnnualResults, eList, INFO$trend_72_12_start, INFO$trend_end, margin=AA.gr)
  graphics.off()  
  # item_append_files(x$id, files = "combo_72.pdf")
  filesWeWant <- c(filesWeWant, "combo_72.pdf")
}
if(INFO$trend_02_12){
  setPDF(basename = "combo_02")
  AA.lo <- setLayout(num.rows=2)
  AA.gr <- setGraph(1, AA.lo)
  AA.pl <-fluxUSGS(CIAnnualResults, eList, INFO$trend_02_12_start, INFO$trend_end, margin=AA.gr)
  AA.gr <- setGraph(2, AA.lo)
  concUSGS(CIAnnualResults, eList, INFO$trend_02_12_start, INFO$trend_end, margin=AA.gr)
  graphics.off()  
  # item_append_files(x$id, files = "combo_02.pdf")
  filesWeWant <- c(filesWeWant, "combo_02.pdf")
}

zip(zipfile="ci.zip", files=filesWeWant)
