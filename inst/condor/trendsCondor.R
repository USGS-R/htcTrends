#Setup libraries
# dir.create('rLibs')
# 
# install.packages(c("EGRETci","usgsEGRET","smwrGraphs","reshape2","sbtools"),
#                  repo="file:packages",type="source",
#                  dependencies=c("Depends","Imports"), lib='rLibs')
install.packages(c("EGRETci","usgsEGRET","smwrGraphs","reshape2","sbtools"),
                 repo="file:inst/condor/packages",type="source",
                 dependencies=c("Depends","Imports"), lib='rLibs')
args <- commandArgs(trailingOnly = TRUE)

i <- as.numeric(args[1])+1 #Should be 1

suppressPackageStartupMessages(library(EGRETci, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(usgsEGRET, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(smwrGraphs, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(reshape2, quietly = TRUE, lib.loc = 'rLibs'))
suppressPackageStartupMessages(library(sbtools, quietly = TRUE, lib.loc = 'rLibs'))

# mainPath <- "D:/LADData/NAWQATrends/CondorPractice/"
# setwd(mainPath)

topFolderID <- "5522f8dae4b027f0aee3d0cb" # Trends
istatNames <- c("Min1Day","Min7Days","Min30Days","Median","Mean","Max30Days","Max7Days","Max1Day")

infoDataTotal <- readRDS("infoData.rds")
sampleDataTotal <- readRDS("sampleData.rds")
flowDataTotal <- readRDS("flowData.rds")


# path <- "D:/LADData/NAWQATrends/CondorPractice/results_May19/newResults"

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
  
  Daily <- Daily[Daily$waterYear >= minWaterYear & Daily$waterYear <= maxWaterYear,]
  
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
  
  source("D:/LADData/RCode/htcTrends/inst/shiny/config.R")
  
  folderID <- item_create(topFolderID, 
                          title=paste(parameter,shortName,sep="_"),
                          session=session) 

#   dir.create(file.path(path, i), showWarnings = FALSE)
#   setwd(file.path(path, i))
  saveRDS(eList, file="eList.rds")
  
  fileStuff <- item_append_files(folderID,
                                 files = file.path(getwd(),"eList.rds"),
                                 session = session
  )

  rename <- file.rename("eList.rds", paste0("eList","_",INFO$constitAbbrev,"_",sampleSite,".rds"))
  
  
  tableResultData <- suppressWarnings(tableResults(eList, verbose = FALSE))
  tableResultData$Site_no <- rep(INFO$Site_no, length=nrow(tableResultData))
  tableResultData$param_nm <- rep(INFO$param_nm, length=nrow(tableResultData))
  tableResultData <- tableResultData[!is.na(tableResultData[,3]) & !is.na(tableResultData[,4]) & 
                                       !is.na(tableResultData[,5]) & !is.na(tableResultData[,6]),]
  
  write.csv(tableResultData, "tableResults.csv", row.names=FALSE)
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"tableResults.csv"),
                         session = session)

  rename <- file.rename("tableResults.csv", paste0("tableResults","_",INFO$constitAbbrev,"_",sampleSite,".csv"))

  fluxTable <- tableChangeSingle(eList, flux = TRUE, yearPoints = yearPoints, verbose=FALSE)
  fluxTable$Site_no <- rep(INFO$Site_no, length=nrow(fluxTable))
  fluxTable$param_nm <- rep(INFO$param_nm, length=nrow(fluxTable))
  
  concTable <- tableChangeSingle(eList, flux = FALSE, yearPoints = yearPoints, verbose=FALSE)
  concTable$Site_no <- rep(INFO$Site_no, length=nrow(concTable))
  concTable$param_nm <- rep(INFO$param_nm, length=nrow(concTable))
  
  write.csv(fluxTable, "tableChangeFlux.csv", row.names=FALSE)
  write.csv(concTable, "tableChangeConc.csv", row.names=FALSE)
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"tableChangeFlux.csv"),
                         session = session)
#   x <- suppressMessages(item_update_identifier(folderID, 'naqwa', 
#                                                'data', paste(parameter,shortName,sep="_"), 
#                                                session ))
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"tableChangeConc.csv"),
                         session = session)

  rename <- file.rename("tableChangeConc.csv", paste0("tableChangeConc","_",INFO$constitAbbrev,"_",sampleSite,".csv"))
  rename <- file.rename("tableChangeFlux.csv", paste0("tableChangeFlux","_",INFO$constitAbbrev,"_",sampleSite,".csv"))
  
  
  setPDF(basename="multiPlotDataOverview",layout = "landscape")
    suppressWarnings(multiPlotDataOverview(eList, USGSstyle=TRUE))
  graphics.off()
  
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"multiPlotDataOverview.pdf"),
                         session = session)

  setPDF(basename="fluxBiasMulti",layout = "landscape")
  fluxBiasMulti(eList, USGSstyle = TRUE)
  graphics.off()
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"fluxBiasMulti.pdf"),
                         session = session)

  setPDF(basename = "PlotHist")
  layoutResponse <- setLayout(num.rows=2)
  AA.gr <- setGraph(1, layoutResponse)
  plotFluxHist(eList,customPar=TRUE, yearStart = yearPoints[1],yearEnd = yearPoints[length(yearPoints)],
               USGSstyle=TRUE, margin=AA.gr,cap=FALSE) 
  AA.gr <- setGraph(2, layoutResponse)
  plotConcHist(eList,customPar=TRUE, yearStart = yearPoints[1],yearEnd = yearPoints[length(yearPoints)], 
               USGSstyle=TRUE, margin=AA.gr,cap=TRUE)
  graphics.off()
  
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"PlotHist.pdf"),
                         session = session)
#   x <- suppressMessages(item_update_identifier(folderID, 'naqwa', 
#                                                'data', paste(parameter,shortName,sep="_"), 
#                                                session ))
  
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
  
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"flowStatistics.csv"),
                         session = session)

  x <- item_append_files(folderID,
                         files = file.path(getwd(),"tableFlowChange.csv"),
                         session = session)

  rename <- file.rename("tableFlowChange.csv", paste0("tableFlowChange","_",INFO$constitAbbrev,"_",sampleSite,".csv"))
  rename <- file.rename("flowStatistics.csv", paste0("flowStatistics","_",INFO$constitAbbrev,"_",sampleSite,".csv"))
  
  setPDF(basename="plotQTimeDaily",layout = "landscape")
    plotQTimeDaily(eList,USGSstyle=TRUE)
  graphics.off()
  
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"plotQTimeDaily.pdf"),
                         session = session)

  pdf("plot15.pdf",heigh=10,width=8)
    plot15(eList, yearStart=yearPoints[1], yearEnd=yearPoints[length(yearPoints)])
  dev.off()
  
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"plot15.pdf"),
                         session = session)

  bootOut <- data.frame(matrix(NA, ncol=29))
  names(bootOut) <- c("rejectC","pValC","estC","lowC","upC","lowC50","upC50","lowC95","upC95",    
                      "likeCUp","likeCDown","rejectF","pValF","estF","lowF","upF","lowF50","upF50",  
                      "lowF95","upF95","likeFUp","likeFDown","baseConc","baseFlux","iBoot",
                      "yearStart","yearEnd","Site_no","param_nm")
  
  errorMessages <- ""
  
  
  eList$Sample$Uncen[eList$Sample$Uncen == 0 & !is.na(eList$Sample$ConcLow)] <- 1
  eList$INFO$minNumUncen <- 5
  
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
    
    session <- authenticate_sb("midcondor@gmail.com", password = "ukC9py<s6Q(F")
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_72_12.txt"),
                                   session = session)
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_72_12.RData"),
                                   session = session)

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
    
    session <- authenticate_sb("midcondor@gmail.com", password = "ukC9py<s6Q(F")
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_82_12.txt"),
                                   session = session)
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_82_12.RData"),
                                   session = session)
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
    
    session <- authenticate_sb("midcondor@gmail.com", password = "ukC9py<s6Q(F")
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_92_12.txt"),
                                   session = session)
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_92_12.RData"),
                                   session = session)

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
    session <- authenticate_sb("midcondor@gmail.com", password = "ukC9py<s6Q(F")
    
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_02_12.txt"),
                                   session = session)
    fileStuff <- item_append_files(folderID,
                                   files = file.path(getwd(),"trend_02_12.RData"),
                                   session = session)
    saveRDS(eBoot_02, paste0("eBoot_02","_",INFO$constitAbbrev,"_",sampleSite,".rds"))

  }
  
  bootOut <- na.omit(bootOut)
  names(bootOut)[names(bootOut) == "lowC"] <- "lowC90"
  names(bootOut)[names(bootOut) == "upC"] <- "upC90"
  names(bootOut)[names(bootOut) == "lowF"] <- "lowF90"
  names(bootOut)[names(bootOut) == "upF"] <- "upF90"
  write.csv(bootOut, file="bootOut.csv",row.names=FALSE)
  
  write.csv(data.frame(as.character(errorMessages), stringsAsFactors = FALSE),
            file="errorMessages.csv",row.names=FALSE)
  
  session <- authenticate_sb("midcondor@gmail.com", password = "ukC9py<s6Q(F")
  
  fileStuff <- item_append_files(folderID,
                                 files = file.path(getwd(),"bootOut.csv"),
                                 session = session)
  rename <- file.rename("bootOut.csv", paste0("bootOut","_",INFO$constitAbbrev,"_",sampleSite,".csv"))
  
  setPDF(basename="plotSDLogQ")
    layoutInfo <- setLayout(width=5, height=5)
    layoutStuff <- setGraph(1, layoutInfo)
    plotSDLogQ(eList,USGSstyle=TRUE, window=8)
  graphics.off()
  
  x <- item_append_files(folderID,
                         files = file.path(getwd(),"plotSDLogQ.pdf"),
                         session = session)

  x <- suppressMessages(item_update_identifier(folderID, 'naqwa', 
                                               'data', paste(parameter,shortName,sep="_"), 
                                               session ))
  
  files <-  list.files() 
  filesWeDontWant <- c("trendsCondor.R","condor.sub","simple.sh","packages.zip",
                       "flowData.RData","infoData.RData","subData.RData","unzip",
                       "rLibs","packages","condor_exec.exe")
  filesWeWant <- files[!(files %in% filesWeDontWant)]
  zip(zipfile="trends.zip", files=filesWeWant)

###########################
# }
###########################