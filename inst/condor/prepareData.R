path <- "D:/LADData/NAWQATrends/Round1_v3"
flowPath <- paste(path,"Flow",sep="/")

dataFile <- "Round1_DATA_v3.csv"
infoFile <- "Round1_INFO_v3.csv"

totalPathData <- file.path(path,dataFile)
if(file.exists(totalPathData)){
  sampleDataTotal <- read.delim(  
    totalPathData, 
    header = TRUE,
    sep=",",
    colClasses=c('character','character','numeric','numeric','numeric','character'),
    fill = TRUE, 
    comment.char="")
  sampleDataTotal$date <- as.Date(sampleDataTotal$date, format = "%m/%d/%Y")
  
}

totalPathInfo <- file.path(path,infoFile)
if(file.exists(totalPathInfo)){
  infoDataTotal <- read.delim(  
    totalPathInfo, 
    header = TRUE,
    sep=",",
    colClasses='character',
    fill = TRUE, 
    comment.char="",
    stringsAsFactors=FALSE)
}

intCols <- c("paStart", "paLong","minNumObs","bootBreak",
             "blockLength","nBoot","qUnit",
             "trend_72_12_start","trend_82_12_start",
             "trend_92_12_start","trend_02_12_start","trend_end")

numCols <- c("windowS","dec_lat_va","dec_long_va","drain_area_va",
             "drainSqKm","windowY","windowQ","minNumUncen")
logicCols <- c("edgeAdjust")
dateCols <- c("blank_start1","blank_end1","blank_start2","blank_end2")

infoDataTotal[,intCols] <- sapply(infoDataTotal[,intCols], as.integer)
infoDataTotal[,numCols] <- sapply(infoDataTotal[,numCols], as.numeric)
infoDataTotal[,logicCols] <- sapply(infoDataTotal[,logicCols], as.logical)
infoDataTotal[,dateCols] <- sapply(infoDataTotal[,dateCols], 
                                   function(x) as.Date(x, format = "%m/%d/%Y"))
infoDataTotal[,dateCols] <- lapply(infoDataTotal[,dateCols], as.Date, origin="1970-01-01")

moreLogic <- c("trend_72_12","trend_82_12","trend_92_12","trend_02_12")
infoDataTotal[,moreLogic] <- sapply(infoDataTotal[,moreLogic], function(x) x == "Y")

totalRows <- nrow(infoDataTotal)
nRowsPerSubset <- totalRows/10
indexChunk <- seq(1,nRowsPerSubset, by=1)
intervals <- findInterval(1:totalRows,seq(nRowsPerSubset,totalRows,nRowsPerSubset))

infoDataTotal$blockLength <- rep(200, nrow(infoDataTotal))

for(i in 0:9){
  
  if(i != 9){
    subsetIndex <- which(intervals == i)
    infoData <- infoDataTotal[subsetIndex,]
  } else {
    subsetIndex <- which(intervals == i | intervals == 10)
    infoData <- infoDataTotal[subsetIndex,]
  }
  
  flowSite <- unique(infoData$Gage_number)
  sampleSite <- unique(infoData$Site_no)
  parameter <- unique(infoData$paramShortName)
  shortName <- unique(infoData$Site_no)
  
  subData <- sampleDataTotal[sampleDataTotal$Site_no %in% sampleSite,]
  subData <- subData[subData$parameter %in% parameter,]
  
  saveRDS(infoData, file=file.path(path,paste0("infoData",i+1,".RData")))
  saveRDS(subData, file=file.path(path,paste0("subData",i+1,".RData")))
}

for(i in 1:10){
  infoData <- readRDS(file.path(path,paste0("infoData",i,".RData")))
  sampleSite <- unique(infoData$Site_no)
  flowData <- data.frame(dateTime = as.Date(NA), value=as.numeric(NA), site=as.character(NA))
  flowData <- flowData[!is.na(flowData$value),]
  
  for(j in sampleSite){
    flowFile <- paste0("Q_",j,".csv") 
    totalPathFlow <- file.path(flowPath,flowFile)
    if(file.exists(totalPathFlow)){
      flowDataTotal <- read.delim(  
        totalPathFlow, 
        header = TRUE,
        sep=",",
        colClasses=c('character','character','character','Date','numeric','numeric'),
        fill = TRUE, 
        comment.char="")
    }
    
    subFlow <- flowDataTotal[,c("start_date","flow_scaled")]
    subFlow$site <- rep(j, nrow(subFlow))
    names(subFlow) <- c("dateTime","value","site")
    subFlow <- subFlow[order(subFlow$dateTime),]
    flowData <- rbind(flowData, subFlow)
  }
  saveRDS(flowData, file=file.path(path,paste0("flowData",i,".RData")))
}
