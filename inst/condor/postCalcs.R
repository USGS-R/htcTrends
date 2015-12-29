setwd("D:/LADData/NAWQATrends/CondorPractice/results_May19")
library(EGRET)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

a <- list.files(pattern = "*.zip")

sampleFolder <- unzip(a[1])
getColTypes <- sampleFolder[grep("bootOut",sampleFolder)]
getColTypes <- read.csv(getColTypes)
typesBootOut <- as.character(sapply(getColTypes, class))
typesBootOut[28:29] <- "character"

tableChangeConcTypes <- sampleFolder[grep("tableChangeConc",sampleFolder)]
tableChangeConcTypes <- read.csv(tableChangeConcTypes)
typestableChangeConc <- as.character(sapply(tableChangeConcTypes, class))
typestableChangeConc[7:8] <- "character"
typestableChangeConc[3:6] <- "numeric"

tableChangeFluxTypes <- sampleFolder[grep("tableChangeFlux",sampleFolder)]
tableChangeFluxTypes <- read.csv(tableChangeFluxTypes)
typestableChangeFlux <- as.character(sapply(tableChangeFluxTypes, class))
typestableChangeFlux[7:8] <- "character"
typestableChangeFlux[3:6] <- "numeric"

tableResultsTypes <- sampleFolder[grep("tableResults",sampleFolder)]
tableResultsTypes <- read.csv(tableResultsTypes)
typestableResults <- as.character(sapply(tableResultsTypes, class))
typestableResults[7:8] <- "character"
typestableResults[2:6] <- "numeric"

tableFlowChangeTypes <- sampleFolder[grep("tableFlowChange",sampleFolder)]
tableFlowChangeTypes <- read.csv(tableFlowChangeTypes)
typestableFlowChange <- as.character(sapply(tableFlowChangeTypes, class))
typestableFlowChange[8:9] <- "character"
typestableFlowChange[3:6] <- "numeric"

flowStatisticsTypes <- sampleFolder[grep("flowStatistics",sampleFolder)]
flowStatisticsTypes <- read.csv(flowStatisticsTypes)
typesflowStatistics <- as.character(sapply(flowStatisticsTypes, class))
typesflowStatistics[2:17] <- "numeric"
typesflowStatistics[18:19] <- "character"

bootOut <- data.frame(matrix(NA, ncol=length(typesBootOut)+3))
names(bootOut) <- c(names(tableFlowChangeTypes),"bias1","bias2","bias3",
                    "residualModeSkewness","residualMedianSkewness","extrapolationMetric")
bootOut <- na.omit(bootOut)

tableChangeConc <- data.frame(matrix(NA, ncol=length(typestableChangeConc)))
names(tableChangeConc) <- names(tableChangeConcTypes)
tableChangeConc <- na.omit(tableChangeConc)

tableChangeFlux <- data.frame(matrix(NA, ncol=length(typestableChangeFlux)))
names(tableChangeFlux) <- names(tableChangeFluxTypes)
tableChangeFlux <- na.omit(tableChangeFlux)

tableFlowChange <- data.frame(matrix(NA, ncol=length(typestableFlowChange)))
names(tableFlowChange) <- names(tableFlowChangeTypes)
tableFlowChange <- na.omit(tableFlowChange)

tableResults <- data.frame(matrix(NA, ncol=length(typestableResults)))
names(tableResults) <- names(tableResultsTypes)
tableResults <- na.omit(tableResults)

flowStatistics <- data.frame(matrix(NA, ncol=length(typesflowStatistics)))
names(flowStatistics) <- names(flowStatisticsTypes)
flowStatistics <- na.omit(flowStatistics)


eList <- readRDS(sampleFolder[grep("eList",sampleFolder)])

INFO <- eList$INFO

unlink(sampleFolder)


for(i in a){
  folder <- unzip(i)
  
  eList <- readRDS(folder[grep("eList",folder)])
  
  INFO <- rbind(INFO, eList$INFO)
  
  Sample <- eList$Sample
  Daily <- eList$Daily
  
  residuals <- log(Sample$ConcHigh)-Sample$yHat
  bootOutPath <- folder[grep("bootOut",folder)]
  bootOutTemp <- read.csv(bootOutPath, colClasses=typesBootOut)

  bootOutTemp$residualModeSkewness <- (mean(residuals) - Mode(residuals))/sd(residuals)
  bootOutTemp$residualMedianSkewness <- 3*(mean(residuals) - median(residuals))/sd(residuals)
  bootOutTemp$extrapolationMetric <- max(Daily$ConcDay, na.rm = TRUE)/(2* max(Sample$ConcHigh, na.rm = TRUE))
  
  flowStatisticsTemp <- read.csv(folder[grep("flowStatistics",folder)], colClasses=typesflowStatistics)
  flowStatistics <- rbind(flowStatistics, flowStatisticsTemp)
  
  tableResultsTemp <- read.csv(folder[grep("tableResults",folder)], colClasses=typestableResults)
  tableResults <- rbind(tableResults, tableResultsTemp)
  
  tableFlowChangeTemp <- read.csv(folder[grep("tableFlowChange",folder)], colClasses=typestableFlowChange)
  tableFlowChange <- rbind(tableFlowChange, tableFlowChangeTemp)
  
  tableChangeFluxTemp <- read.csv(folder[grep("tableChangeFlux",folder)], colClasses=typestableChangeFlux)
  tableChangeFlux <- rbind(tableChangeFlux, tableChangeFluxTemp)
  
  tableChangeConcTemp <- read.csv(folder[grep("tableChangeConc",folder)], colClasses=typestableChangeConc)
  tableChangeConc <- rbind(tableChangeConc, tableChangeConcTemp)
  
  biasOut <- fluxBiasStat(eList$Sample)
  bootOutTemp$bias1 <- rep(as.numeric(biasOut[1]), nrow(bootOutTemp))
  bootOutTemp$bias2 <- rep(as.numeric(biasOut[2]), nrow(bootOutTemp))
  bootOutTemp$bias3 <- rep(as.numeric(biasOut[3]), nrow(bootOutTemp))
  
  bootOut <- rbind(bootOut, bootOutTemp)
  
  unlink(folder)
}

write.csv(INFO, file="INFO.csv", row.names=FALSE)

goodJobs <- INFO

totalJobs <- readRDS("D:/LADData/NAWQATrends/CondorPractice/infoData.RData")

common_cols <- intersect(colnames(INFO), colnames(totalJobs))

library(dplyr)
notFinished <- anti_join(totalJobs[,common_cols], goodJobs[,common_cols])
write.csv(notFinished, file="notFinished.csv", row.names=FALSE)

bootOut <- bootOut[!duplicated(bootOut),]
tableChangeConc <- tableChangeConc[!duplicated(tableChangeConc),]
tableChangeFlux <- tableChangeFlux[!duplicated(tableChangeFlux),]
tableFlowChange <- tableFlowChange[!duplicated(tableFlowChange),]
tableResults <- tableResults[!duplicated(tableResults),]
flowStatistics <- flowStatistics[!duplicated(flowStatistics),]

saveRDS(notFinished, file="notFinished.rds")
saveRDS(goodJobs, file="goodJobs.rds")
saveRDS(bootOut, file="bootOut.rds")
saveRDS(tableChangeConc, file="tableChangeConc.rds")
saveRDS(tableChangeFlux, file="tableChangeFlux.rds")
saveRDS(tableFlowChange, file="tableFlowChange.rds")
saveRDS(tableResults, file="tableResults.rds")
saveRDS(flowStatistics, file="flowStatistics.rds")

write.csv(bootOut, file="bootOut.csv", row.names=FALSE)
write.csv(tableChangeConc, file="tableChangeConc.csv", row.names=FALSE)
write.csv(tableChangeFlux, file="tableChangeFlux.csv", row.names=FALSE)
write.csv(tableFlowChange, file="tableFlowChange.csv", row.names=FALSE)
write.csv(tableResults, file="tableResults.csv", row.names=FALSE)
write.csv(flowStatistics, file="flowStatistics.csv", row.names=FALSE)
