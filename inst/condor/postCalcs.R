setwd("D:/LADData/RCode/htcTrends/inst/extdata/results")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

a <- list.files(pattern = "*.zip")

sampleFolder <- unzip(a[1])
bootOutFile <- sampleFolder[grep("bootOut",sampleFolder)]
bootOut <- read_csv(bootOutFile)

tableChangeConc <- read_csv(sampleFolder[grep("tableChangeConc",sampleFolder)],
                            col_types = list(col_integer(), col_integer(), 
                                             col_number(),col_number(),col_number(),col_number(),
                                             col_character(),col_character()))


tableChangeFlux <- read_csv(sampleFolder[grep("tableChangeFlux",sampleFolder)],
                            col_types = list(col_integer(), col_integer(), 
                                             col_number(),col_number(),col_number(),col_number(),
                                             col_character(),col_character()))

tableResults <- read_csv(sampleFolder[grep("tableResults",sampleFolder)],
                            col_types = list(col_integer(), 
                                             col_number(),col_number(),col_number(),col_number(),col_number(),
                                             col_character(),col_character()))

tableFlowChange <- read_csv(sampleFolder[grep("tableFlowChange",sampleFolder)],
                         col_types = list(col_integer(), col_integer(),
                                          col_number(),col_number(),col_number(),col_number(),
                                          col_character(),col_character(),col_character()))

flowStatistics <- read_csv(sampleFolder[grep("flowStatistics",sampleFolder)],
                            col_types = list(col_integer(), 
                                             col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
                                             col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
                                             col_character(),col_character()))

eList <- readRDS(sampleFolder[grep("eList",sampleFolder)])

INFO <- eList$INFO
Sample <- eList$Sample
Daily <- eList$Daily

biasOut <- fluxBiasStat(eList$Sample)
bootOut$bias1 <- rep(as.numeric(biasOut[1]), nrow(bootOut))
bootOut$bias2 <- rep(as.numeric(biasOut[2]), nrow(bootOut))
bootOut$bias3 <- rep(as.numeric(biasOut[3]), nrow(bootOut))

residuals <- log(Sample$ConcHigh)-Sample$yHat
bootOut$residualModeSkewness <- (mean(residuals) - Mode(residuals))/sd(residuals)
bootOut$residualMedianSkewness <- 3*(mean(residuals) - median(residuals))/sd(residuals)
bootOut$extrapolationMetric <- max(Daily$ConcDay, na.rm = TRUE)/(2* max(Sample$ConcHigh, na.rm = TRUE))

unlink(sampleFolder)

for(i in a[-1]){
  folder <- unzip(i)
  if(!is.null(folder[grep("eList",folder)])){
    eList <- readRDS(folder[grep("eList",folder)])
    
    INFO <- rbind(INFO, eList$INFO)
    
    Sample <- eList$Sample
    Daily <- eList$Daily
    
    residuals <- log(Sample$ConcHigh)-Sample$yHat
    bootOutTemp <- read_csv(folder[grep("bootOut",folder)])
    
    biasOut <- fluxBiasStat(eList$Sample)
    bootOutTemp$bias1 <- rep(as.numeric(biasOut[1]), nrow(bootOutTemp))
    bootOutTemp$bias2 <- rep(as.numeric(biasOut[2]), nrow(bootOutTemp))
    bootOutTemp$bias3 <- rep(as.numeric(biasOut[3]), nrow(bootOutTemp))
    
    bootOutTemp$residualModeSkewness <- (mean(residuals) - Mode(residuals))/sd(residuals)
    bootOutTemp$residualMedianSkewness <- 3*(mean(residuals) - median(residuals))/sd(residuals)
    bootOutTemp$extrapolationMetric <- max(Daily$ConcDay, na.rm = TRUE)/(2* max(Sample$ConcHigh, na.rm = TRUE))
    bootOut <- rbind(bootOut, bootOutTemp)
  
    flowStatistics <- rbind(flowStatistics, read_csv(folder[grep("flowStatistics",folder)],
                                                     col_types = list(col_integer(), 
                                                                      col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
                                                                      col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
                                                                      col_character(),col_character())))
    
    tableResults <- rbind(tableResults, read_csv(folder[grep("tableResults",folder)],
                                                 col_types = list(col_integer(), 
                                                                  col_number(),col_number(),col_number(),col_number(),col_number(),
                                                                  col_character(),col_character())))
    
    tableFlowChange <- rbind(tableFlowChange, read_csv(folder[grep("tableFlowChange",folder)],
                                                       col_types = list(col_integer(), col_integer(),
                                                                        col_number(),col_number(),col_number(),col_number(),
                                                                        col_character(),col_character(),col_character())))
    
    tableChangeFlux <- rbind(tableChangeFlux, read_csv(folder[grep("tableChangeFlux",folder)],
                                                       col_types = list(col_integer(), col_integer(), 
                                                                        col_number(),col_number(),col_number(),col_number(),
                                                                        col_character(),col_character())))
    
    tableChangeConc <- rbind(tableChangeConc, read_csv(folder[grep("tableChangeConc",folder)],
                                                       col_types = list(col_integer(), col_integer(), 
                                                                        col_number(),col_number(),col_number(),col_number(),
                                                                        col_character(),col_character())))
  }
  unlink(folder)
}

bootOut <- bootOut[!duplicated(bootOut),]
tableChangeConc <- tableChangeConc[!duplicated(tableChangeConc),]
tableChangeFlux <- tableChangeFlux[!duplicated(tableChangeFlux),]
tableFlowChange <- tableFlowChange[!duplicated(tableFlowChange),]
tableResults <- tableResults[!duplicated(tableResults),]
flowStatistics <- flowStatistics[!duplicated(flowStatistics),]

saveRDS(bootOut, file="bootOut.rds")
saveRDS(tableChangeConc, file="tableChangeConc.rds")
saveRDS(tableChangeFlux, file="tableChangeFlux.rds")
saveRDS(tableFlowChange, file="tableFlowChange.rds")
saveRDS(tableResults, file="tableResults.rds")
saveRDS(flowStatistics, file="flowStatistics.rds")

write.csv(INFO, file="INFO.csv", row.names=FALSE)
write.csv(bootOut, file="bootOut.csv", row.names=FALSE)
write.csv(tableChangeConc, file="tableChangeConc.csv", row.names=FALSE)
write.csv(tableChangeFlux, file="tableChangeFlux.csv", row.names=FALSE)
write.csv(tableFlowChange, file="tableFlowChange.csv", row.names=FALSE)
write.csv(tableResults, file="tableResults.csv", row.names=FALSE)
write.csv(flowStatistics, file="flowStatistics.csv", row.names=FALSE)
