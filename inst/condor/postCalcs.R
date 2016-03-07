library(readr)
library(EGRET)
setwd("D:/LADData/RCode/htcTrends/inst/extdata/results")

a <- list.files(pattern = "*.zip")
tempFiles <- unzip(a[1],list = TRUE)
tempFolder <- tempdir()

sampleFolder <- unzip(a[1],exdir = tempFolder)
bootOutFile <- sampleFolder[grep("bootOut",sampleFolder)]

bootOut <- read_csv( bootOutFile)
# 
# tableChangeConc <- read_csv(sampleFolder[grep("tableChangeConc",sampleFolder)],
#                             col_types = list(col_integer(), col_integer(),
#                                              col_number(),col_number(),col_number(),col_number(),
#                                              col_character(),col_character()))
# 
# 
# tableChangeFlux <- read_csv(sampleFolder[grep("tableChangeFlux",sampleFolder)],
#                             col_types = list(col_integer(), col_integer(),
#                                              col_number(),col_number(),col_number(),col_number(),
#                                              col_character(),col_character()))
# 
# tableResults <- read_csv(sampleFolder[grep("tableResults",sampleFolder)],
#                             col_types = list(col_integer(),
#                                              col_number(),col_number(),col_number(),col_number(),col_number(),
#                                              col_character(),col_character()))
# 
# tableFlowChange <- read_csv(sampleFolder[grep("tableFlowChange",sampleFolder)],
#                          col_types = list(col_integer(), col_integer(),
#                                           col_number(),col_number(),col_number(),col_number(),
#                                           col_character(),col_character(),col_character()))
# 
# flowStatistics <- read_csv(sampleFolder[grep("flowStatistics",sampleFolder)],
#                             col_types = list(col_integer(),
#                                              col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
#                                              col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
#                                              col_character(),col_character()))

file.remove(sampleFolder)

for(i in a[-1]){
  tempFiles <- unzip(i,list = TRUE)
  bootOutFile <- tempFiles[grep("bootOut",tempFiles$Name),1]
  if(length(nchar(bootOutFile)) > 0){
    folder <- unzip(i,exdir = tempFolder, files = bootOutFile)
    bootOut <- rbind(bootOut, read_csv( folder))
    file.remove(folder)
    # cat(i , "\n")
  } else {
    cat(i, "\n")
  }

}
#   flowStatistics <- rbind(flowStatistics, read_csv(folder[grep("flowStatistics",folder)],
#                                                    col_types = list(col_integer(),
#                                                                     col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
#                                                                     col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),col_number(),
#                                                                     col_character(),col_character())))
# 
#   tableResults <- rbind(tableResults, read_csv(folder[grep("tableResults",folder)],
#                                                col_types = list(col_integer(),
#                                                                 col_number(),col_number(),col_number(),col_number(),col_number(),
#                                                                 col_character(),col_character())))
# 
#   tableFlowChange <- rbind(tableFlowChange, read_csv(folder[grep("tableFlowChange",folder)],
#                                                      col_types = list(col_integer(), col_integer(),
#                                                                       col_number(),col_number(),col_number(),col_number(),
#                                                                       col_character(),col_character(),col_character())))
# 
#   tableChangeFlux <- rbind(tableChangeFlux, read_csv(folder[grep("tableChangeFlux",folder)],
#                                                      col_types = list(col_integer(), col_integer(),
#                                                                       col_number(),col_number(),col_number(),col_number(),
#                                                                       col_character(),col_character())))
# 
#   tableChangeConc <- rbind(tableChangeConc, read_csv(folder[grep("tableChangeConc",folder)],
#                                                      col_types = list(col_integer(), col_integer(),
#                                                                       col_number(),col_number(),col_number(),col_number(),
#                                                                       col_character(),col_character())))
#   file.remove(folder)
# }

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

library(sbtools)
source("auth.R")
# setwd("D:/LADData/RCode/htcTrends/inst/extdata/results")
item_replace_files("5684401ce4b0a04ef493313b", c("INFO.csv",
                                                 "bootOut.csv",
                                                 "tableChangeConc.csv",
                                                 "tableChangeFlux.csv",
                                                 "tableFlowChange.csv",
                                                 "tableResults.csv",
                                                 "flowStatistics.csv"))
item_replace_files("56844047e4b0a04ef493313e", c("bootOut.rds",
                                                 "tableChangeConc.rds",
                                                 "tableChangeFlux.rds",
                                                 "tableFlowChange.rds",
                                                 "tableResults.rds",
                                                 "flowStatistics.rds"))