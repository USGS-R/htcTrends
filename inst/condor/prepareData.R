library(readr)
library(lubridate)
path <- "D:/LADData/RCode/htcTrends/inst/extdata"

infoFile <- "CIDA_trends_info.csv"

infoDataTotal <- read_csv(file.path(path,infoFile))
infoDataTotal$blank_start1 <- parse_date(infoDataTotal$blank_start1, format = "%m/%d/%Y")
infoDataTotal$blank_end1 <- parse_date(infoDataTotal$blank_end1, format = "%m/%d/%Y")
infoDataTotal$blank_start2 <- parse_date(infoDataTotal$blank_start2, format = "%m/%d/%Y")
infoDataTotal$blank_end2 <- parse_date(infoDataTotal$blank_end2, format = "%m/%d/%Y")
infoDataTotal$blank_start3 <- parse_date(infoDataTotal$blank_start3, format = "%m/%d/%Y")
infoDataTotal$blank_end3 <- parse_date(infoDataTotal$blank_end3, format = "%m/%d/%Y")

moreLogic <- c("trend_72_12","trend_82_12","trend_92_12","trend_02_12")
infoDataTotal[,moreLogic] <- sapply(infoDataTotal[,moreLogic], function(x) x == "Y")


dataFile <- "CIDA_trends_data.csv"
sampleDataTotal <- read_csv(file.path(path,dataFile))
sampleDataTotal$date <- as.Date(parse_date_time(sampleDataTotal$date, c("%m/%d/%Y","%Y-%m-%d")))

saveRDS(infoDataTotal, file="infoData.rds")
saveRDS(sampleDataTotal, file="sampleData.rds")


tools::write_PACKAGES("D:/LADData/RCode/htcTrends/inst/condor/packages/src/contrib",type="source", verbose=TRUE)

sampleSite <- unique(infoDataTotal$Site_no)
flowData <- data.frame(dateTime = as.Date(NA), value=as.numeric(NA), site=as.character(NA))
flowData <- flowData[!is.na(flowData$value),]

for(j in sampleSite){
  flowFile <- paste0("Q_",j,".csv") 
  totalPathFlow <- file.path(path,flowFile)
  if(file.exists(totalPathFlow)){
    flowDataTotal <- read_csv(totalPathFlow, 
                              col_types = list(col_character(), col_character(), col_character(),
                                               col_date(),col_number(), col_number()))
  } else {
    cat(j,"\n")
  }
  
  subFlow <- flowDataTotal[,c("start_date","flow_gage")]
  subFlow$site <- rep(j, nrow(subFlow))
  names(subFlow) <- c("dateTime","value","site")
  subFlow <- subFlow[order(subFlow$dateTime),]
  flowData <- rbind(flowData, subFlow)
}
saveRDS(flowData, file="flowData.rds")
