library(readr)
library(lubridate)
path <- "D:/LADData/RCode/htcTrends/inst/extdata"

infoFile <- "Nut2_INFO_12222015b.csv"

infoDataTotal <- read_csv(file.path(path,infoFile))
infoDataTotal$blank_start1 <- parse_date(infoDataTotal$blank_start1, format = "%m/%d/%Y")
infoDataTotal$blank_end1 <- parse_date(infoDataTotal$blank_end1, format = "%m/%d/%Y")
infoDataTotal$blank_start2 <- parse_date(infoDataTotal$blank_start2, format = "%m/%d/%Y")
infoDataTotal$blank_end2 <- parse_date(infoDataTotal$blank_end2, format = "%m/%d/%Y")
infoDataTotal$blank_start3 <- parse_date(infoDataTotal$blank_start3, format = "%m/%d/%Y")
infoDataTotal$blank_end3 <- parse_date(infoDataTotal$blank_end3, format = "%m/%d/%Y")

moreLogic <- c("trend_72_12","trend_82_12","trend_92_12","trend_02_12")
infoDataTotal[,moreLogic] <- sapply(infoDataTotal[,moreLogic], function(x) x == "Y")


dataFile <- "Nut2_DATA_12222015b.csv"
sampleDataTotal <- read_csv(file.path(path,dataFile))
sampleDataTotal$date <- as.Date(parse_date_time(sampleDataTotal$date, c("%m/%d/%Y","%Y-%m-%d")))

saveRDS(infoDataTotal, file="infoData.rds")
saveRDS(sampleDataTotal, file="sampleData.rds")


tools::write_PACKAGES("inst/condor/packages/src/contrib",type="source", verbose=TRUE)
