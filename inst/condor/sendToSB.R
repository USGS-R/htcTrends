library(sbtools)
library(usgsEGRET)
source("D:/LADData/RCode/htcTrends/inst/condor/auth.R")
topFolderID <- "5679a0e9e4b0da412f4fc2b7" #Phase II

infoDataTotal <- readRDS("D:/LADData/RCode/htcTrends/inst/condor/infoData.rds")
sampleDataTotal <- readRDS("D:/LADData/RCode/htcTrends/inst/condor/sampleData.rds")
flowDataTotal <- readRDS("D:/LADData/RCode/htcTrends/inst/condor/flowData.rds")

tempFolder <- tempdir()
setwd("D:/LADData/RCode/htcTrends/inst/extdata/resultsCI")
a <- list.files(pattern = "*.zip")

for(i in 2:nrow(infoDataTotal)){
  id <- paste(infoDataTotal$paramShortName[i], infoDataTotal$Site_no[i],sep="_")
  x <- query_item_in_folder(id, "5679a0e9e4b0da412f4fc2b7")
  sampleFolder <- unzip(a[i])
  CI <- readRDS("./CIAnnualResults.rds")
  item_append_files(x$id, sampleFolder)
}