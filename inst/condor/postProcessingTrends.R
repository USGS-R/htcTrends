install.packages("sbtools", repos = "http://owi.usgs.gov/R")
install.packages("EGRETci", repos = "http://owi.usgs.gov/R")

library(sbtools)
library(EGRET)
library(EGRETci)

#Change to your email:
source("D:/LADData/RCode/htcTrends/inst/shiny/config.R")

site <- "01112900"
param <- "Ammonia"

# site <- "01122610"
# param <- "OrthoP_F"

x <- query_item_identifier(scheme='naqwa', type = 'data', key = paste(param,site,sep="_"))

tempDir <- tempdir()
item_file_download(x$id, names='eList.RData',
                   destinations = file.path(tempDir,'eList.RData'), 
                   session=session, overwrite_file=TRUE)
load(file.path(tempDir,'eList.RData'))

pdf("diffContour.pdf")
plotDiffContours(eList, 1982, 2012, maxDiff = 1.5, 
                 qBottom = 50, qTop = 5000)
dev.off()
shell.exec("diffContour.pdf")

item_file_download(x$id, names='trend_82_12.RData',
                   destinations = file.path(tempDir,'trend_82_12.RData'), 
                   session=session, overwrite_file=TRUE)
load(file.path(tempDir,'trend_82_12.RData'))

pdf("fluxTrend.pdf")
plotHistogramTrend(eList, eBoot, caseSetUp)
dev.off()
shell.exec("fluxTrend.pdf")

# Quick comparisons:
site <- c("01112900", "01118500", "01122610")
param <- "Total Nitrogen"

pdf("Compare3.pdf")
par(mfcol=c(3,1))
for (i in site){
  x <- query_item_identifier('naqwa', 'data', 
                             paste(param,i,sep="_"), 
                             session=session)
  
  tempDir <- tempdir()
  item_file_download(x$id, names='eList.RData',
                     destinations = file.path(tempDir,'eList.RData'), 
                     session=session, overwrite_file=TRUE)
  load(file.path(tempDir,'eList.RData'))

  plotFluxHist(eList, yearStart = 1992, yearEnd = 2012)
  
}
dev.off()
shell.exec("Compare3.pdf")


