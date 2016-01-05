library(EGRET)
library(EGRETci)
library(sbtools)
library(leaflet)
library(dplyr)
library(DT)

tempFolder <- tempdir()

source("auth.R")

#Raw Data
rawDataID <- "5683f4b1e4b0a04ef4927c36"
infoFile <- "infoData.rds"
item_file_download(rawDataID, names=infoFile,
                   destinations = file.path(tempFolder,infoFile), 
                   overwrite_file=TRUE)

genInfo <- readRDS(file.path(tempFolder,infoFile))

#Summary data:
summaryFolder <- "56844047e4b0a04ef493313e"
item_file_download(summaryFolder, names='bootOut.rds',
                   destinations = file.path(tempFolder,'bootOut.rds'), 
                   overwrite_file=TRUE)

bootOut <- readRDS(file.path(tempFolder,'bootOut.rds'))

topFolderID <- "5679a0e9e4b0da412f4fc2b7"

shinyServer(function(input, output, session) {
  
  eList_Start <- eventReactive(input$getData, {
    source("config.R")
    id <- idText()
    x <- query_item_identifier(type='naqwa', scheme = 'dataII', key = id)
    item_file_download(x$id, names="eList.rds",
                       destinations = file.path(tempFolder,"eList.rds"), 
                       overwrite_file=TRUE) 
    
    eList_Start <- readRDS(file.path(tempFolder,"eList.rds"))
  })
  
  rawBoot <- reactive({
    eList_Start <- eList_Start()    
    source("config.R")
    id <- idText()
    x <- query_item_identifier(type='naqwa', scheme = 'dataII', key = id)
    itemsInFolder <- item_list_files(x$id)
    trendsFile <- itemsInFolder$fname[grep(pattern = ".RData", itemsInFolder$fname)]
    item_file_download(x$id, names=trendsFile[1],
                       destinations = file.path(tempFolder,trendsFile[1]), 
                       overwrite_file=TRUE) 
    
    load(file.path(tempFolder,trendsFile[1]))
    
    eBoot
  })
  
  eList <- reactive({

    eList_Start <- eList_Start()      
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    eList <- setPA(eList_Start, paStart, paLong)
    
  })
  
  flowPlotStuff <- reactive({
    
    eList <- eList()
    stat = as.integer(input$flowStat)
    qUnit = as.integer(input$qUnit)
    logScale = input$logScaleFlow

    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat, qUnit = qUnit),
           "plotSDLogQ" = plotSDLogQ(eList),
           "plotQTimeDaily" = plotQTimeDaily(eList, qUnit = qUnit, logScale = logScale),
           "plotFour" = plotFour(eList, qUnit = qUnit),
           "plotFourStats" = plotFourStats(eList, qUnit = qUnit)
           
    )
    
    pdf("plot.pdf")
    switch(input$flowPlots,
           "plotFlowSingle" = plotFlowSingle(eList, istat=stat, qUnit = qUnit),
           "plotSDLogQ" = plotSDLogQ(eList),
           "plotQTimeDaily" = plotQTimeDaily(eList, qUnit = qUnit, logScale = logScale),
           "plotFour" = plotFour(eList, qUnit = qUnit),
           "plotFourStats" = plotFourStats(eList, qUnit = qUnit)
           
    )
    dev.off()
    
  })

  output$flowPlotsOut <- renderPlot({ 
    flowPlotStuff()
  })
  
  dataPlotStuff <- reactive({
    
    eList <- eList()
    qUnit = as.integer(input$qUnit)
    logScale = input$logScaleData

    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList, logScale = logScale),
           "boxQTwice" = boxQTwice(eList, qUnit = qUnit),
           "plotConcTime" = plotConcTime(eList, logScale = logScale),
           "plotConcQ" = plotConcQ(eList, qUnit = qUnit, logScale = logScale),
           "multiPlotDataOverview" = multiPlotDataOverview(eList, qUnit = qUnit)
           
    )
    
    pdf("plot.pdf")
    switch(input$dataPlots,
           "boxConcMonth" = boxConcMonth(eList, logScale = logScale),
           "boxQTwice" = boxQTwice(eList, qUnit = qUnit),
           "plotConcTime" = plotConcTime(eList, logScale = logScale),
           "plotConcQ" = plotConcQ(eList, qUnit = qUnit, logScale = logScale),
           "multiPlotDataOverview" = multiPlotDataOverview(eList, qUnit = qUnit)
           
    )
    dev.off()
    
  })
  
  output$dataPlotsOut <- renderPlot({ 
    dataPlotStuff()
  })
  
  trendPlotStuff <- reactive({
    
    eList <- eList()
    rawBoot <- rawBoot()
    INFO <- eList$INFO
    caseSetUp <- data.frame(year1=min(c(INFO$trend_72_12_start,
                                        INFO$trend_82_12_start,
                                        INFO$trend_92_12_start,
                                        INFO$trend_02_12_start),na.rm = TRUE),
                            year2 = INFO$trend_end,
                            nBoot=INFO$nBoot,
                            bootBreak = INFO$bootBreak,
                            blockLength = INFO$blockLength)
     
    switch(input$trendPlots,
           "plotHistogramTrendConc" = plotHistogramTrend(eList, rawBoot, caseSetUp, flux = FALSE),
           "plotHistogramTrendFlux" = plotHistogramTrend(eList, rawBoot, caseSetUp, flux = TRUE)
    )
    
    pdf("plot.pdf")
    switch(input$trendPlots,
           "plotHistogramTrendConc" = plotHistogramTrend(eList, rawBoot, caseSetUp, flux=FALSE),
           "plotHistogramTrendFlux" = plotHistogramTrend(eList, rawBoot, caseSetUp, flux = TRUE)
    )
    dev.off()
    
  })
  
  output$trendPlotsOut <- renderPlot({ 
    trendPlotStuff()
  })

  modelPlotStuff <- reactive({
    
    eList <- eList()

    date1 = input$date1
    date2 = input$date2
    date3 = input$date3
    qLow = input$flowRange[1]
    qHigh = input$flowRange[2]
    qMid = input$qMid
    qUnit = as.integer(input$qUnit)
    logScale = input$logScaleModel
    fluxUnit = as.integer(input$fluxUnit)
    centerDate = input$centerDate
    yearStart = as.integer(input$yearRange[1])
    yearEnd = as.integer(input$yearRange[2])
    maxDiff = input$maxDiff
    from = as.numeric(input$concRange[1])
    to = as.numeric(input$concRange[2])
    by = as.integer(input$by)+1
    rResid <- input$rResid

    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = plotConcPred(eList, logScale = logScale),
           "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
           "plotResidPred" = plotResidPred(eList, rResid = rResid),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit, rResid = rResid),
           "plotResidTime" = plotResidTime(eList, rResid = rResid),
           "boxResidMonth" = boxResidMonth(eList, rResid = rResid),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,qLow=qLow,qHigh=qHigh),
           "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                     centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit, rResid = rResid),
           "plotContours" = plotContours(eList, qUnit=qUnit,yearStart = yearStart, yearEnd = yearEnd,
                                         qBottom = qLow, qTop=qHigh,contourLevels = seq(from, to, length.out =by)),
           "plotDiffContours" = plotDiffContours(eList, year0=yearStart,year1 = yearEnd, maxDiff = maxDiff,
                                                 qUnit=qUnit,qBottom = qLow, qTop=qHigh)
    )
    
    pdf("plot.pdf")
      switch(input$modelPlots,
             "plotConcTimeDaily" = plotConcTimeDaily(eList),
             "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
             "plotConcPred" = plotConcPred(eList, logScale = logScale),
             "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
             "plotResidPred" = plotResidPred(eList, rResid = rResid),
             "plotResidQ" = plotResidQ(eList, qUnit=qUnit, rResid = rResid),
             "plotResidTime" = plotResidTime(eList, rResid = rResid),
             "boxResidMonth" = boxResidMonth(eList, rResid = rResid),
             "boxConcThree" = boxConcThree(eList),
             "plotConcHist" = plotConcHist(eList),
             "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
             "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,qLow=qLow,qHigh=qHigh),
             "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                       centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
             "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit, rResid = rResid),
             "plotContours" = plotContours(eList, qUnit=qUnit,yearStart = yearStart, yearEnd = yearEnd,
                                           qBottom = qLow, qTop=qHigh,contourLevels = seq(from, to, length.out =by)),
             "plotDiffContours" = plotDiffContours(eList, year0=yearStart,year1 = yearEnd, maxDiff = maxDiff,
                                                   qUnit=qUnit,qBottom = qLow, qTop=qHigh)
      )
    dev.off()
    
  })
  
  output$modelPlotsOut <- renderPlot({
    modelPlotStuff()
  })
  
  output$downloadModelPlot <- downloadHandler(
    
    filename = function() {
      paste(input$modelPlots, "pdf", sep = ".")
    },
    content = function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
  output$downloadTrendPlot <- downloadHandler(
    
    filename = function() {
      paste(input$trendPlots, "pdf", sep = ".")
    },
    content = function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
  output$downloadDataPlot <- downloadHandler(
    filename = function() {
      paste(input$dataPlots, "pdf", sep = ".")
    },
    content = function(file) {
      
      file.copy("plot.pdf", file)
    }
  )
  
  output$downloadFlowPlot <- downloadHandler(
    filename = function() {
      paste(input$flowPlots, "pdf", sep = ".")
    },
    content = function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
  output$saveData <- downloadHandler(
    filename = function() {
      paste("eList", "rds", sep = ".")
    },
    content = function(file) {
      
      file.copy(file.path(tempFolder,"eList.rds"), file)
    }
  )
  
  output$SampleText <- renderUI({
    
    eList <- eList()
    
    if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  output$metaData <- DT::renderDataTable({
    
    eList <- eList()
    
    INFO <- eList$INFO
    
    flippedTable <- data.frame(t(INFO[,names(INFO) %in% c("station_nm","site_no","agency_cd",
                                         "dec_lat_va","dec_long_va","tz_cd",
                                         "drainSqKm","shortName","param_nm",
                                         "param_units","param_nm",
                                         "paramNumber")]))
    
    DT::datatable(flippedTable, colnames = "",
                  options = list(pageLength = nrow(flippedTable)))
  })
  
  output$modelDataToChose <- DT::renderDataTable({
    genInfo <- choseData()
    legendTitle <- attr(genInfo, "legendTitle")
    
    genInfo <- genInfo[,c("param_nm","shortName","drainSqKm","colData")]
    
    names(genInfo) <- c("param_nm","shortName","drainSqKm",legendTitle)
    
    genInfoDT <- DT::datatable(genInfo, selection = "single")
    genInfoDT <- formatRound(genInfoDT, legendTitle, 2) 
    genInfoDT
    
  })
  
  output$modelText <- renderUI({
    
    eList <- eList()
    
    if(all(is.na(eList$Sample))){
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  observe({
    eList <- eList()
    updateNumericInput(session, "maxDiff", value = diff(range(eList$Sample$ConcAve)))
  })

  observe({
    eList <- eList()
    updateSliderInput(session, "yearRange", 
                      min = ceiling(min(eList$Daily$DecYear)), max = floor(max(eList$Daily$DecYear)))
  })
  
  observe({
    eList <- eList()
    contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
    updateSliderInput(session, "concRange", 
                      min = 0.5*contours[1], max = 2*contours[length(contours)])
  })
  
  observe({
    eList <- eList()
    qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
    qFactor <- qFactor@qUnitFactor
    updateSliderInput(session, "flowRange", 
                      min = 0.5*as.numeric(round(qFactor * quantile(eList$Daily$Q, probs = 0.1),digits = 1)),
                      max = 2*as.numeric(round(qFactor * quantile(eList$Daily$Q, probs = 0.9),digits = 1)))
  })    
  
  observe({
    eList <- eList()
    updateDateInput(session, "date1", value=as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01"))
  })
  
  observe({
    eList <- eList()    
    updateDateInput(session, "date2", value=as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01"))
  })
  
  observe({
    eList <- eList()   
    updateDateInput(session, "date3", value=as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01"))
  })

  observe({
    eList <- eList()
    qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
    qFactor <- qFactor@qUnitFactor
    updateNumericInput(session, "qMid", value = round(qFactor * quantile(eList$Daily$Q, probs = 0.5),digits = 1))
  })

  output$flowCode <- renderPrint({

    stat = as.integer(input$flowStat)
    qUnit = as.integer(input$qUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    logScale = input$logScaleFlow
    
    outText <- switch(input$flowPlots,
                      "plotFlowSingle" = paste0("plotFlowSingle(eList, istat=", stat,", qUnit = ", qUnit, ")"),
                      "plotSDLogQ" = paste0("plotSDLogQ(eList", ")"),
                      "plotQTimeDaily" = paste0("plotQTimeDaily(eList, logScale = ",logScale,", qUnit = ", qUnit, ")"),
                      "plotFour" = paste0("plotFour(eList, qUnit = ", qUnit, ")"),
                      "plotFourStats" = paste0("plotFourStats(eList, qUnit = ", qUnit, ")")
                      
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$trendCode <- renderPrint({
    eList <- eList()
    INFO <- eList$INFO
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    
    outText <- switch(input$trendPlots,
                      "plotHistogramTrendConc" = paste0("plotHistogramTrend(eList, eBoot, caseSetUp, flux=FALSE)"),
                      "plotHistogramTrendFlux" = paste0("plotHistogramTrend(eList, eBoot, caseSetUp, flux=TRUE)")
                      
    )
    
    HTML(paste0("caseSetUp <- data.frame(year1=", min(c(INFO$trend_72_12_start,
                                                        INFO$trend_82_12_start,
                                                        INFO$trend_92_12_start,
                                                        INFO$trend_02_12_start),na.rm = TRUE),",\n",
                                            "year2 = ", INFO$trend_end,",\n",
                                            "nBoot = ", INFO$nBoot,",\n",
                                            "bootBreak = ", INFO$bootBreak,",\n",
                                            "blockLength = ", INFO$blockLength, ")\n",
                "setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$getCode <- renderPrint({
    
    id <- idText()
    
    HTML(paste0("library(sbtools)\n",
                "authenticate()\n",
                "id <- '", id, "'\n",
                "tempFolder <- tempdir()\n",
                "x <- query_item_identifier(type='naqwa', scheme = 'dataII', key = id)\n",
                "item_file_download(x$id, names='eList.rds',\n",
                     "destinations = file.path(tempFolder,'eList.rds'),\n", 
                     "overwrite_file=TRUE)\n",
                "eList_Start <- readRDS(file.path(tempFolder,'eList.rds'))"))
    
  })
  
  output$dataCode <- renderPrint({

    qUnit = as.integer(input$qUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    logScale = input$logScaleData

    outText <- switch(input$dataPlots,
                      "boxConcMonth" = paste0("boxConcMonth(eList, logScale = ", logScale,")"),
                      "boxQTwice" = paste0("boxQTwice(eList, qUnit = ", qUnit, ")"),
                      "plotConcTime" = paste0("plotConcTime(eList, logScale = ", logScale,")"),
                      "plotConcQ" = paste0("plotConcQ(eList, logScale = ", logScale,", qUnit = ", qUnit, ")"),
                      "multiPlotDataOverview" = paste0("multiPlotDataOverview(eList, qUnit = ", qUnit, ")")
                      
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$modelCode <- renderPrint({
    
    eList <- eList()

    qUnit = as.integer(input$qUnit)
    fluxUnit = as.integer(input$fluxUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    date1 = input$date1
    date2 = input$date2
    date3 = input$date3
    qLow = input$flowRange[1]
    qHigh = input$flowRange[2]
    qMid = input$qMid
    centerDate = input$centerDate
    yearStart = as.integer(input$yearRange[1])
    yearEnd = as.integer(input$yearRange[2])
    maxDiff = as.integer(input$maxDiff)
    from = input$concRange[1]
    to = input$concRange[2]
    by = as.integer(input$by) + 1
    rResid = input$rResid

    outText <- switch(input$modelPlots,
                      "plotConcTimeDaily" = paste0("plotConcTimeDaily(eList)"),
                      "plotFluxTimeDaily" = paste0("plotFluxTimeDaily(eList, fluxUnit = ", fluxUnit),
                      "plotConcPred" = paste0("plotConcPred(eList)"),
                      "plotFluxPred" = paste0("plotFluxPred(eList, fluxUnit = ", fluxUnit, ")"),
                      "plotResidPred" = paste0("plotResidPred(eList, rResid = ", rResid, ")"),
                      "plotResidQ" = paste0("plotResidQ(eList, qUnit = ", qUnit, ", rResid = ", rResid, ")"),
                      "plotResidTime" = paste0("plotResidTime(eList, rResid = ", rResid, ")"),
                      "boxResidMonth" = paste0("boxResidMonth(eList, rResid = ", rResid, ")"),
                      "boxConcThree" = paste0("boxConcThree(eList)"),
                      "plotConcHist" = paste0("plotConcHist(eList)"),
                      "plotFluxHist" = paste0("plotFluxHist(eList, fluxUnit = ", fluxUnit, ")"),
                      "plotConcQSmooth" = paste0("plotConcQSmooth(eList, date1 = '",date1, "', date2 = '",
                                                 date2,"', date3 = '",date3, "', qLow = ",qLow,", qHigh = ",qHigh,")"),
                      "plotConcTimeSmooth" = paste0("plotConcTimeSmooth(eList, q1 = ",qLow,
                                                    ", q2 = ",qMid, ", q3 = ",qHigh, ", yearStart = ",
                                                    yearStart,", yearEnd = ",yearEnd,", centerDate = ",centerDate,")"),
                      "fluxBiasMulti" = paste0("fluxBiasMulti(eList, qUnit = ", qUnit,", fluxUnit = ", fluxUnit,", rResid = ", rResid, ")"),
                      "plotContours" = paste0("plotContours(eList, qUnit=", qUnit,", yearStart = ",yearStart,
                                              ", yearEnd = ",yearEnd,", qBottom = ",qLow,", qTop = ",qHigh,
                                              ", contourLevels = seq(",from,", ",to,", length.out=",by, "))"),
                      "plotDiffContours" = paste0("plotDiffContours(eList, qUnit=",qUnit,", year0=",yearStart,",year1 = ",
                                                  yearEnd,", qBottom = ",qLow,", qTop = ",qHigh, ", maxDiff = ",maxDiff,")")
    )
    
    HTML(paste0("setPA(eList, paStart = ",paStart, ", paLong = ", paLong,")\n",
                outText))
    
  })
  
  output$mymap <- leaflet::renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -99.5, lat = 40, zoom=4) 
    
  })

  choseData <- reactive({

    paramList = input$paramList
    trendTime = input$trendTime
    up = input$up
    fluxOrConc = input$flux
    
    if(paramList == "All"){
      subData <- genInfo
    } else {
      subData <- genInfo[genInfo$param_nm == paramList,]
    }
    
    subData <- suppressWarnings(left_join(subData, bootOut, by=c("Site_no", "param_nm")))
    
    if(trendTime == "All"){
      subData <- subData
    } else {
      
      subData <- switch(trendTime,
                        "1972-2012" = subData[subData$yearStart %in% c(1970:1975),],
                        "1982-2012" = subData[subData$yearStart %in% c(1980:1985),],
                        "1992-2012" = subData[subData$yearStart %in% c(1990:1995),],
                        "2002-2012" = subData[subData$yearStart %in% c(2000:2005),])
    }
    
    if(nrow(subData) > 0){
      
        subData <- subData[!is.na(subData$yearStart),]
        subData$ID <-  paste(as.character(subData$param_nm),subData$Site_no,sep="_")
        
        if(fluxOrConc == "Flux"){
          if(up == "Up"){
            subData$colData <- subData$likeFUp
            legendTitle <- "Probablity that\n Flux is Upwards"
          } else {
            subData$colData <- subData$likeFDown
            legendTitle <- "Probablity that\nFlux is Downwards"
          }
        } else {
          if(up == "Up"){
            subData$colData <- subData$likeCUp
            legendTitle <- "Probablity that\nConc is Upwards"
          } else {
            subData$colData <- subData$likeCDown
            legendTitle <- "Probablity that\nConc is Downwards"
          }
        }
        
        attr(subData,"legendTitle") <- legendTitle
    }
    
      subData
  })
  
  observe({
    
    subData <- choseData()
    col_types <- c("darkblue","dodgerblue","green","yellow","orange","red","brown")
    
    if(nrow(subData) > 0){
      
      legendTitle <- attr(subData,"legendTitle") 
      leg_vals <- unique(as.numeric(quantile(subData$colData, probs=c(0,0.01,0.1,0.25,0.5,0.75,0.9,.99,1), na.rm=TRUE)))
      
      if(length(leg_vals) > 1){
        pal = colorBin(col_types, subData$colData, bins = leg_vals)
      } else {
        pal = colorBin(col_types, subData$colData, bins = c(0,leg_vals,1))
      }
      
      leafletProxy("mymap", data=subData) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(lat=~dec_lat_va, lng=~dec_long_va, 
                         layerId = ~ID,
                   fillColor = ~pal(colData), 
                   popup = ~ID,
                   # weight=1,
                   radius=3,
                   stroke=FALSE,
                   # color = "black",
                   fillOpacity = 0.8, opacity = 0.8) %>%
        addLegend(
          position = 'bottomleft',
          pal=pal,
          values=~colData,
          opacity = 0.8,
          title = legendTitle)      
    } else {
      leafletProxy("mymap", data=subData) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearPopups() 
    }

  })
  
  idText <- reactive({
    
    if(input$tabs == "Table\n<i class=\"fa fa-bars\"></i>"){
      tableClick <- input$modelDataToChose_rows_selected
      if(is.null(tableClick)){
        return
      }
      genInfo <- choseData()
      text <- genInfo$ID[as.integer(tableClick[length(tableClick)])]      
    } else {
      click <- input$mymap_marker_click
      if(is.null(click)){
        return  
      }   
      text<-click$id   
    } 
    
    text
    
  })
  
  output$Click_text<-renderText({
    
    HTML(paste0("<h5>",idText(),"</h5>"))
    
  })

  output$textMessage <- renderUI({
    eList <- eList()
    INFO <- eList$INFO
    HTML(paste0("Data retrieved from sciencebase: ",INFO$station_nm))
  
  })

  
})
