library(EGRET)
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
    logScale = as.logical(as.integer(input$logScaleFlow))

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
    # setPNG()
    flowPlotStuff()
    # dev.off()    
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
    # setPNG()
    dataPlotStuff()
    # dev.off()  
  })

  modelPlotStuff <- reactive({
    
    eList <- eList()
    
    if(!is.list(eList)){
      eList <- eList_onLoad
    }
    
    if(is.null(input$date1)){
      date1 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01")
    } else {
      date1 = input$date1
    }
    
    if(is.null(input$date2)){
      date2 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01")
    } else {
      date2 = input$date2
    }
    
    if(is.null(input$date3)){
      date3 = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01")
    } else {
      date3 = input$date3
    }
    
    if(is.null(input$qLow)){
      qLow = round(quantile(eList$Daily$Q, probs = 0.1),digits = 1)
    } else {
      qLow = input$qLow
    }
    
    if(is.null(input$qHigh)){
      qHigh = round(quantile(eList$Daily$Q, probs = 0.9),digits = 1)
    } else {
      qHigh = input$qHigh
    }
    
    if(is.null(input$qMid)){
      qMid = round(quantile(eList$Daily$Q, probs = 0.5),digits = 1)
    } else {
      qMid = input$qMid
    }
    
    if(is.null(input$qUnit)){
      qUnit = 1
    } else {
      qUnit = as.integer(input$qUnit)
    }
    
    if(is.null(input$logScaleModel)){
      logScale = FALSE
    } else {
      logScale = input$logScaleModel
    }
    
    if(is.null(input$fluxUnit)){
      fluxUnit = 3
    } else {
      fluxUnit = as.integer(input$fluxUnit)
    }
    
    if(is.null(input$centerDate)){
      centerDate = "04-01"
    } else {
      centerDate = input$centerDate
    }
    
    if(is.null(input$yearRange)){
      yearStart = ceiling(min(eList$Daily$DecYear))
      yearEnd = floor(max(eList$Daily$DecYear))
    } else {
      yearStart = as.integer(input$yearRange[1])
      yearEnd = as.integer(input$yearRange[2])
    }
    
#     if(is.null(input$yearEnd)){
#       yearEnd = floor(max(eList$Daily$DecYear))
#     } else {
#       yearEnd = as.integer(input$yearEnd)
#     }
    
    if(is.null(input$maxDiff)){
      maxDiff = diff(range(eList$Sample$ConcAve))
    } else {
      maxDiff = round(as.numeric(input$maxDiff),digits = 3)
    }
    
    contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
    
    if(is.null(input$from)){
      from <- contours[1]
    } else {
      from = as.numeric(input$from)
    }
    
    if(is.null(input$to)){
      to <- contours[length(contours)]
    } else {
      to = as.numeric(input$to)
    }
    
    if(is.null(input$by)){
      by <- 5
    } else {
      by = as.integer(input$by)
    }

    switch(input$modelPlots,
           "plotConcTimeDaily" = plotConcTimeDaily(eList),
           "plotFluxTimeDaily" = plotFluxTimeDaily(eList, fluxUnit=fluxUnit),
           "plotConcPred" = plotConcPred(eList, logScale = logScale),
           "plotFluxPred" = plotFluxPred(eList, fluxUnit=fluxUnit),
           "plotResidPred" = plotResidPred(eList),
           "plotResidQ" = plotResidQ(eList, qUnit=qUnit),
           "plotResidTime" = plotResidTime(eList),
           "boxResidMonth" = boxResidMonth(eList),
           "boxConcThree" = boxConcThree(eList),
           "plotConcHist" = plotConcHist(eList),
           "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
           "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,qLow=qLow,qHigh=qHigh),
           "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                     centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
           "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit),
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
             "plotResidPred" = plotResidPred(eList),
             "plotResidQ" = plotResidQ(eList, qUnit=qUnit),
             "plotResidTime" = plotResidTime(eList),
             "boxResidMonth" = boxResidMonth(eList),
             "boxConcThree" = boxConcThree(eList),
             "plotConcHist" = plotConcHist(eList),
             "plotFluxHist" = plotFluxHist(eList, fluxUnit=fluxUnit),
             "plotConcQSmooth" = plotConcQSmooth(eList, date1=date1,date2=date2, date3=date3,qLow=qLow,qHigh=qHigh),
             "plotConcTimeSmooth" = plotConcTimeSmooth(eList, q1=qLow, q2=qMid, q3=qHigh, logScale = logScale,
                                                       centerDate=centerDate,yearStart=yearStart, yearEnd=yearEnd),
             "fluxBiasMulti" = fluxBiasMulti(eList, fluxUnit=fluxUnit, qUnit=qUnit),
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
  
  output$from <- renderUI({
    if(input$modelPlots %in% c("plotContours")){
      eList <- eList()
      contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
      numericInput("from", label = h5("From"), value = contours[1])
    }
  })  
  
  output$to <- renderUI({
    if(input$modelPlots %in% c("plotContours")){
      eList <- eList()
      contours <- pretty(c(min(eList$surfaces[,,3]), max(eList$surfaces[,,3])), n=5)
      numericInput("to", label = h5("To"), value = contours[length(contours)])
    }
  })
  
  output$by <- renderUI({
    if(input$modelPlots %in% c("plotContours")){
      numericInput("by", label = h5("Number of divisions"), value = 5)
    }
  })
  
  output$downloadModelPlot <- downloadHandler(
    
    filename = function() {
      paste(input$modelPlots, "pdf", sep = ".")
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
    
    if(is.na(eList$Sample)){
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else if(nrow(eList$Sample) == 0) {
      HTML(paste0("<h4>","No water quality data", "</h4>"))
    } else {
      HTML("")
    }
  })
  
  output$flowLog <- renderUI({
    if(input$flowPlots == "plotQTimeDaily"){
      radioButtons("logScaleFlow", label = h4("Scale"),
                   choices = list("Linear" = 0, "Log" = 1), 
                   selected = 0)
    }
  })
  
  output$maxDiff <- renderUI({
    if(input$modelPlots %in% c("plotDiffContours")){
      eList <- eList()
      numericInput("maxDiff", label = h5("maxDiff"), value = diff(range(eList$Sample$ConcAve)))
    }
  })

  observe({
    eList <- eList()
    updateSliderInput(session, "yearRange", 
                      min = ceiling(min(eList$Daily$DecYear)), max = floor(max(eList$Daily$DecYear)))
  })
  
  output$date1 <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      eList <- eList()
      dateInput("date1", label = h5("date1"), 
                value = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.1), origin="1970-01-01"))
    }
  })
  
  output$date2 <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      eList <- eList()
      dateInput("date2", label = h5("date2"), 
                value = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.5), origin="1970-01-01"))
    }
  })
  
  output$date3 <- renderUI({
    if(input$modelPlots == "plotConcQSmooth"){
      eList <- eList()
      dateInput("date3", label = h5("date3"), 
                value = as.Date(quantile(eList$Daily$Date, type=1, probs = 0.9), origin="1970-01-01"))
    }
  })

  output$qLow <- renderUI({
    if(input$modelPlots %in% c("plotConcQSmooth","plotConcTimeSmooth","plotContours","plotDiffContours")){
      eList <- eList()
      qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
      qFactor <- qFactor@qUnitFactor
      numericInput("qLow", label = h5("qLow"), value = round(qFactor * quantile(eList$Daily$Q, probs = 0.1),digits = 1))
    }
  })
  
  output$qHigh <- renderUI({
    if(input$modelPlots %in% c("plotConcQSmooth","plotConcTimeSmooth","plotContours","plotDiffContours")){
      eList <- eList()
      qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
      qFactor <- qFactor@qUnitFactor
      numericInput("qHigh", label = h5("qHigh"), value = round(qFactor * quantile(eList$Daily$Q, probs = 0.9),digits = 1))
    }
  })
  
  output$qMid <- renderUI({
    if(input$modelPlots %in% c("plotConcQSmooth","plotConcTimeSmooth")){
      eList <- eList()
      qFactor <- qConst[shortCode=as.integer(input$qUnit)][[1]]
      qFactor <- qFactor@qUnitFactor
      numericInput("qMid", label = h5("qMid"), value = round(qFactor * quantile(eList$Daily$Q, probs = 0.5),digits = 1))
    }
  })


  output$flowCode <- renderPrint({

    stat = as.integer(input$flowStat)
    qUnit = as.integer(input$qUnit)
    paStart = as.integer(which(month.name == input$paStart))
    paLong = as.integer(input$paLong)
    logScale = as.logical(as.integer(input$logScaleFlow))
    
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
    qLow = input$qLow
    qHigh = input$qHigh
    qMid = input$qMid
    centerDate = input$centerDate
    yearStart = as.integer(input$yearRange[1])
    yearEnd = as.integer(input$yearRange[2])
    maxDiff = as.integer(input$maxDiff)
    from = as.numeric(input$from)
    to = as.numeric(input$to)
    by = as.integer(input$by)

    outText <- switch(input$modelPlots,
                      "plotConcTimeDaily" = paste0("plotConcTimeDaily(eList)"),
                      "plotFluxTimeDaily" = paste0("plotFluxTimeDaily(eList, fluxUnit = ", fluxUnit),
                      "plotConcPred" = paste0("plotConcPred(eList)"),
                      "plotFluxPred" = paste0("plotFluxPred(eList, fluxUnit = ", fluxUnit, ")"),
                      "plotResidPred" = paste0("plotResidPred(eList)"),
                      "plotResidQ" = paste0("plotResidQ(eList, qUnit = ", qUnit, ")"),
                      "plotResidTime" = paste0("plotResidTime(eList)"),
                      "boxResidMonth" = paste0("boxResidMonth(eList)"),
                      "boxConcThree" = paste0("boxConcThree(eList)"),
                      "plotConcHist" = paste0("plotConcHist(eList)"),
                      "plotFluxHist" = paste0("plotFluxHist(eList, fluxUnit = ", fluxUnit, ")"),
                      "plotConcQSmooth" = paste0("plotConcQSmooth(eList, date1 = '",date1, "', date2 = '",
                                                 date2,"', date3 = '",date3, "', qLow = ",qLow,", qHigh = ",qHigh,")"),
                      "plotConcTimeSmooth" = paste0("plotConcTimeSmooth(eList, q1 = ",qLow,
                                                    ", q2 = ",qMid, ", q3 = ",qHigh, ", yearStart = ",
                                                    yearStart,", yearEnd = ",yearEnd,", centerDate = ",centerDate,")"),
                      "fluxBiasMulti" = paste0("fluxBiasMulti(eList, qUnit = ", qUnit,", fluxUnit = ", fluxUnit,")"),
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
    if(is.null(input$paramList)){
      paramList = "All"
    } else {
      paramList = input$paramList
    }
    
    if(is.null(input$trendTime)){
      trendTime = "All"
    } else {
      trendTime = input$trendTime
    }
    
    if(is.null(input$up)){
      up = "Up"
    } else {
      up = input$up
    }
    
    if(is.null(input$flux)){
      fluxOrConc = "Conc"
    } else {
      fluxOrConc = input$flux
    }
    
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

  observe({
    eList <- eList()
    INFO <- eList$INFO
    updateTextInput(session, "inText", 
                    value = paste0("Data retrieved from sciencebase: ",INFO$station_nm))
  
  })

  
})
