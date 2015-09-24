shinyUI(
  fluidPage(
    h2("NAWQA Trends Exploration"),
    h4("Step 1: Explore the available data sets from the 'Choose Data' tab"),
    h4("Step 2: Choose a data set from either the map or table, and click the 'Get Data' button to retrieve from sciencebase"),
    h4("Step 3: Explore that data in the 'Analyze Data' tab"),
    h4("Step 4: Click on the Download Plot button for properly formatted pdf graph output"),
    htmlOutput("dataAvailable"),
    fluidRow(
      column(11,
             tabsetPanel(
               tabPanel("Choose Data",
                     fluidRow(
                       column(3,
                              uiOutput("paramList"),
                              selectInput("trendTime", label = "Trend Timeline", 
                                          choices = c("All","1972-2012","1982-2012","1992-2012","2002-2012"),
                                          multiple = FALSE),
                              radioButtons("flux", label = "", inline = TRUE,
                                           choices = c("Conc","Flux")),
                              radioButtons("up", label = "Trend", inline = TRUE,
                                           choices = c("Up", "Down")),
                              htmlOutput("Click_text"),
                              actionButton("getData", "Get Data"),
                              downloadButton('saveData', 'Save Data')),
                       column(9,
                              tabsetPanel(id = "tabs",
                                tabPanel("Map",
                                  leaflet::leafletOutput("mymap")),
                              tabPanel("Table",
                                       fluidRow(column(1),
                                                column(11,
                                                       DT::dataTableOutput('modelDataToChose')))
                              ))
                     ))),
               tabPanel("Analyze Data",
                        fluidRow(
                          column(3, h5("Period of Analysis:", align = "right", style = "margin-top: 40px")),
                          column(3,
                                 selectInput("paStart", label = "Starting Month", 
                                             choices = c(month.name),
                                             selected = "October", multiple = FALSE)),
                          column(4,
                                 selectInput("paLong", label = "Number of Months", 
                                             choices = 1:12,
                                             selected = 12, multiple = FALSE))
                        ),
                        fluidRow(
                          column(3, h5("Units:", align = "right", style = "margin-top: 40px")),
                          column(3,
                                 selectInput("fluxUnit", label = "Flux Units", 
                                             choices = list("pounds/day" = 1,
                                                            "tons/day" = 2,
                                                            "kg/day" = 3,
                                                            "thousands of kg/day" = 4,
                                                            "tons/year" = 5,
                                                            "thousands of tons/year" = 6,
                                                            "millions of tons/year" = 7,
                                                            "thousands of kg/year" = 8,
                                                            "millions of kg/year" = 9,
                                                            "billions of kg/year" = 10,
                                                            "thousands of tons/day" = 11,
                                                            "millions of kg/day" = 12,
                                                            "kg/year" = 13),
                                             selected=3, multiple = FALSE)
                                 
                          ),
                          column(4,
                                 selectInput("qUnit", label = "Flow Units", 
                                             choices = list("Cubic Feet per Second" = 1,
                                                            "Cubic Meters per Second" = 2,
                                                            "Thousand Cubic Feet per Second" = 3,
                                                            "Thousand Cubic Meters per Second" = 4),
                                             selected=1, multiple = FALSE)
                                 
                          )
                        ),
                  tabsetPanel(
                    tabPanel("MetaData",
                             fluidRow(column(1),
                                      column(11,
                                             DT::dataTableOutput('metaData')))
                    ),
                    tabPanel("Flow History",
                             fluidRow(
                               column(3,
                                      selectInput("flowPlots", label = "Flow History", 
                                                  choices = c("plotFlowSingle","plotSDLogQ","plotQTimeDaily","plotFour","plotFourStats"),
                                                  selected = "plotFlowSingle", multiple = FALSE),
                                      uiOutput("flowStatistic"),
                                      uiOutput("flowLog")),
                               column(9,
                                      h4("R Code:"),
                                      verbatimTextOutput("flowCode"),
                                      plotOutput("flowPlotsOut"),
                                      downloadButton('downloadFlowPlot', 'Download Plot'))
                             )),
                    tabPanel("Explore Data",
                             htmlOutput("SampleText"),
                             fluidRow(
                               column(3,
                                      selectInput("dataPlots", label = "Data", 
                                                  choices = c("boxConcMonth","boxQTwice","plotConcTime","plotConcQ","multiPlotDataOverview"),
                                                  selected = "multiPlotDataOverview", multiple = FALSE) ,
                                      uiOutput("dataLog")),
                               column(9,
                                      h4("R Code:"),
                                      verbatimTextOutput("dataCode"),
                                      plotOutput("dataPlotsOut"),
                                      downloadButton('downloadDataPlot', 'Download Plot'))
                             )),
                    tabPanel("Explore Model",
                             fluidRow(
                               column(3,
                                      selectInput("modelPlots", label = "Data", 
                                                  choices = c("plotConcTimeDaily","plotFluxTimeDaily","plotConcPred","plotFluxPred","plotResidPred",
                                                              "plotResidQ","plotResidTime","boxResidMonth","boxConcThree",
                                                              "plotConcHist","plotFluxHist","plotConcQSmooth","plotConcTimeSmooth",
                                                              "fluxBiasMulti","plotContours","plotDiffContours"),
                                                  selected = "fluxBiasMulti", multiple = FALSE),
                                      uiOutput("modelLog"),
                                      uiOutput("date1"),
                                      uiOutput("date2"),
                                      uiOutput("date3"),
                                      uiOutput("qLow"),
                                      uiOutput("qMid"),
                                      uiOutput("qHigh"),
                                      uiOutput("yearStart"),
                                      uiOutput("yearEnd"),
                                      uiOutput("centerDate"),
                                      uiOutput("maxDiff"),
                                      uiOutput("from"),
                                      uiOutput("to"),
                                      uiOutput("by")),
                               column(9,
                                      h4("R Code:"),
                                      verbatimTextOutput("modelCode"),
                                      plotOutput("modelPlotsOut"),
                                      downloadButton('downloadModelPlot', 'Download Plot'))
                             ))
                  )))),
      column(1)
    )
    
  )
)