
shinyUI(
  fluidPage(
  
    fluidRow(
      column(4,
        h1("Trends")
#         selectInput("data", label = "Data", 
#                          choices = c("Water Sample",
#                                      "Passive Samples",
#                                      "Water Samples + Passive"),
#                          selected = "Water Sample + ToxCast", multiple = FALSE),
#         selectInput("sites", label = "Site", 
#                                       choices = c("All",summaryFile$site),
#                                       selected = "All", multiple = FALSE),
#         selectInput("groupCol", label = "Annotation (# choices)", 
#                                       choices = setNames(names(endPointInfo)[-3],groupChoices),
#                                       selected = names(endPointInfo)[20], multiple = FALSE),
#         tags$div(class="header", checked=NA,
#                  tags$p("For annotation information, see: "),
#                  tags$a(href="http://www.epa.gov/ncct/toxcast/files/ToxCast%20Assays/ToxCast_Assay_Annotation_Data_Users_Guide_20141021.pdf", "ToxCast")),
#         
#         uiOutput("groupControl")
        ),
      column(8, 
             leaflet::leafletOutput("mymap")
      )))
        
    
)