library(dplyr)
library(leaflet)


shinyServer(function(input, output) {
  
    

    
    output$mymap <- leaflet::renderLeaflet({
      
      leaflet() %>%
        addProviderTiles("Esri.WorldPhysical") %>%
        setView(lng = -83.5, lat = 44.5, zoom=5) 
      
    })
    
    observe({
      
#       if(nrow(sumStat) == 0){
#         sumStat <- summaryFile
#         sumStat$sumHits <- sumStat$nChem
#       }
#       
#       mapData <- right_join(stationINFO, sumStat, by=c("shortName"="site"))
#       mapData <- mapData[!is.na(mapData$dec.lat.va),]
#       # mapData <- mapData[!is.na(mapData$nChem),]
#       
#       col_types <- c("darkblue","dodgerblue","green","yellow","orange","red","brown")
#       leg_vals <- unique(as.numeric(quantile(mapData$maxEAR, probs=c(0,0.01,0.1,0.25,0.5,0.75,0.9,.99,1), na.rm=TRUE)))
#       
#       cols <- colorNumeric(col_types, domain = leg_vals)
#       rad <- 1.5*seq(5000,25000, 500)
#       mapData$sizes <- rad[as.numeric(
#         cut(mapData$sumHits, 
#             c(-1,unique(as.numeric(
#               quantile(mapData$sumHits, probs=seq(.01,.99,length=length(rad)), na.rm=TRUE)
#             )),(max(mapData$sumHits,na.rm=TRUE)+1))
#         ))]
#       pal = colorBin(col_types, mapData$maxEAR, bins = leg_vals)
#       
#       if(input$sites != "All"){
#         mapData <- mapData[mapData$shortName == input$sites,]
#       }
#       
#       leafletProxy("mymap", data=mapData) %>%
#         clearShapes() %>%
#         clearControls() %>%
#         addCircles(lat=~dec.lat.va, lng=~dec.long.va, 
#                    popup=mapData$Station.Name
#                    #                    paste0('<b>',mapData$Station.Name,"</b><br/><table>",
#                    #                           "<tr><td>Frequency</td><td>",sprintf("%1.1f",mapData$freq),'</td></tr>',
#                    #                           "<tr><td>nSample</td><td>",mapData$nSamples,'</td></tr>',
#                    #                           "<tr><td>maxEAR</td><td>",sprintf("%1.1f",mapData$maxEAR),'</td></tr>',
#                    #                           "<tr><td>nChem</td><td>",mapData$nChem,'</td></tr>',
#                    #                           "<tr><td>nEndPoints</td><td>",mapData$nEndPoints,'</td></tr></table>')
#                    ,
#                    fillColor = ~pal(maxEAR), 
#                    weight = 1,
#                    color = "black",
#                    fillOpacity = 0.8, radius = ~sizes, opacity = 0.8) %>%
#         addLegend(
#           position = 'bottomleft',
#           pal=pal,
#           values=~maxEAR,
#           opacity = 0.8,
#           title = 'Maximum EAR')
      
    })

})