#' Explore models with graphs
#' 
#' Open interactive graphs for metabolism models
#' 
#' @param browse use browser for graph rendering
#' @export
#' @import shiny
#' @import sbtools
#' @import smwrGraphs
#' @import usgsEGRET
#' @import leaflet
explore_model <- function(browse=TRUE){
  if(is.null(current_session())) authenticate_sb()
  runApp(system.file('shiny',package='htcTrends'), launch.browser = browse)
}