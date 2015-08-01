#' Explore models with graphs
#' 
#' Open interactive graphs for metabolism models
#' 
#' @param browse use browser for graph rendering
#' @export
#' @importFrom shiny runApp
#' @importFrom sbtools current_session
explore_model <- function(browse=TRUE){
  if(is.null(current_session())) login_sb()
  runApp(system.file('shiny','model',package='powstreams'), launch.browser = browse)
}