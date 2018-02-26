#' Function to run annovarR shiny APP service
#' 
#' @param appDir The application to run. 
#' Default is system.file('extdata', 'tools/shiny/R', package = 'annovarR')
#' @param ... Other parameters pass to \code{\link[shiny]{runApp}}
#' @export
#' 
#' @examples
#' \dontrun{
#'   web()
#' }
web <- function(appDir = system.file("extdata", "tools/shiny/R", package = "annovarR"), 
  ...) {
  params <- list(...)
  params <- config.list.merge(list(appDir), params)
  do.call(runApp, params)
}
