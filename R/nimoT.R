
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Don't hesitate to contact me if you ",
                               "have any question:\n",
                               "emanuel.huber@pm.me\n\n",
                               "There is only one function to run in this package:\n",
                               "runNimoT"))
}



#' nimoT: A package for reading and visualising nimoT data
#'
#' This is a shiny project
#'
#' @import graphics
#' @import Cairo
#' @import shiny
#' @import shinyFiles
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats runmed smooth.spline
#' @importFrom utils read.table
#' @importFrom signal interp1
#' @importFrom mmand gaussianSmooth
"_PACKAGE"
#> [1] "_PACKAGE"



#' Lauch the unit converter app
#'
#' Lauch the nimoT reader app!
#'
#' @export
#'
#' @return shiny application object
runNimoT <- function(...) {
  shinyApp(ui = shinyAppUI, server = shinyAppServer, ...)
}