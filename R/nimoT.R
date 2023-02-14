
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Don't hesitate to contact me if you ",
                               "have any question:\n",
                               "emanuel.huber@pm.me\n\n",
                               "There is only one function to run in this package:\n",
                               "runNimoT"))
  utils::data(logo, package = "nimoT")
}



#' nimoT: A package for reading and visualising nimoT data
#'
#' This is a shiny project
#'
#' @import graphics
#' @import shiny
#' @import shinyFiles
#' @import colourpicker
#' @import doremi
# @importFrom colourpicker updateColourInput colourInput
#' @importFrom fs path_home
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats runmed smooth.spline
#' @importFrom utils read.table data
#' @importFrom signal interp1
#' @importFrom mmand gaussianSmooth
#' @importFrom yaml write_yaml read_yaml
"_PACKAGE"
#> [1] "_PACKAGE"



#' Lauch the unit converter app
#'
#' Lauch the nimoT reader app!
#'
#' @param ... Arguments to be passed to the function
#' @export
#'
#' @return shiny application object
runNimoT <- function(...) {
  shinyApp(ui = shinyAppUI, server = shinyAppServer, ...)
}


#' Logo GEOTEST
#'
#' The logo of GEOTEST
#'
#' @format a read png object
#'
#' @source geotest
"logo"
