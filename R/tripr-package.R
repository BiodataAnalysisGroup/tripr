#' tripr
#'
#'
#' T-cell Receptor/Immunoglobulin Profiler (TRIP)
#'
#' @details
#' The only function you're likely to need from tripr is [run_app()].
#' Otherwise refer to the vignettes for using tripr.
#'
#' @docType package
#' @name tripr
#' @import dplyr
#' @importFrom plyr ldply join .
#' @importFrom dplyr '%>%'
#' @importFrom shiny includeHTML shinyServer
#' @importFrom shinyFiles shinyDirButton shinyDirChoose
#' @importFrom shinyjs useShinyjs hidden extendShinyjs js
#' @importFrom shinyBS bsButton bsPopover bsModal
#' @importFrom plotly renderPlotly plotlyOutput plot_ly
#' @importFrom RColorBrewer brewer.pal
#' @importFrom plot3D hist3D persp3D image2D
#' @importFrom grDevices dev.off pdf png rainbow
#' @importFrom graphics axis barplot boxplot legend lines pie text
#' @importFrom methods new
#' @importFrom stats aggregate median spline
#' @importFrom utils read.csv tar write.table
#' @importFrom stringr str_length str_detect
#' @importFrom gridExtra grid.table
#' @importFrom data.table data.table .N rbindlist .SD
#'
NULL
