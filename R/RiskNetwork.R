#' Risk network modeling and analysis.
#'
#' RiskNetwork is a Shiny web application for risk network modeling and
#' analysis.
#' @import bnlearn
#' @import d3heatmap
#' @import lattice
#' @import networkD3
#' @import rhandsontable
#' @import shiny
#' @import shinydashboard
#' @export
RiskNetwork <- function() {
  shiny::runApp(system.file('rn', package='RiskNetwork'))
}
