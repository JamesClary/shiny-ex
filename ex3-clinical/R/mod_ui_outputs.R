#' Module for UI outputs
#'
#' Module for UI outputs
#'
#' @param id Namespace
#'
#' @returns Shiny Module

mod_ui_outputs <- function(id){

  ns <- NS(id)

  tagList(
    plotOutput(outputId = ns('simPlot')),
    plotOutput(outputId = ns('simPlotE')),
    plotOutput(outputId = ns('simPlotDose'))
  )

}
