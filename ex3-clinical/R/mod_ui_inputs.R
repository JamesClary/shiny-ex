#' Module for UI inputs
#'
#' Module for UI inputs
#'
#' @param id Namespace
#'
#' @returns Shiny Module

mod_ui_inputs <- function(id){

  ns <- NS(id)

  tagList(
    actionBttn(
      inputId = ns('btn'),
      label = 'Simulate'
    ),
    br(),
    h4('Simulation Details'),
    hr(),
    numericInput(
      inputId = ns("nsim"),
      label = 'Number of Simulations',
      value = 10,
      min = 1,
      max = 1000
    ),
    numericInput(
      inputId = ns('interval'),
      label = "Interdose Interval",
      value = 24
    ),
    numericInput(
      inputId = ns('simtime'),
      label = "Simulation Time",
      value = 336
    ),
    conditionalPanel(
      condition = "input.nsim > 1",
      selectInput(
        inputId = ns('perc'),
        label = 'Prediction Interval',
        choices = c('80', '90', '95'),
        selected = '90',
        multiple = FALSE
      ),
      ns = NS(id)
    ),
    br(),
    h4('Population Covariate'),
    hr(),
    numericInput(
      inputId = ns('WTKG'),
      label = 'Weight (kg)',
      value = 60,
      min = 0
    )
  )


}
