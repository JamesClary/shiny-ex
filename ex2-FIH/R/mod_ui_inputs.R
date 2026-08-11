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
    selectInput(
      inputId = ns('modtype'),
      label = 'Model',
      choices = c('Uninfected', 'Infected'),
      selected = 'Uninfected'
    ),
    sliderInput(
      inputId = ns('regs'),
      label = 'No. of Regimens',
      value = 2,
      min = 1,
      max = 5,
      step = 1
    ),
    matrixInput(
      inputId = ns('dosetab'),
      value = as.matrix(
        data.frame(
          'Dose' = c(5, 10),
          'Doses' = c(2, 1),
          'Interval' = c(12, 24)
          )
        ),
      label = 'Dose Regimens',
      class = 'character',
      rows = list(
        n = 1,
        names = FALSE,
        extend = FALSE,
        delta = 1
      ),
      cols = list(
        n = 1,
        names = TRUE,
        extend = FALSE
      )
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
      inputId = ns('simtime'),
      label = "Simulation Time",
      value = 24
    ),
    numericRangeInput(
      inputId = ns('paramcalc'),
      label = 'Parameter Calculation Time',
      value = c(0,24)
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
    selectInput(
      inputId = ns('log10'),
      label = 'Log Y Axis',
      choices = c('True', 'False')
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
