#' Update Dosing Table
#'
#' @param id Namespace
#'
#' @returns Shiny server module


mod_server_dosetable <- function(id){

  moduleServer(id, function(input, output, session){

    ns <- session$ns

    observeEvent(input$regs, {

      # Get current table
      df <- input$dosetab

      #create large table with Additional inputs
      df2 <- data.frame(
        'Dose' = rep(5, 20),
        'Doses' = rep(0, 20),
        'Interval' = rep(24, 20)
      )

    df3 <- rbind(df,df2)

    updateMatrixInput(
      inputId = 'dosetab',
      value = as.matrix(df3[1:input$regs,]),
      session = getDefaultReactiveDomain()
    )
    })

  })

}
