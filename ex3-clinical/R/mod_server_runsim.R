#' Simulation Server
#'
#' @param id Namespace
#' @param dosedat dosing data.frame
#'
#' @returns Shiny server module


mod_server_runsim  <- function(id, dosedat){

  moduleServer(id, function(input, output, session){

    mod <- eventReactive(input$btn, {

      mods <- mread(system.file('extdata', 'adaptive-dose-mod.cpp', package = 'ex3-clinical'))

      return(mods)

    })

    sim_dataframe <- eventReactive(input$btn, {

      df <- dosedat()

      if(input$nsim == 1){

        out <- mod() %>%
          data_set(df) %>%
          zero_re()%>%
          mrgsim_df(tgrid = seq(0, input$simtime, 0.1))

      } else {

        out <- mod() %>%
          data_set(df) %>%
          mrgsim_df(tgrid = seq(0, input$simtime, 0.1))

      }

      return(out)



    })
    return(reactive({sim_dataframe()}))

  })
}
