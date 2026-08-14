#' Simulation Server
#'
#' @param id Namespace
#' @param dosedat dosing data.frame
#'
#' @returns Shiny server module


mod_server_runsim  <- function(id, dosedat){

  moduleServer(id, function(input, output, session){

    mod <- eventReactive(input$dosevec, {

      mods <- mread(system.file('extdata', 'adaptive-dose-mod.cpp', package = 'ex3-clinical'))

      dose_vec_num <- as.numeric(unlist(strsplit(input$dosevec, ','))) # grab vector and make numeric

      mods <- env_update(mods, DOSEQD = dose_vec_num) # push to mrgsolve environment
      mods <- env_eval(mods) # update mod to run sim

      return(mods)

    })

    sim_dataframe <- eventReactive(input$btn, {

      df <- dosedat()

      set.seed(12345)

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
