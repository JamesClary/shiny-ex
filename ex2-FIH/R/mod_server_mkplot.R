#' Plotting Server
#'
#' @param id Namespace
#' @param simdat_raw simulation data.frame
#'
#' @returns Shiny server module


mod_server_mkplot  <- function(id, simdat_raw){

  moduleServer(id, function(input, output, session){

    probs <- eventReactive(input$btn, {
      if(input$perc =='80') probs <- c(.1, .9)
      if(input$perc =='90') probs <- c(.05, .94)
      if(input$perc =='95') probs <- c(.025, .975)

      return(probs)
    })

    simdat <- eventReactive(input$btn, {

      dat <- simdat_raw()

      return(dat)
    })

    plot_out <- eventReactive(input$btn, {

      out_sum <- simdat() %>%
        mutate_all(as.numeric) %>%
        group_by(group) %>%
        mutate(Group = paste0('Group ', group, ' - Dose: ', DOSE, ' Q', ii2)) %>%
        ungroup

      levels <- unique(out_sum$Group)

      out_sum <- out_sum %>%
        mutate(Group = factor(Group, levels = levels))

      if(input$nsim == 1){

        plot <- ggplot(out_sum %>% filter(Cc > 0))+
          geom_line(aes(time, Cc, color = factor(Group), group = Group))+
          labs(x = 'Time Since First Dose (h)',
               y = 'Concentration (ng/mL)',
               color = 'Dosing Regimen')+
          theme_bw()

      } else {

        probsc <- probs()

        out_sum <- out_sum %>%
          reframe(plow = quantile(Cc, probs = probsc[1]),
                  pmid = quantile(Cc, probs = 0.5),
                  phi  = quantile(Cc, probs = probsc[2]),
                  .by = c(Group, time))

        plot <- ggplot(out_sum %>% filter(pmid > 0))+
          geom_ribbon(aes(time, ymin = plow, ymax = phi, fill = factor(Group)), color = NA, alpha = 0.2)+
          geom_line(aes(time, pmid, color = factor(Group), group = Group))+
          labs(x = 'Time Since First Dose (h)',
               y = 'Concentration (ng/mL)',
               color = 'Dosing Regimen',
               fill = 'Dosing Regimen')+
          theme_bw()


      }

      if(input$log10 == 'True') plot <- plot + scale_y_log10()

      plot+
        theme(axis.title = element_text(size = 18))+
        theme(axis.text = element_text(size = 14))+
        theme(legend.title = element_text(size = 14))+
        theme(legend.text = element_text(size = 16))

    })

    output$simPlot <- renderPlot(plot_out())

  })

}
