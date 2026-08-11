#' Table Server
#'
#' @param id Namespace
#' @param simdat_raw simulation data.frame
#'
#' @returns Shiny server module


mod_server_mktable  <- function(id, simdat_raw){

  moduleServer(id, function(input, output, session){

  simdat <- eventReactive(input$btn, {

    dat <- simdat_raw()

    return(dat)
  })

  out_tab <- eventReactive(input$btn, {

    out_sum <- simdat() %>%
      mutate_all(as.numeric) %>%
      group_by(group) %>%
      mutate(Group = paste0('Group ', group, ' - Dose: ', DOSE, ' Q', ii2)) %>%
      ungroup

    levels <- unique(out_sum$Group)

    out_sum <- out_sum %>%
      mutate(Group = factor(Group, levels = levels))

    if(input$nsim == 1){

      out <- out_sum %>%
        filter(time >= input$paramcalc[1] & time <= input$paramcalc[2]) %>%
        reframe(Cmax = signif(max(Cc), 3),
                Tmax = round(time[which.max(Cc)], 1),
                Cmin = signif(last(Cc), 3),
                AUCtau = round(auc_partial(time, Cc)),
                Thalf = round(half_life(cl = CLapp, vc = Vcapp, q = Qapp, vp = Vpapp), 1),
                .by = Group) %>%
        mutate(N = input$nsim) %>%
        select(Group, N, Cmax, Tmax, Cmin, AUCtau, Thalf)

    } else {

      out <- out_sum %>%
        filter(time >= input$paramcalc[1] & time <= input$paramcalc[2]) %>%
        reframe(Cmax = signif(max(Cc), 3),
                Tmax = round(time[which.max(Cc)], 1),
                Cmin = signif(last(Cc), 3),
                AUCtau = round(auc_partial(time, Cc)),
                Thalf = half_life(cl = CLapp, vc = Vcapp, q = Qapp, vp = Vpapp),
                .by = c(ID, Group)) %>%
        reframe(Cmax = paste0(signif(median(Cmax), 3), ' (', signif(min(Cmax), 3), '-', signif(max(Cmax), 3), ')'),
                Tmax = paste0(round(median(Tmax), 1), ' (', round(min(Tmax), 1), '-', round(max(Tmax), 1), ')'),
                Cmin = paste0(signif(median(Cmin), 3), ' (', signif(min(Cmin), 3), '-', signif(max(Cmin), 3), ')'),
                AUCtau = paste0(round(median(AUCtau)), ' (', round(min(AUCtau)), '-', round(max(AUCtau)), ')'),
                Thalf = paste0(round(median(Thalf), 1), ' (', round(min(Thalf), 1), '-', round(max(Thalf), 1), ')'),
                .by = Group) %>%
        mutate(N = input$nsim) %>%
        select(Group, N, Cmax, Tmax, Cmin, AUCtau, Thalf)

    }

    return(out)

  })

  output$simTable <- function(){

    if(input$nsim == 1){

      kable(out_tab() %>% distinct() |> select(-N),
            col.names = c('Dosing Group',
                          'Cmax (ng/mL)',
                          'Tmax (h)',
                          'Cmin (ng/mL)',
                          'AUCtau (ng*h/mL)',
                          'T1/2 (h)'),
            format = 'html') %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "hover", "condensed")
        )

    } else {

      kable(out_tab() %>% distinct(),
            col.names = c('Dosing Group',
                          'N',
                          'Cmax (ng/mL)',
                          'Tmax (h)',
                          'Cmin (ng/mL)',
                          'AUCtau (ng*h/mL)',
                          'T1/2 (h)'),
            format = 'html') %>%
        kable_styling(
          font_size = 15,
          bootstrap_options = c("striped", "hover", "condensed")
        ) %>%
        footnote('Median (Minimum-Maximum)')

    }


  }

  })
}
