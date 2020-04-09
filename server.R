# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# --------------------------------------------------------------
# Definición de funciones
# --------------------------------------------------------------
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = as.Date(date), y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    xlab("fecha") + 
    ylab("casos acumulados") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = as.Date(date), y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Fecha", "Día del caso 100", "Día del caso 10", 
                                                      "Día de la 10ma muerte", "Día de la 1ra muerte")) {
  if (start_point=="Fecha") {
    
    g <- plot_ly(cv_cases, x = ~date, y = ~new_outcome, type = 'bar', color = ~region, 
                 colors = country_cols) %>%
    layout(xaxis = list(title = "fecha"), yaxis = list(title = "nuevos casos"), barmode = "stack")

  }
  
  if (start_point=="Día del caso 100") {
    cv_cases = subset(cv_cases, days_since_case100>0)

    g <- ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Días desde el caso 100")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día del caso 10") {
    cv_cases = subset(cv_cases, days_since_case10>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_case10, y = new_outcome, fill = region,
                              text = paste0("Day ",days_since_case10, "\n", region, ": ",new_outcome)))+
      xlab("Días desde el caso 10")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 10ma muerte") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Días desde la 10ma muerte")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 1ra muerte") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_death1, y = new_outcome, fill = region,
                              text = paste0("Day ",days_since_death1, "\n", region, ": ",new_outcome))) +
      xlab("Días desde la 1ra muerte")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  g
}

country_cases_plot_prov = function(cv_cases, start_point=c("Fecha", "Día del caso 100", "Día del caso 10", 
                                                           "Día de la 10ma muerte", "Día de la 1ra muerte")) {
  if (start_point=="Fecha") {
    
    g <- plot_ly(cv_cases, x = ~date, y = ~new_outcome, type = 'bar', color = ~region) %>%
      layout(xaxis = list(title = "fecha"), yaxis = list(title = "nuevos casos"), barmode = "stack")
    
  }
  
  if (start_point=="Día del caso 100") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region,
                              text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Días desde el caso 100")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      #scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día del caso 10") {
    cv_cases = subset(cv_cases, days_since_case10>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_case10, y = new_outcome, fill = region,
                              text = paste0("Day ",days_since_case10, "\n", region, ": ",new_outcome)))+
      xlab("Días desde el caso 10")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      #scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 10ma muerte") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region,
                              text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Días desde la 10ma muerte")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
     # scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 1ra muerte") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    
    g <- ggplot(cv_cases, aes(x = days_since_death1, y = new_outcome, fill = region,
                              text = paste0("Day ",days_since_death1, "\n", region, ": ",new_outcome))) +
      xlab("Días desde la 1ra muerte")
    g1 <- g +
      geom_bar(position="stack", stat="identity") +
      ylab("nuevos casos") + theme_bw() +
      # scale_fill_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  g
}



# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, 
                                    start_point=c("Fecha", "Día del caso 100", "Día del caso 10", 
                                                  "Día de la 10ma muerte", "Día de la 1ra muerte")) {
  if (start_point=="Fecha") {
    
    g <- plot_ly(cv_cases, x = ~date, y = ~outcome, type = 'scatter', mode = 'lines+markers', color = ~region, 
                 colors = country_cols) %>%
      layout(xaxis = list(title = "fecha"), yaxis = list(title = "casos acumulados"), barmode = "stack")
    
  }
  
  if (start_point=="Día del caso 100") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Días desde el caso 100")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día del caso 10") {
    cv_cases = subset(cv_cases, days_since_case10>0)
    g = ggplot(cv_cases, aes(x = days_since_case10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case10,"\n", region, ": ",outcome))) +
      xlab("Días desde el caso 10")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 10ma muerte") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Días desde la 10ma muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 1ra muerte") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death1,"\n", region, ": ",outcome))) +
      xlab("Días desde la 1ra muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  g
}

country_cases_cumulative_prov = function(cv_cases, 
                                    start_point=c("Fecha", "Día del caso 100", "Día del caso 10", 
                                                  "Día de la 10ma muerte", "Día de la 1ra muerte")) {
  if (start_point=="Fecha") {
    
    g <- plot_ly(cv_cases, x = ~date, y = ~outcome, type = 'scatter', mode = 'lines+markers', color = ~region) %>%
      layout(xaxis = list(title = "fecha"), yaxis = list(title = "casos acumulados"), barmode = "stack")
  }
  
  if (start_point=="Día del caso 100") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Días desde el caso 100")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día del caso 10") {
    cv_cases = subset(cv_cases, days_since_case10>0)
    g = ggplot(cv_cases, aes(x = days_since_case10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case10,"\n", region, ": ",outcome))) +
      xlab("Días desde el caso 10")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 10ma muerte") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Días desde la 10ma muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 1ra muerte") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death1,"\n", region, ": ",outcome))) +
      xlab("Días desde la 1ra muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  g
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Fecha", "Día del caso 100", "Día del caso 10", 
                                                                "Día de la 10ma muerte", "Día de la 1ra muerte"))  {
  if (start_point=="Fecha") {
    g <- plot_ly(cv_cases, x = ~date, y = ~log10(outcome), type = 'scatter', mode = 'lines+markers', color = ~region, 
                 colors = country_cols) %>%
      layout(xaxis = list(title = "fecha"), yaxis = list(title = "casos acumulados (log10)"), barmode = "stack")
    
  }
  
  if (start_point=="Día del caso 100") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
      xlab("Días desde el caso 100")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día del caso 10") {
    cv_cases = subset(cv_cases, days_since_case10>0)
    g = ggplot(cv_cases, aes(x = days_since_case10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case10, "\n", region, ": ",outcome))) +
      xlab("Días desde el caso 10")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 10ma muerte") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Días desde la 10ma muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 1ra muerte") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death1, "\n", region, ": ",outcome))) +
      xlab("Días desde la 1ra muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      scale_colour_manual(values=country_cols) +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  g
}

country_cases_cumulative_log_prov = function(cv_cases, start_point=c("Fecha", "Día del caso 100", "Día del caso 10", 
                                                                     "Día de la 10ma muerte", "Día de la 1ra muerte"))  {
  if (start_point=="Fecha") {
    g <- plot_ly(cv_cases, x = ~date, y = ~log10(outcome), type = 'scatter', mode = 'lines+markers', color = ~region) %>%
      layout(xaxis = list(title = "fecha"), yaxis = list(title = "casos acumulados (log10)"), barmode = "stack")
    
  }
  
  if (start_point=="Día del caso 100") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
      xlab("Días desde el caso 100")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día del caso 10") {
    cv_cases = subset(cv_cases, days_since_case10>0)
    g = ggplot(cv_cases, aes(x = days_since_case10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case10, "\n", region, ": ",outcome))) +
      xlab("Días desde el caso 10")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 10ma muerte") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Días desde la 10ma muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  
  if (start_point=="Día de la 1ra muerte") {
    cv_cases = subset(cv_cases, days_since_death1>0)
    g = ggplot(cv_cases, aes(x = days_since_death1, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death1, "\n", region, ": ",outcome))) +
      xlab("Días desde la 1ra muerte")
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
      ylab("casos acumulados (log10)") + theme_bw() +
      scale_y_continuous(trans="log10") +
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    g <- ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  }
  g
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  # ----------------------------------------------------
  # Datasets reactivos
  # ----------------------------------------------------
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
  })
  
  reactive_db_argentina = reactive({
    cv_cases_argentina %>% filter(date == input$plot_date_argentina)
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_last24h_argentina = reactive({
    # Este if porque sino el play no anda para los datos de Argentina
    if(input$plot_date_argentina == "2020-03-04") {
      cv_cases_argentina %>% filter(date == "2020-03-03" & new_cases>0)  
    } else {
      cv_cases_argentina %>% filter(date == input$plot_date_argentina & new_cases>0)  
    }
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_argentina = reactive({
    large_countries = reactive_db_argentina() %>% filter(argentina_ID %in% argentina$NAME_1)
    large_countries = large_countries[order(large_countries$argentina_ID),]
    large_countries
  })
  
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h_argentina = reactive({
    large_countries = reactive_db_last24h_argentina() %>% filter(argentina_ID %in% argentina$NAME_1)
    large_countries = large_countries[order(large_countries$argentina_ID),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_argentina = reactive({
    argentina[argentina$NAME_1 %in% reactive_db_large_argentina()$argentina_ID, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  reactive_polygons_last24h_argentina = reactive({
    argentina[argentina$NAME_1 %in% reactive_db_large_last24h_argentina()$argentina_ID, ]
  })
  
  # ----------------------------------------------------
  # Outputs reactivos
  # ----------------------------------------------------
  # Fecha
  output$clean_date_reactive <- renderText({
    format(input$plot_date,"%d %B %Y")
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " casos")
  })
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$death), big.mark=","), " muertes")
  })
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recuperados")
  })
  output$reactive_active_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " casos activos")
  })
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " países/regiones afectados")
  })
  # Outputs reactivos de Argentina
  output$reactive_case_count_argentina <- renderText({
    paste0(prettyNum(sum(reactive_db_argentina()$cases), big.mark=","), " casos")
  })
  output$reactive_death_count_argentina <- renderText({
    paste0(prettyNum(sum(reactive_db_argentina()$death), big.mark=","), " muertes")
  })
  output$reactive_recovered_count_arg <- renderText({
    paste0(prettyNum(sum(reactive_db_argentina()$recovered), big.mark=","), " recuperados")
  })
  output$reactive_active_count_arg <- renderText({
    paste0(prettyNum(sum(reactive_db_argentina()$active_cases), big.mark=","), " casos activos")
  })
  
  output$reactive_new_cases_24h <- renderText({
    valor <- (cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new
    valorusar <- format(valor ,big.mark=",",scientific=FALSE)
    paste0(valorusar, " nuevos en las últimas 24 horas")
  })
  
  output$reactive_new_cases_24h_arg <- renderText({
    valor <- (cv_aggregated_argentina %>% filter(date == input$plot_date_argentina & region=="País"))$new
    valorusar <- format(valor ,big.mark=",",scientific=FALSE)
    paste0(valorusar, " nuevos en las últimas 24 horas")
  })

  # ----------------------------------------------------
  # Mapa Argentina
  # ----------------------------------------------------
  output$mymap_argentina <- renderLeaflet({ 
    basemap_argentina
  })
  
  observeEvent(input$plot_date_argentina, {
    req(input$nav == "Mapa Argentina")
    # message(input$plot_date_argentina)    
    # if(input$plot_date_argentina == "2020-04-03") {
    #   browser()
    # }
    # Correccion cantidades
    poly <- reactive_polygons_argentina()
    argie <- reactive_db_large_argentina()
    tmp <- merge(poly[,c("NAME_1", "per100k")], argie[,c("argentina_ID", "per100k")], by.x = "NAME_1", by.y = "argentina_ID")
    poly@data$per100k <- tmp@data$per100k.y # Ahora si actualizado

    leafletProxy("mymap_argentina") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      #addPolygons(data = reactive_polygons_argentina(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal_argentina(reactive_db_large_argentina()$per100k)) %>%
      addPolygons(data = poly, stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal_argentina(poly@data$per100k)) %>%
      addCircleMarkers(data = reactive_db_last24h_argentina(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/1.7),
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (nuevos)",
                       label = sprintf("<strong>%s (últimas 24h)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Casos cada 100,000: %g",
                                       reactive_db_last24h_argentina()$argentina_ID, reactive_db_last24h_argentina()$new_cases, reactive_db_last24h_argentina()$new_deaths,
                                       reactive_db_last24h_argentina()$newper100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      addCircleMarkers(data = reactive_db_argentina(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/1.7),
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (acumulados)",
                       label = sprintf("<strong>%s (acumulados)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Casos cada 100,000: %g",
                                       reactive_db_argentina()$argentina_ID, reactive_db_argentina()$cases, reactive_db_argentina()$deaths,
                                       reactive_db_argentina()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto"))
    
  })
  
  # ----------------------------------------------------
  # Mapa
  # ----------------------------------------------------
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  # leaflet Proxy
  observeEvent(input$nav, {
    req(input$nav == "Mapa Mundial")
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>%
      addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/4),
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (nuevos)",
                       label = sprintf("<strong>%s (últimas 24h)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Recuperados: %d<br/>Casos cada 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%

      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (acumulados)",
                       label = sprintf("<strong>%s (acumulados)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Recuperados: %d<br/>Casos cada 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%

      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/4),
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (activos)",
                       label = sprintf("<strong>%s (activos)</strong><br/>Casos confirmados: %g<br/>Casos cada 100,000: %g<br/><i><small>Se excluyen individuos<br/>recuperados (%g) o muertos (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto"))
  })
  
  observeEvent(input$plot_date, {
    req(input$nav == "Mapa Mundial")
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>% 
      addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/4), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (nuevos)",
                       label = sprintf("<strong>%s (últimas 24h)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Recuperados: %d<br/>Casos cada 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (acumulados)",
                       label = sprintf("<strong>%s (acumulados)</strong><br/>Casos confirmados: %g<br/>Muertes: %d<br/>Recuperados: %d<br/>Casos cada 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/4), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (activos)",
                       label = sprintf("<strong>%s (activos)</strong><br/>Casos confirmados: %g<br/>Casos cada 100,000: %g<br/><i><small>Se excluyen individuos<br/>recuperados (%g) o muertos (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) 
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, input$plot_date)
  })
  
  output$cumulative_plot_arg <- renderPlot({
    cumulative_plot(cv_aggregated_argentina, input$plot_date_argentina)
  })
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                        selected = cv_today_100$country)
    }
  }, ignoreInit = TRUE)
  
  
  # ----------------------------------------------------
  # Dataframe reactivo mundial
  # ----------------------------------------------------
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cv_cases_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    
    if (input$outcome_select=="Cases") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  
  # ----------------------------------------------------
  # Datos de Argentina
  # ----------------------------------------------------
  country_reactive_db_arg = reactive({
    db = cv_cases
    db$region = db$country
    
    if (input$outcome_select_arg=="Casos") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select_arg=="Muertes") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% "Argentina")
  })
  
  # ----------------------------------------------------
  # Datos de Provincias
  # ----------------------------------------------------
  #browser()
  country_reactive_db_prov = reactive({

    db = cv_cases_argentina
    db$region = db$argentina_ID
    
    if (input$outcome_select_prov=="Cases") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select_prov=="Deaths") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% input$region_select_prov)
  })
  
  # ----------------------------------------------------
  # Datos de Argentina
  # ----------------------------------------------------
  # country-specific plots
  output$country_plot_arg <- renderPlotly({
#    browser()
    country_cases_plot(country_reactive_db_arg(), start_point=input$start_date_arg)
  })
  
  # country-specific plots
  output$country_plot_cumulative_arg <- renderPlotly({
    #browser()
    country_cases_cumulative(country_reactive_db_arg(), start_point=input$start_date_arg)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log_arg <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db_arg(), start_point=input$start_date_arg)
  })
  
  
  # ----------------------------------------------------
  # Datos provinciales
  # ----------------------------------------------------
  # country-specific plots
  output$country_plot_prov <- renderPlotly({
    country_cases_plot_prov(country_reactive_db_prov(), start_point=input$start_date_prov)
  })
  
  # country-specific plots
  output$country_plot_cumulative_prov <- renderPlotly({
    country_cases_cumulative_prov(country_reactive_db_prov(), start_point=input$start_date_prov)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log_prov <- renderPlotly({
    country_cases_cumulative_log_prov(country_reactive_db_prov(), start_point=input$start_date_prov)
  })
  
  # ----------------------------------------------------
  # Datos mundiales
  # ----------------------------------------------------
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date)
  })
  
  waiter_hide()
  
})

