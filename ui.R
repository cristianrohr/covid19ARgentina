#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage(theme = shinytheme("yeti"), collapsible = TRUE,
             "Dashboard COVID-19 Argentina", id="nav",
             tabPanel("Mapa Argentina",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap_argentina", width="100%", height="100%"),
                          absolutePanel(id = "controlsargentina", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        h3(textOutput("reactive_case_count_argentina"), align = "right"),
                                        h4(textOutput("reactive_death_count_argentina"), align = "right", style="color:#ed1c24"),
                                        sliderInput("plot_date_argentina",
                                                    label = h5("Seleccione fecha de mapeo"),
                                                    min = as.Date(cv_min_date_argentina,"%Y-%m-%d"),
                                                    max = as.Date(current_date_argentina,"%Y-%m-%d"),
                                                    value = as.Date(current_date_argentina),
                                                    timeFormat = "%d %b",
                                                    animate=animationOptions(interval = 2000, loop = FALSE))
                          )
                      )
             ),
             tabPanel("Mapa Mundial",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right", style="color:#ed1c24"),
                                        span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                        span(h4(textOutput("reactive_new_cases_24h"), align = "right"), style="color:#b4333d"),
                                        #span(h4(textOutput("reactive_case_count_Argentina"), align = "right")),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        tags$i(h6("Actualizado diariamente. Para actualizaciones regulares, visite: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                        tags$i(h6("Los casos reportados presentan variación debido a la capacidad de testeo de cada país.")),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        sliderInput("plot_date",
                                                    label = h5("Seleccione fecha de mapeo"),
                                                    min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                    max = as.Date(current_date,"%Y-%m-%d"),
                                                    value = as.Date(current_date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = 2000, loop = FALSE))
                          )
                          #,
                          
                          # absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                          #               tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          
                          # absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                          #                             actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                          #                                          onclick = sprintf("window.open('%s')", 
                          #                                           "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          
                          
                      )
             ),
             
             
             
             tabPanel("Detalle Argentina",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("outcome_select_arg", "Desenlace:",   
                                      choices = c("Casos", "Muertes"), 
                                      selected = c("Casos"),
                                      multiple = FALSE),
                          pickerInput("start_date_arg", "Fecha inicio gráfico:",   
                                      choices = c("Fecha", "Día del caso 100", "Día de la 10ma muerte"), 
                                      options = list(`actions-box` = TRUE),
                                      selected = "Fecha",
                                      multiple = FALSE)
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Diarios", plotlyOutput("country_plot_arg")),
                            tabPanel("Acumulado", plotlyOutput("country_plot_cumulative_arg")),
                            tabPanel("Acumulado (log10)", plotlyOutput("country_plot_cumulative_log_arg"))
                          )
                        )
                      )
             ),
             tabPanel("Detalle por Provincia",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("region_select_prov", "Provincia:",   
                                      choices = as.character(sort(cv_today_1_argentina[order(-cv_today_1_argentina$cases),]$argentina_ID)), 
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Por favor seleccione una opción!"),
                                      selected = cv_today_1_argentina$argentina_ID,
                                      multiple = TRUE), 
                          
                          pickerInput("outcome_select_prov", "Desenlace:",   
                                      choices = c(Casos = "Cases", Muertes = "Deaths"), 
                                      selected = c("Cases"),
                                      multiple = FALSE),
                          
                          pickerInput("start_date_prov", "Fecha inicio gráfico:",   
                                      choices = c("Fecha", "Día del caso 100", "Día de la 10ma muerte"), 
                                      options = list(`actions-box` = TRUE),
                                      selected = "Fecha",
                                      multiple = FALSE), 
                          "Seleccione Desenlace, regiones y fecha de inicio para actualizar los gráficos. Se muestras provincias con almenos 1 caso."
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Diarios", plotlyOutput("country_plot_prov")),
                            tabPanel("Acumulado", plotlyOutput("country_plot_cumulative_prov")),
                            tabPanel("Acumulado (log10)", plotlyOutput("country_plot_cumulative_log_prov"))
                          )
                        )
                      )
             ),
             tabPanel("Detalle Mundial",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("level_select", "Nivel:",   
                                      choices = c(Mundial = "Global", Continente = "Continent", País = "Country"), 
                                      selected = c("Country"),
                                      multiple = FALSE),
                          
                          pickerInput("region_select", "País/Region:",   
                                      choices = as.character(sort(cv_today_100[order(-cv_today_100$cases),]$country)), 
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Por favor seleccione una opción!"),
                                      #selected = cv_today_100$country,
                                      selected = c("Argentina", "Brazil", "USA", "Italy", "Spain", "Chile", "Ecuador", "Uruguay", "Mainland China"),
                                      multiple = TRUE), 
                          
                          pickerInput("outcome_select", "Desenlace:",   
                                      choices = c(Casos = "Cases", Muertes = "Deaths"), 
                                      selected = c("Cases"),
                                      multiple = FALSE),
                          
                          pickerInput("start_date", "Fecha inicio gráfico:",   
                                      choices = c("Fecha", "Día del caso 100", "Día de la 10ma muerte"), 
                                      options = list(`actions-box` = TRUE),
                                      selected = "Fecha",
                                      multiple = FALSE), 
                          "Seleccione Desenlace, regiones y fecha de inicio para actualizar los gráficos. Se muestras países con almenos 100 casos."
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Diarios", plotlyOutput("country_plot")),
                            tabPanel("Acumulado", plotlyOutput("country_plot_cumulative")),
                            tabPanel("Acumulado (log10)", plotlyOutput("country_plot_cumulative_log"))
                          )
                        )
                      )
             ),
             tabPanel("Acerca de este sitio",
                      tags$div(
                        tags$h4("Last update"), 
                        h6(paste0(update)),
                        "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                        tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                        tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                        "The aim of this site is to complement the above resources by providing several interactive features not currently available elsewhere, including the timeline function, 
                        the ability to overlay past outbreaks, and an emphasis on normalised counts (per 100,000 individuals).",tags$br(),
                        tags$br(),tags$h4("Background"), 
                        "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                        tags$br(),tags$br(),
                        "In isolation, these headlines can be hard to interpret. 
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                        tags$br(),tags$br(),
                        "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                        "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                        tags$br(),tags$br(),tags$h4("Sources"),
                        tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                        " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                        # tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
                        # tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                        # tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                        # tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                        # tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
                        # substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
                        # The upper limit of this range is used for illustrative purposes in the Outbreak comarisons tab.",tags$br(),
                        # tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                        tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github"),tags$br(),
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                        "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                        tags$br(),tags$br(),tags$h4("Contact"),
                        "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                        tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                      )
             )
  )
  
)
