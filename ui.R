#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

hostname.sistema <- Sys.info()["nodename"]
hostname.sistema <- "argentum"
if(hostname.sistema == "argentum") {
  aboutPanel <- tabPanel("Acerca de este sitio",
                         tags$div(
                           
                           tags$h4("¿Porque este dashboard?"),
                           
                           "La aplicación funda las bases sobre la cual agregaremos en Héritas capas de datos de genomas virales a
                           ser secuenciadas en nuestras plataformas NGS. Dado su utilidad para visualización, es 
                           de código abierto y cada uno puede aportar lo que crea necesario.",
                           
                           tags$br(),
                           tags$br(),tags$h4("Datos argentina"), 
                           "El Ministerio de Salud provee ", tags$a(href="https://www.argentina.gob.ar/coronavirus/informe-diario", "informes diarios "), " con la cantidad de casos y fallecidos por provincia.",
                           "Dicha información es parseada y subida al repositorio de github ", tags$a(href="https://github.com/cristianrohr/covid19ARgentina_stats", "covid19ARgentina_stats"),
                           "Dicho repositorio tomo como base el trabajo realizado por usuario de github ", tags$a(href="https://github.com/martingra/COVID19Argentina", "martingra"),
                           
                           tags$br(),
                           tags$br(),
                           tags$h4("Créditos"), 
                           "La idea original fue tomada del siguiente repositorio", tags$a(href="https://github.com/eparker12/nCoV_tracker", "eparker12/nCoV_tracker. "), 
                           "El objetivo del repositorio original era proveer una forma de mostrar la información a modo de 'timeline' y la información normalizada cada 1000,000 habitantes.", tags$br(),
                           "Cristian Rohr, Científico de Datos de Héritas creo una version customizada (", tags$a(href="https://github.com/cristianrohr/covid19ARgentina", "cristianrohr/covid19ARgentina"), ")", "del dashboard original, centrada en Argentina, con la idea de brindar granularidad a nivel de provincia
                           con información respecto a la evolución de la pandemia por COVID19.",
                           
                           tags$br(),tags$br(),tags$h4("Orígenes de datos"),
                           tags$p(tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports.")),
                           tags$p(tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github")),
                           tags$p(tags$b("Reportes Ministerio de Salud: "), tags$a(href="https://www.argentina.gob.ar/coronavirus/informe-diario", "Reporte diario")),
                           tags$p(tags$b("Datos argentina: "), tags$a(href="https://github.com/cristianrohr/covid19ARgentina_stats", "covid19ARgentina_stats")),
                           tags$p(tags$b("Coordenadas Argentina: "), tags$a(href="http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip", "Shapefile Argentina")),
                           tags$p(tags$b("Estimado población por provincia: "), tags$a(href="https://raw.githubusercontent.com/pmoracho/R/master/femicidios.ar/data/poblacion.csv", "habitantes por provincia")),
                           tags$br(),
                           tags$br(),tags$h4("Sugerencias"), 
                           "Sitios interesantes:", tags$br(),
                           tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                           tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),
                           
                           tags$br(),
                           tags$br(),tags$h4("Contacto"),
                           "cristian.rohr@heritas.com.ar Plataforma Ciencia de Datos - Héritas", tags$br(),
                           "martin.vazquez@heritas.com.ar Director Científico - Héritas",
                           tags$br(),
                           tags$br(),
                           tags$br()
                         )
  )
  
  tema <- "heritas.css"
  
  links_redes1 <- absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                tags$a(href='https://heritas.com.ar', target='_blank', tags$img(src='favicon.png',height='48',width='48')))
  
  links_redes2 <- absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                             onclick = sprintf("window.open('%s')",
                                                               "https://twitter.com/HeritasArg")))
  intervaloanimacion <- 4000
} else {
  aboutPanel <- tabPanel("Acerca de este sitio",
                         tags$div(
                           tags$h4("Idea original"), 
                           "La idea original fue tomada del siguiente repositorio", tags$a(href="https://github.com/eparker12/nCoV_tracker", "eparker12/nCoV_tracker"), tags$br(),
                           "El objetivo del repositorio original era proveer una forma de mostrar la información a modo de 'timeline' y la información normalizada cada 1000,000 habitantes.",
                           "Se publicó un articulo sobre este sitio en ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                           "Y el mapa también aparecio en el programa BBC World Service ",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                           
                           tags$br(),
                           tags$br(),tags$h4("Datos argentina"), 
                           "Si bien el Ministerio de Salud provee ", tags$a(href="https://www.argentina.gob.ar/coronavirus/informe-diario", "informes diarios "), " la información no se encuentra tabulada, se entrega en formato pdf.",
                           "El usuario de ", tags$a(href="https://github.com/martingra/COVID19Argentina", "github martingra"),  "se encargo de generar información tabulada sin embargo dejo de actualizar el repositorio el 29 de marzo, por lo cual cree mi propio repositorio con información actualizada ",
                           tags$a(href="https://github.com/cristianrohr/covid19ARgentina_stats", "covid19ARgentina_stats"),
                           
                           tags$br(),tags$br(),tags$h4("Autor"),
                           "Msc. Cristian Rohr",tags$br(),
                           "Máster en Ciencia de Datos y Tecnologías Inteligentes por la Universidad de Granada - España y Licenciado en Bioinformática por la Universidad Nacional de Entre Ríos - Argentina",tags$br(),
                           "Actualmente trabajando en la Plataforma de Ciencia de Datos en ", tags$a(href="http://heritas.com.ar", "Héritas"),tags$br(),
                           tags$br(),tags$h4("Contacto"),
                           "cristianrohr768@gmail.com",tags$br(),
                           icon("linkedin"), tags$a(href="https://www.linkedin.com/in/cristianrohrbio", "https://www.linkedin.com/in/cristianrohrbio"), tags$br(),
                           icon("github"),  tags$a(href="http://cristianrohr.github.io/", "http://cristianrohr.github.io/"), tags$br(),
                           icon("github"),  tags$a(href="https://github.com/cristianrohr", "https://github.com/cristianrohr"), tags$br(),
                           
                           
                           tags$br(),tags$h4("Sugerencias"), 
                           "Para información más actualizada sobre la pandemia puede visitar los siguientes sitios:", tags$br(),
                           tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                           tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                           
                           tags$br(),tags$br(),tags$h4("Código"),
                           "El código y datos de entrada para generar el dashboard se encuentran disponibles en ",tags$a(href="https://github.com/cristianrohr/covid19ARgentina", "Github."),
                           tags$br(),tags$br(),tags$h4("Orígenes"),
                           tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," con información adicional de ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports, "),
                           tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github, "),
                           tags$b("Datos argentina: "), tags$a(href="https://github.com/cristianrohr/covid19ARgentina_stats", "covid19ARgentina_stats, "),
                           tags$b("Reportes Ministerio de Salud: "), tags$a(href="https://www.argentina.gob.ar/coronavirus/informe-diario", "Reporte diario, "),
                           tags$b("Coordenadas Argentina: "), tags$a(href="http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip", "Shapefile Argentina, "),
                           tags$b("Estimado población por provincia: "), tags$a(href="https://raw.githubusercontent.com/pmoracho/R/master/femicidios.ar/data/poblacion.csv", "habitantes por provincia"),
                           tags$br(),
                           tags$br(),tags$br(),tags$br()
                         )
  )
  
  tema <- "flatly.css"
  
  links_redes1 <- absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                tags$a(href='https://github.com/cristianrohr', target='_blank', icon("github", "fa-3x")))
  
  links_redes2 <- absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                             onclick = sprintf("window.open('%s')",
                                                               "https://twitter.com/cristianrohr")))
  intervaloanimacion <- 2000
}


library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    theme = tema,
    collapsible = TRUE,
             "Dashboard COVID-19 Argentina", id="nav",
             tabPanel("Mapa Argentina",
                      use_waiter(),
                      waiter_show_on_load(spin_flower(), color = "#161616"),
                      div(class="outer",
                          tags$head(includeCSS("styles.css"),
                                    setBackgroundColor(color = "ghostwhite"),
                                    setShadow(class = "well")),
                          tags$br(),
                          leafletOutput("mymap_argentina", width="100%", height="99%"),
                          absolutePanel(id = "controlsargentina", class = "panel panel-default",
                                        top = 80, left = 20, width = "18%", fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        h3(textOutput("reactive_case_count_argentina"), align = "right"),
                                        h4(textOutput("reactive_death_count_argentina"), align = "right", style="color:#ed1c24"),
                                        span(h4(textOutput("reactive_recovered_count_arg"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_active_count_arg"), align = "right"), style="color:#cc4c02"),
                                        span(h4(textOutput("reactive_new_cases_24h_arg"), align = "right"), style="color:#b4333d"),
                                        tags$i(h6("Actualizado diariamente. ", tags$a(href="https://github.com/cristianrohr/covid19ARgentina_stats", "Datos COVID19 Argentina."))),
                                        plotOutput("cumulative_plot_arg", height = "12em", width="100%"),
                                        sliderInput("plot_date_argentina",
                                                    label = h5("Seleccione fecha de mapeo"),
                                                    min = as.Date(cv_min_date_argentina,"%Y-%m-%d"),
                                                    max = as.Date(current_date_argentina,"%Y-%m-%d"),
                                                    value = as.Date(current_date_argentina),
                                                    timeFormat = "%d %b",
                                                    animate=animationOptions(interval = intervaloanimacion, loop = FALSE,
                                                                             playButton = icon('play', "fa-2x"))),
                                        tags$i(h6("Click en botón PLAY (botón azul) para animación de evolución de casos."))
                          ),
                          links_redes1,
                          links_redes2
                      ),
                      tags$br(),
                      tags$br()
             ),
             tabPanel("Mapa Mundial",
                      div(class="outer",
                          tags$br(),
                          leafletOutput("mymap", width="100%", height="99%"),
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = "18%", fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right", style="color:#ed1c24"),
                                        span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                        span(h4(textOutput("reactive_new_cases_24h"), align = "right"), style="color:#b4333d"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        tags$i(h6("Actualizado diariamente. Para actualizaciones regulares, visite: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                        tags$i(h6("Los casos reportados presentan variación debido a la capacidad de testeo de cada país.")),
                                        plotOutput("cumulative_plot", height="12em", width="100%"),
                                        sliderInput("plot_date",
                                                    label = h5("Seleccione fecha de mapeo"),
                                                    min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                    max = as.Date(current_date,"%Y-%m-%d"),
                                                    value = as.Date(current_date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = intervaloanimacion, loop = FALSE,
                                                                             playButton = icon('play', "fa-2x"))),
                                        tags$i(h6("Click en botón PLAY (botón azul) para animación de evolución de casos.")),
                                        style = "z-index: 420;"
                          ),
                          
                          links_redes1,
                          links_redes2
                          
                          
                      ),
                      tags$br(),
                      tags$br()
             ),
             
             
             
             tabPanel("Detalle Argentina",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("outcome_select_arg", "Desenlace:",   
                                      choices = c("Casos", "Muertes"), 
                                      selected = c("Casos"),
                                      multiple = FALSE),
                          pickerInput("start_date_arg", "Fecha inicio gráfico:",   
                                      #choices = c("Fecha", "Día del caso 100", "Día de la 10ma muerte"),
                                      choices = c("Fecha", "Día del caso 100", "Día del caso 10", "Día de la 10ma muerte", "Día de la 1ra muerte"), 
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
                                      #choices = c("Fecha", "Día del caso 100", "Día de la 10ma muerte"), 
                                      choices = c("Fecha", "Día del caso 100", "Día del caso 10", "Día de la 10ma muerte", "Día de la 1ra muerte"), 
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
                                      selected = c("Argentina", "Brazil", "USA", "Italy", "Spain", "Chile", "Ecuador", "Uruguay", "Mainland China"),
                                      multiple = TRUE), 
                          
                          pickerInput("outcome_select", "Desenlace:",   
                                      choices = c(Casos = "Cases", Muertes = "Deaths"), 
                                      selected = c("Cases"),
                                      multiple = FALSE),
                          
                          pickerInput("start_date", "Fecha inicio gráfico:",   
                                      #choices = c("Fecha", "Día del caso 100", "Día de la 10ma muerte"), 
                                      choices = c("Fecha", "Día del caso 100", "Día del caso 10", "Día de la 10ma muerte", "Día de la 1ra muerte"), 
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
             aboutPanel
  )
  
)
