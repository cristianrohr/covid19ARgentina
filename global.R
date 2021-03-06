# --------------------------------------------------------------
# Paquetes necesarios
# --------------------------------------------------------------
source("instalar_paquetes.R")


# --------------------------------------------------------------
# Datos
# --------------------------------------------------------------
# update data with automated script
source("jhu_data_full.R")  # John Hopkins University
source("argentina_data.R") # Argentina

# import updated data
cv_cases = read.csv("input_data/coronavirus.csv")
cv_cases_argentina <- read.csv("input_data/coronavirus_argentina.csv")

# Informacion de países
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")

# Información de Argentina
argentina <- readRDS("input_data/mapa_argentina.rds")

provincias <- argentina@data
provincias_coord <- read.csv("input_data/provincias.csv", header = T, sep = ",")

# Prediccion Población Argentina
# url <- "https://raw.githubusercontent.com/pmoracho/R/master/femicidios.ar/data/poblacion.csv"
# file <- file.path(".", "input_data/", basename(url))
# download.file(url, file)
poblacion <- read.csv("input_data/poblacion.csv", stringsAsFactors = FALSE)
poblacion <- poblacion[poblacion$year == "2020",]

# --------------------------------------------------------------
# Seteo de variables útiles
# --------------------------------------------------------------
# set mapping colour
#covid_col = "#6db69c"
covid_col = "#FF0000"



### ------------------------------------
### DATA PROCESSING: COVID-19
### World data
### ------------------------------------

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }


cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
#cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")
cv_max_date_clean = format(current_date,"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
# Agrego stats por Millon de habitantes
cv_cases$per1M = as.numeric(format(round(cv_cases$cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$newper1M = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$activeper1M = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/1000000),1),nsmall=1))


# add variable for days since 100th case and 10th death and 1th death
cv_cases$days_since_case100 <- 0
cv_cases$days_since_case10 <- 0
cv_cases$days_since_death10 <- 0
cv_cases$days_since_death1 <- 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_case10[country_db$cases>=10] = 1:sum(country_db$cases>=10)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  country_db$days_since_death1[country_db$deaths>=1] = 1:sum(country_db$deaths>=1)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_case10[cv_cases$country==country_name] = country_db$days_since_case10
  cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
  cv_cases$days_since_death1[cv_cases$country==country_name] = country_db$days_since_death1
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_Argentina = sum(cv_today$cases[cv_today$country=="Argentina"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Other"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                per1M, newper1M, activeper1M,
                                days_since_case100, days_since_case10, days_since_death10, days_since_death1)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 <- 0
cv_cases_continent$days_since_case10 <- 0
cv_cases_continent$days_since_death10 <- 0
cv_cases_continent$days_since_death1 <- 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
  continent_db$days_since_case10[continent_db$cases>=10] = 1:sum(continent_db$cases>=10)
  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
  continent_db$days_since_death1[continent_db$deaths>=1] = 1:sum(continent_db$deaths>=1)
  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
  cv_cases_continent$days_since_case10[cv_cases_continent$continent==continent_name] = continent_db$days_since_case10
  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
  cv_cases_continent$days_since_death1[cv_cases_continent$continent==continent_name] = continent_db$days_since_death1
}
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,0.01,0.1,0.5,1,10,50, 500)
cv_pal <- colorBin("Accent", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]
# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (activos)", "2019-COVID (nuevos)", "2019-COVID (acumulados)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (nuevos)", "2019-COVID (acumulados)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.Voyager) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "<small>Casos activos cada 100,000</small>") #%>%

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")
# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(12, "Set3"), brewer.pal(10, "Paired"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
#cls = rep(c(brewer.pal(8,"Set2"), brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"), brewer.pal(10, "Paired"), brewer.pal(8,"Dark2"), brewer.pal(12, "Set3")),3)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names
# Override Argentina
country_cols['Argentina'] <- "#5995da"


### ------------------------------------
### DATA PROCESSING: COVID-19
### data Argentina
### ------------------------------------

# extract dates from cv data
if (any(grepl("/", cv_cases_argentina$date))) {
  cv_cases_argentina$date = format(as.Date(cv_cases_argentina$date, format="%d/%m/%Y"),"%Y-%m-%d")
} else { cv_cases_argentina$date = as.Date(cv_cases_argentina$date, format="%Y-%m-%d") }

cv_cases_argentina$date = as.Date(cv_cases_argentina$date)
cv_min_date_argentina = as.Date(min(cv_cases_argentina$date),"%Y-%m-%d")
current_date_argentina = as.Date(max(cv_cases_argentina$date),"%Y-%m-%d")
cv_max_date_clean_argentina = format(current_date_argentina,"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases_argentina = merge(cv_cases_argentina, provincias, by.x = "argentina_ID", by.y = "NAME_1")
cv_cases_argentina = merge(cv_cases_argentina, poblacion, by.x = "argentina_ID", by.y = "provincia")
cv_cases_argentina = merge(cv_cases_argentina, provincias_coord, by.x = "argentina_ID", by.y = "provincia_nombre")

cv_cases_argentina = cv_cases_argentina[order(cv_cases_argentina$date),]
cv_cases_argentina$per100k = as.numeric(format(round(cv_cases_argentina$cases/(cv_cases_argentina$total/100000),1),nsmall=1))
cv_cases_argentina$newper100k = as.numeric(format(round(cv_cases_argentina$new_cases/(cv_cases_argentina$total/100000),1),nsmall=1))
cv_cases_argentina$activeper100k = as.numeric(format(round(cv_cases_argentina$active_cases/(cv_cases_argentina$total/100000),1),nsmall=1))
cv_cases_argentina$million_pop = as.numeric(cv_cases_argentina$total>1e6)
# Agrego stats por Millon de habitantes
cv_cases_argentina$per1M = as.numeric(format(round(cv_cases_argentina$cases/(cv_cases_argentina$total/1000000),1),nsmall=1))
cv_cases_argentina$newper1M = as.numeric(format(round(cv_cases_argentina$new_cases/(cv_cases_argentina$total/1000000),1),nsmall=1))
cv_cases_argentina$activeper1M = as.numeric(format(round(cv_cases_argentina$active_cases/(cv_cases_argentina$total/1000000),1),nsmall=1))


# add variable for days since 100th case and 10th death and 1th death
cv_cases_argentina$days_since_case100 <- 0
cv_cases_argentina$days_since_case10 <- 0
cv_cases_argentina$days_since_death10 <- 0 
cv_cases_argentina$days_since_death1 <- 0
for (i in 1:length(unique(cv_cases_argentina$argentina_ID))) {
  country_name = as.character(unique(cv_cases_argentina$argentina_ID))[i]
  country_db = subset(cv_cases_argentina, argentina_ID==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_case10[country_db$cases>=10] = 1:sum(country_db$cases>=10)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  country_db$days_since_death1[country_db$deaths>=1] = 1:sum(country_db$deaths>=1)
  cv_cases_argentina$days_since_case100[cv_cases_argentina$argentina_ID==country_name] = country_db$days_since_case100
  cv_cases_argentina$days_since_case10[cv_cases_argentina$argentina_ID==country_name] = country_db$days_since_case10
  cv_cases_argentina$days_since_death10[cv_cases_argentina$argentina_ID==country_name] = country_db$days_since_death10
  cv_cases_argentina$days_since_death1[cv_cases_argentina$argentina_ID==country_name] = country_db$days_since_death1
}

# creat variable for today's data
cv_today_argentina = subset(cv_cases_argentina, date==current_date) 
current_case_count_argentina = sum(cv_today_argentina$cases)
current_death_count_argentina = sum(cv_today_argentina$deaths)

# create subset for countries with at least 1 cases
cv_today_1_argentina = subset(cv_today_argentina, cases>=1)

# write current day's data
write.csv(cv_today_argentina %>% select(c(argentina_ID, date, update, cases, new_cases, deaths, new_deaths,
                                          recovered, new_recovered, active_cases, 
                                          per100k, newper100k, activeper100k,
                                          per1M, newper1M, activeper1M,
                                          days_since_case100, days_since_case10, days_since_death10, days_since_death1)), "input_data/coronavirus_today_argentina.csv")

# Mapa
cv_large_countries_arg = cv_today_argentina %>% filter(argentina_ID %in% argentina@data$NAME_1)
if (all(cv_large_countries_arg$argentina_ID %in% argentina@data$NAME_1)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries_arg = cv_large_countries_arg[order(cv_large_countries_arg$argentina_ID),]

argentina@data <- left_join(argentina@data,cv_large_countries_arg, by = c("NAME_1" = "argentina_ID"))

bins_argentina = c(0,0.01,0.1,0.5,1,5,50,500)
cv_pal_argentina <- colorBin("Accent", domain = argentina@data$per100k, bins = bins_argentina)
plot_map_argentina <- argentina[argentina$NAME_1 %in% cv_large_countries_arg$argentina_ID, ]

arg.center <- countries[countries$country == "Argentina", c("latitude", "longitude")]

basemap_argentina = leaflet(plot_map_argentina) %>% 
  setView(lng = arg.center$lon, lat = arg.center$lat, zoom = 4.2 ) %>%
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (acumulados)", "2019-COVID (nuevos)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (nuevos)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #addProviderTiles(providers$CartoDB.Voyager) %>%
    addLegend("bottomright", pal = cv_pal_argentina, values = ~cv_large_countries_arg$per100k,
            title = "<small>Casos acumulados cada 100,000</small>") #%>%


# sum cv case counts by date
cv_aggregated_argentina = aggregate(cv_cases_argentina$cases, by=list(Category=cv_cases_argentina$date), FUN=sum)
names(cv_aggregated_argentina) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated_argentina)) { 
  if (i==1) { cv_aggregated_argentina$new[i] = 0 }
  if (i>1) { cv_aggregated_argentina$new[i] = cv_aggregated_argentina$cases[i] - cv_aggregated_argentina$cases[i-1] }
}

# add plotting region
cv_aggregated_argentina$date = as.Date(cv_aggregated_argentina$date,"%Y-%m-%d")
# add plotting region
cv_aggregated_argentina$region <- "País"
# assign colours to provinces to ensure consistency between plots 
cls_prov = c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"))
cls_names_prov = c(as.character(unique(cv_cases_argentina$argentina_ID)))
prov_cols = cls_prov[1:length(cls_names_prov)]
names(prov_cols) = cls_names_prov
