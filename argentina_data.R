## Autor: Cristian Rohr
## Los datos para casos a nivel de provincias fueron obtenidos originalmente del siguiente repositorio
## https://github.com/martingra/COVID19Argentina
## Sin embargo lo dejo de actualizar por lo tanto comenze a llevar mi propio registro en el siguiente repo
## https://github.com/cristianrohr/covid19ARgentina_stats


# load libraries
update_argentina = function(input_df, tag) {
  names(input_df)[1:2] = c("Province", "Country")
  input_df$Country = input_df$Country %>% str_replace_all(., " ", "") 
  colnames(input_df) <- gsub(pattern = "/2020","/20", colnames(input_df))
  colnames(input_df) <- gsub(pattern = "^3/","03/", colnames(input_df))
  dates = names(input_df)[which(names(input_df)=="03/01/20"):ncol(input_df)]
  input_df = input_df %>% 
    select(-c(Country, Lat, Long)) %>% 
    group_by(Province) %>% 
    summarise_each(funs(sum)) %>%
    data.frame()
  rownames(input_df) = input_df$Province
  rownames(input_df) = paste0(input_df$Province,"_",tag)
  input_df = input_df %>% select(-c(Province)) %>% t()
  input_df = data.frame(input_df)
  input_df$Date = dates
  rownames(input_df) = 1:nrow(input_df)
  input_df$Date = format(as.Date(input_df$Date,"%m/%d/%y"))
  input_df
}


# confirmed cases
argentina_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/cristianrohr/covid19ARgentina_stats/master/data/time_series_19-covid-Confirmed_argentina.csv"))
#argentina_cases <- as.data.frame(data.table::fread("input_data/time_series_19-covid-Confirmed_argentina.csv"))
argentina_cases[is.na(argentina_cases)]=0
total_cases_argentina <- sum(argentina_cases[,ncol(argentina_cases)])
argentina_cases = update_argentina(argentina_cases, "cases")
if (total_cases_argentina != sum(argentina_cases[nrow(argentina_cases),1:(ncol(argentina_cases)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# deaths
argentina_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/cristianrohr/covid19ARgentina_stats/master/data/time_series_19-covid-Deaths_argentina.csv"))
#argentina_deaths <- as.data.frame(data.table::fread("input_data/time_series_19-covid-Deaths_argentina.csv"))
argentina_deaths[is.na(argentina_deaths)]=0
total_deaths_argentina <- sum(argentina_deaths[,ncol(argentina_deaths)])
argentina_deaths = update_argentina(argentina_deaths, "deaths")
if (total_deaths_argentina!=sum(argentina_deaths[nrow(argentina_deaths),1:(ncol(argentina_deaths)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# recovered
argentina_rec <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/cristianrohr/covid19ARgentina_stats/master/data/time_series_19-covid-Recovered_argentina.csv"))
#argentina_rec <- as.data.frame(data.table::fread("input_data/time_series_19-covid-Recovered_argentina.csv"))
argentina_rec[is.na(argentina_rec)]=0
total_rec_argentina <- sum(argentina_rec[,ncol(argentina_rec)])
argentina_rec = update_argentina(argentina_rec, "recovered")
if (total_rec_argentina!=sum(argentina_rec[nrow(argentina_rec),1:(ncol(argentina_rec)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

argentina_cases <- argentina_cases[-c(1:2),]
argentina_deaths <- argentina_deaths[-c(1:2),]
argentina_rec <- argentina_rec[-c(1:2),]

# merge dataframes 
argentina_merge = merge(argentina_cases, argentina_deaths, by = "Date")
argentina_merge = merge(argentina_merge, argentina_rec, by = "Date")
argentina_merge$Date = as.Date(argentina_merge$Date, format="%Y-%m-%d")
argentina_merge$update = 1:nrow(argentina_merge)
write.csv(argentina_merge, "input_data/argentina_data.csv")

# Descarga del Shapefile de Argentina
# tmp <- tempdir()
# url <- "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip"
# file <- file.path(".", "input_data",basename(url))
# download.file(url, file)
# unzip(file, exdir = tmp)
# Leo el Shapefile de Argentina
#argentina <- readOGR(dsn = "input_data/mapa_argentina/", layer = "ARG_adm1", use_iconv=TRUE, encoding='UTF-8', stringsAsFactors=FALSE)

# Creo mi propio shapefile mas liviano
# Provincias <- getData('GADM', country='ARG', level=1) # Cargar datos de Provincias
# ProvinciasSimple <- gSimplify(Provincias, tol = 0.1 , topologyPreserve = TRUE) # reducir complejidad
# ProvinciasSimple <- SpatialPolygonsDataFrame(ProvinciasSimple, data.frame(Provincias))
# saveRDS(ProvinciasSimple, "input_data/mapa_argentina.rds")
argentina <- readRDS("input_data/mapa_argentina.rds")
provincias <- argentina@data

collated_data_argentina = NULL
# loop to add new data for each new situation report
for (i in c(1:nrow(argentina_merge))) {
  
  message(i)
  
  # extract subset of data for date in row i
  argentina_subset = argentina_merge[i,]
  if(i %in% c(1,2)) {
    argentina_subset_cases = data.frame(Capital.Federal_cases = 1)
    argentina_subset_deaths = argentina_subset[,which(grepl("_deaths", names(argentina_subset)))]
    argentina_subset_rec = argentina_subset[,which(grepl("_recovered", names(argentina_subset)))]
  } else if(i == 3) {
    argentina_subset_cases = data.frame(Capital.Federal_cases = 2)
    argentina_subset_deaths = argentina_subset[,which(grepl("_deaths", names(argentina_subset)))]
    argentina_subset_rec = argentina_subset[,which(grepl("_recovered", names(argentina_subset)))]
  } else  {
    argentina_subset_cases = argentina_subset[,which(grepl("_cases", names(argentina_subset)))]
    argentina_subset_cases = argentina_subset_cases[,which(colSums(argentina_subset_cases)>0)]
    argentina_subset_deaths = argentina_subset[,which(grepl("_deaths", names(argentina_subset)))]
    argentina_subset_rec = argentina_subset[,which(grepl("_recovered", names(argentina_subset)))]  
  }
  
  
  # build new dataframe to add updated data
  new_data = data.frame(argentina_ID = names(argentina_subset_cases) %>% str_replace_all(., "_cases", ""),
                        date = format(as.Date(argentina_subset$Date[1],"%Y-%m-%d")),
                        update = i,
                        cases = NA, new_cases = 0,
                        deaths = 0, new_deaths = 0,
                        recovered = 0, new_recovered = 0)
  
  # update column names in new_jhu dataframes to include country names only
  colnames(argentina_subset_cases) = colnames(argentina_subset_cases) %>% str_replace_all(., "_cases", "") 
  colnames(argentina_subset_deaths) = colnames(argentina_subset_deaths) %>% str_replace_all(., "_deaths", "") 
  colnames(argentina_subset_rec) = colnames(argentina_subset_rec) %>% str_replace_all(., "_recovered", "")
  
  # loop to update cases
  for (j in 1:nrow(new_data)) {
    # update case numbers
    country_name = as.character(new_data$argentina_ID[j])
    new_data$cases[j] = argentina_subset_cases[,country_name]
    new_data$deaths[j] = argentina_subset_deaths[,country_name]
    new_data$recovered[j] = argentina_subset_rec[,country_name]
  }
  
  # append new data to collated dataframe
  collated_data_argentina = rbind(collated_data_argentina, new_data)
  collated_data_argentina$argentina_ID = as.character(collated_data_argentina$argentina_ID)
  
  # calculate new cases, deaths and recoveries
  if (i == 1) {
    collated_data_argentina$new_cases = collated_data_argentina$cases
    collated_data_argentina$new_deaths = collated_data_argentina$deaths
    collated_data_argentina$new_recovered = collated_data_argentina$recovered
  }
  
  if (i > 1) {
    # split it into date i and date i-1
    today = subset(collated_data_argentina, update==i)
    yesterday = subset(collated_data_argentina, update==(i-1))
    
    for (k in 1:nrow(today)) {
      country_name = today$argentina_ID[k]
      
      # if present in yesterday's data, calculate new cases by subtraction
      if (country_name %in% yesterday$argentina_ID) {
        collated_data_argentina$new_cases[collated_data_argentina$argentina_ID==country_name & collated_data_argentina$update==i] = today$cases[today$argentina_ID==country_name] - yesterday$cases[yesterday$argentina_ID==country_name] 
        collated_data_argentina$new_deaths[collated_data_argentina$argentina_ID==country_name & collated_data_argentina$update==i] = today$deaths[today$argentina_ID==country_name] - yesterday$deaths[yesterday$argentina_ID==country_name] 
        collated_data_argentina$new_recovered[collated_data_argentina$argentina_ID==country_name & collated_data_argentina$update==i] = today$recovered[today$argentina_ID==country_name] - yesterday$recovered[yesterday$argentina_ID==country_name] 
      } else {
        # if absent from yesterday's data, new observations = total observations
        collated_data_argentina$new_cases[collated_data_argentina$argentina_ID==country_name & collated_data_argentina$update==i] = today$cases[today$argentina_ID==country_name] 
        collated_data_argentina$new_deaths[collated_data_argentina$argentina_ID==country_name & collated_data_argentina$update==i] = today$deaths[today$argentina_ID==country_name]  
        collated_data_argentina$new_recovered[collated_data_argentina$argentina_ID==country_name & collated_data_argentina$update==i] = today$recovered[today$argentina_ID==country_name] 
      }
    }
  }
}
# allow for repatriation or reassigned cases without negative new_cases, new_deaths and new_recovered counts
collated_data_argentina$new_cases[collated_data_argentina$new_cases<0] = 0
collated_data_argentina$new_deaths[collated_data_argentina$new_deaths<0] = 0
collated_data_argentina$new_recovered[collated_data_argentina$new_recovered<0] = 0

# add active case data (total cases - deaths/recovered)
collated_data_argentina$active_cases = collated_data_argentina$cases - (collated_data_argentina$deaths + collated_data_argentina$recovered)

# update provinces names
provincias$NAME_1
collated_data_argentina[collated_data_argentina$argentina_ID == "Buenos.Aires","argentina_ID"] <- "Buenos Aires"
collated_data_argentina[collated_data_argentina$argentina_ID == "Capital.Federal","argentina_ID"] <- "Ciudad de Buenos Aires"
collated_data_argentina[collated_data_argentina$argentina_ID == "Cordoba","argentina_ID"] <- "Córdoba"
collated_data_argentina[collated_data_argentina$argentina_ID == "Entre.Rios","argentina_ID"] <- "Entre Ríos"
collated_data_argentina[collated_data_argentina$argentina_ID == "La.Pampa","argentina_ID"] <- "La Pampa"
collated_data_argentina[collated_data_argentina$argentina_ID == "La.Rioja","argentina_ID"] <- "La Rioja"
collated_data_argentina[collated_data_argentina$argentina_ID == "Neuquen","argentina_ID"] <- "Neuquén"
collated_data_argentina[collated_data_argentina$argentina_ID == "Rio.Negro","argentina_ID"] <- "Río Negro"
collated_data_argentina[collated_data_argentina$argentina_ID == "San.Juan","argentina_ID"] <- "San Juan"
collated_data_argentina[collated_data_argentina$argentina_ID == "San.Luis","argentina_ID"] <- "San Luis"
collated_data_argentina[collated_data_argentina$argentina_ID == "Santa.Cruz","argentina_ID"] <- "Santa Cruz"
collated_data_argentina[collated_data_argentina$argentina_ID == "Santa.Fe","argentina_ID"] <- "Santa Fe"
collated_data_argentina[collated_data_argentina$argentina_ID == "Santiago.del.Estero","argentina_ID"] <- "Santiago del Estero"
collated_data_argentina[collated_data_argentina$argentina_ID == "Tierra.del.Fuego","argentina_ID"] <- "Tierra del Fuego"
collated_data_argentina[collated_data_argentina$argentina_ID == "Tucuman","argentina_ID"] <- "Tucumán"

# re-order
collated_data_argentina = collated_data_argentina[order(as.Date(collated_data_argentina$date, format="%Y-%m-%d"), -collated_data_argentina$cases),]

# update time stamp
collated_data_argentina$last_update = NA
collated_data_argentina$last_update[nrow(collated_data_argentina)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %H:00"), "GMT")

# save file
write.csv(collated_data_argentina, "input_data/coronavirus_argentina.csv", row.names=F)
rm(list = ls())
