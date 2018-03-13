library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(rworldmap)
library(ggmap)

if('source_data.csv' %in% dir('data')){
  source_data <- read_csv('data/source_data.csv')
} else {
  
  
  # read in data - DWT - dead weight tonnage (tons) - how much the ship can carry
  source_data <- read_excel('data/ShippingSourceData.xlsx',
                            sheet = 'Source')
  
  # get lat and lon data
  if('city_lat_lon.csv' %in% dir('data')){
    city_dat_lon <- read_csv('data/city_lat_lon.csv')
  } else {
    # run funtion
    get_lat_lon()
    #read in data
    city_dat_lon <- read_csv('data/city_lat_lon.csv')
  }
  
  # check to make sure we have all the lat lon for city names
  unique_city_names <- sort(unique(c(source_data$`Discharge Port`, source_data$`Load Port`)))
  stopifnot(sort(unique(city_dat_lon$city_names)) == unique_city_names)
 
  # join lat lon got Load Port and Discharge Port
  source_data$`Load Port Lat` <- 
    source_data$`Load Port Lon` <-
    source_data$`Discharge Port Lat` <-
    source_data$`Discharge Port Lon` <- 
    NA
  
  for(i in unique_city_names){
    # get load port lat lon
    source_data$`Load Port Lat`[which(source_data$`Load Port` == i)] <- city_dat_lon$lat[which(city_dat_lon$city_names == i)]
    source_data$`Load Port Lon`[which(source_data$`Load Port` == i)] <- city_dat_lon$lon[which(city_dat_lon$city_names == i)]
    
    # get Discharge port lat lon
    source_data$`Discharge Port Lat`[which(source_data$`Discharge Port` == i)] <- city_dat_lon$lat[which(city_dat_lon$city_names == i)]
    source_data$`Discharge Port Lon`[which(source_data$`Discharge Port` == i)] <- city_dat_lon$lon[which(city_dat_lon$city_names == i)]
    
  }
  
  # add more variables - indicator for when weight is greater than DWT - overcappacity
  source_data$Overcapacity <- ifelse(source_data$Weight > source_data$DWT, 'Overcapacity',
                                     ifelse(is.na(source_data$Weight) | is.na(source_data$DWT), NA, 'No Overcapacity'))
  
  # numeric for difference between weight and DWT
  source_data$`DWT And Weight Difference ` <- source_data$DWT - source_data$Weight
  
  # recode Trading Co
  source_data$`Trading Co` <- ifelse(is.na(source_data$`Trading Co`), 'Unknown', source_data$`Trading Co`)
  
  # turn dat variables into date object
  source_data$`Arrival Date (Load)` <- as.Date(source_data$`Arrival Date (Load)`)
  source_data$`Departure Date (Load)` <- as.Date(source_data$`Departure Date (Load)`)
  source_data$`Arrival Date (Discharge)` <- as.Date(source_data$`Arrival Date (Discharge)`)
  source_data$`Departure Date (Discharge)` <- as.Date(source_data$`Departure Date (Discharge)`)
  
  # # # fix dates 
  # source_data <- fix_dates(source_data)
  # 
  # save data to data 
  write_csv(source_data, 'data/source_data.csv')
  
}

date_cols_load <- c("Arrival Date (Load)","Departure Date (Load)")
date_cols_dis <- c("Arrival Date (Discharge)","Departure Date (Discharge)" )

# get unique choices for each variable
load_country <- sort(unique(source_data$`Load Country`))
discharge_country <- sort(unique(source_data$`Discharge Country`))


# get start and end
ending_date <-  as.Date(Sys.time())
beginning_date <- min(source_data$`Departure Date (Load)`, na.rm = TRUE)

# load world map
world <- map_data(map="world")



