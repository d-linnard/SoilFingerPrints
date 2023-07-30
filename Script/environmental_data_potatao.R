#remotes::install_github("mikejohnson51/AOI") # suggested!
#remotes::install_github("mikejohnson51/climateR")

library(AOI)
library(climateR)
library(dplyr)
library(sf)



# write as a function for monthly average:
envn_var = c("sph","vpd","pr", "rmin","rmax","srad",
             "tmmn","tmmx","vs","th","pdsi","pet","etr","fm100","fm1000")

env_data_monthly = function(long, lat){
  
  ts = data.frame(lng  = long, lat = lat) %>% 
    sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
    getGridMET(varname =  envn_var, 
               startDate = "2013-01-01", 
               endDate = "2022-06-30")
  
  ts$year_month = format(as.Date(ts$date), "%Y-%m")
  ts%>% group_by(year_month)
    
  
  df2<- ts[, c("year_month", "pr","vpd", "rmin", "rmax", "srad", "tmmn", "tmmx", "vs", "th",
               "pdsi.x", "pdsi.y","pet", "etr", "fm100","fm1000")]
  agg_tbl <- df2 %>% group_by(year_month) %>% 
    summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE))
  
return(data.frame(agg_tbl))
}



# write as a function for yearly average:
envn_var = c("sph","vpd","pr", "rmin","rmax","srad",
             "tmmn","tmmx","vs","th","pdsi","pet","etr","fm100","fm1000")#varialbes you want to extract


env_data_yearly= function(long, lat){
  
  ts = data.frame(lng  = long, lat = lat) %>% 
    sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
    getGridMET(varname =  envn_var, 
               startDate = "2013-01-01", 
               endDate = "2022-06-30") #package climateR
  
  ts$year = format(as.Date(ts$date), "%Y") #format the date column
  ts%>% group_by(year)
    
  
  df2<- ts[, c("year", "pr","vpd", "rmin", "rmax", "srad", "tmmn", "tmmx", "vs", "th",
               "pdsi.x", "pdsi.y","pet", "etr", "fm100","fm1000")]
  agg_tbl <- df2 %>% group_by(year) %>% 
    summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) #summarize
  
  return(data.frame(agg_tbl))
}


# long term averages 10 year average

envn_var = c("sph","vpd","pr", "rmin","rmax","srad",
             "tmmn","tmmx","vs","th","pdsi","pet","etr","fm100","fm1000")


env_data_10yr= function(long, lat){
  
  ts = data.frame(lng  = long, lat = lat) %>% 
    sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
    getGridMET(varname =  envn_var, 
               startDate = "2013-01-01", 
               endDate = "2022-06-30")
  
  df2<- ts[, c("pr","vpd","rmin", "rmax", "srad", "tmmn", "tmmx", "vs", "th",
               "pdsi.x", "pdsi.y","pet", "etr", "fm100","fm1000")]
  agg_tbl <- df2 %>%
    summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE))
  
  return(data.frame(agg_tbl))
}


#read data
df = read.csv('C:/Users/theox/Desktop/Summer Projects/SoilFingerPrints/Data/SamplingLocations/SamplingLocations.csv',
              fileEncoding="latin1")
df = df[c('ID', 'lat', 'long', 'FieldType', 'Location','SamplingYear')]
head(df)
df$FieldType = gsub('Field', "", df$FieldType)
df$FieldType = gsub('field', "", df$FieldType)
df$FieldType = gsub('Non-virgin ', "Non-virgin", df$FieldType)
df$FieldType = gsub('Virgin ', "Virgin", df$FieldType)
unique(df$FieldType)


#extract 10 year average data from all pins:
data_all_overall = c()

for (i in 1:length(df$long)){
  
  data = env_data_10yr(long = df$long[i], lat = df$lat[i])
  data_ij = cbind(df[i,], data)
  data_all_overall = rbind(data_all_overall, data_ij)
}

col_name = c("location", "latitude", "longitude", "precipitation_mean", "mean_vapor_pressure_deficit_mean",
             "minimum_near_surface_relative_humidity_mean", "maximum_near_surface_relative_humidity_mean",
             "surface_downwelling_solar_radiation_mean","minimum_near_surface_air_temperature_mean",
             "maximum_near_surface_air_temperature_mean", "wind_speed_at_10m_mean", "wind_direcation_at_10m_mean",
             "palmer_drought_severity_index_x_mean", "palmer_drought_severity_index_y_mean",
             "reference_grass_evapotranspiration_mean", "reference_alfaalfa_evapotranspiration_mean",
             "dead_fuel_moisture_100hour_mean" , "dead_fuel_moisture_1000hour_mean")
names(data_all_overall) = col_name

#write.csv(data_all_overall, "10yr_avg_environmental_data_potato.csv")



#extract yearly average data from all pins:
data_all_yearly = c()

for (i in 1:length(df$lat)){
  
  data = env_data_yearly(long = df$long[i], lat = df$lat[i])
  
  data_ij = cbind(df[i,], data)
  data_all_yearly = rbind(data_all_yearly, data_ij)
}

col_name = c("location", "latitude", "longitude", "year", "precipitation_mean", "mean_vapor_pressure_deficit_mean",
             "minimum_near_surface_relative_humidity_mean", "maximum_near_surface_relative_humidity_mean",
             "surface_downwelling_solar_radiation_mean","minimum_near_surface_air_temperature_mean",
             "maximum_near_surface_air_temperature_mean", "wind_speed_at_10m_mean", "wind_direcation_at_10m_mean",
             "palmer_drought_severity_index_x_mean", "palmer_drought_severity_index_y_mean",
             "reference_grass_evapotranspiration_mean", "reference_alfaalfa_evapotranspiration_mean",
             "dead_fuel_moisture_100hour_mean" , "dead_fuel_moisture_1000hour_mean")
names(data_all_yearly) = col_name

names(data_all_yearly)

#write.csv(data_all_yearly, "yearly_environmental_data_potato.csv")




#extract monthly average data from all pins:
data_all_monthly = c()

for (i in 1:length(df$lat)){
  
  data = env_data_monthly(long = df$long[i], lat = df$lat[i])
  data_ij = cbind(df[i,], data)
  data_all_monthly = rbind(data_all_monthly, data_ij)
}

col_name = c("location", "latitude", "longitude", "year_month", "precipitation_mean", "mean_vapor_pressure_deficit_mean",
             "minimum_near_surface_relative_humidity_mean", "maximum_near_surface_relative_humidity_mean",
             "surface_downwelling_solar_radiation_mean","minimum_near_surface_air_temperature_mean",
             "maximum_near_surface_air_temperature_mean", "wind_speed_at_10m_mean", "wind_direcation_at_10m_mean",
             "palmer_drought_severity_index_x_mean", "palmer_drought_severity_index_y_mean",
             "reference_grass_evapotranspiration_mean", "reference_alfaalfa_evapotranspiration_mean",
             "dead_fuel_moisture_100hour_mean" , "dead_fuel_moisture_1000hour_mean")
names(data_all_monthly) = col_name

names(data_all_monthly)

#write.csv(data_all_monthly, "monthly_environmental_data_potato.csv")

