library(rjson)
library(httr)
library(geosphere)

config = fromJSON(file = '../../keys.json')

select_track <- function(track_name = 'ibiai'){
  track_inputs <- list(
    ibiai = data.frame(path = '-17.00379589,-44.7916|-17.0328,-44.7708', samples = 200),
    oeste_baiano = data.frame(path = '-12.6473,-45.9213|-12.2066,-45.8300', samples = 500)
  )
  return(track_inputs[[track_name]])
}

get_track_data <- function(track_name = 'ibiai'){
  track_inputs <- select_track(track_name)
  
  res = GET("https://maps.googleapis.com/maps/api/elevation/json",
            query = list(path = track_inputs$path,samples = track_inputs$samples, key = config[['google_maps']][['key']]))
  
  data = fromJSON(rawToChar(res$content))
  
  
  for(i in 1:track_inputs$samples){
    elevation <- data$results[[i]]$elevation
    lat <-  data$results[[i]]$location$lat
    lng <-  data$results[[i]]$location$lng
    dS <- ifelse(i == 1, 0, distm(
      c(lng, lat),
      c(track_data_df$lng[i-1], track_data_df$lat[i-1]), 
      fun = distHaversine))
    resolution <- data$results[[i]]$resolution
    track_data_temp <- data.frame(lat = lat, lng = lng, dS = dS, height = elevation, resolution = resolution)
    if(i == 1){track_data_df <- track_data_temp}else{track_data_df <- rbind(track_data_df,  track_data_temp)}
  }
  track_data_df$path = cumsum(track_data_df$dS)
  return(track_data_df)
}

