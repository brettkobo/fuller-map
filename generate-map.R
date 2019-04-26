library(tidyverse)
library(tidycensus)
library(magrittr)
library(sf)
library(viridis)

#function to generate clean tempature maps
generate_temp_map <- function(raw_data) {
  map <- raw_data  %>%
    filter(!STATEFP %in% c("02", "15")) %>%
    ggplot(aes(fill = tmax_avg, color = tmax_avg)) + 
    geom_sf() + 
    scale_fill_viridis(option = "inferno", limits=c(-30, 120), guide = FALSE, na.value="white") +
    scale_color_viridis(option = "inferno", limits=c(-30, 120), na.value="white",
                        guide = guide_colorbar(
                          direction = "horizontal",
                          barheight = unit(4, units = "mm"),
                          barwidth = unit(50, units = "mm"),
                          draw.ulim = F,
                          title.position = 'top',
                          title.hjust = 0.5,
                          label.hjust = 0.5)) +
    labs(title = "Temperature in United States by Tract",
         subtitle = paste0("Average Max Tempature on ", format(date_value, format="%B %d, %Y")),
         color = "Temperature Range (F°)") +
    theme(plot.title = element_text(hjust = .5, size = 20, face = "bold"),
          plot.subtitle = element_text(hjust = .5, color = "#2F4F4F"), 
          legend.title = element_text(color = "#2F4F4F", size = 9),
          legend.position = "bottom",
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = NA),
          panel.grid.minor = element_line(colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA))
  return(map)
}

#reading the weather data in for 3-21-2018
weather_data <- read_csv("data/weather_data.csv")

#reading in the US stations
just_usa <- read_csv("data/just_usa_stations.csv")

#getting state codes to read in census data
state_abb <- fips_codes$state %>% unique()

# pulling all of the counties with the population, makes it easier to join data too
totalpop_sf_county <- reduce(
  map(state_abb[1:51], function(x) {
    get_acs(geography = "county", variables = "B01003_001", state = x, geometry = TRUE, keep_geo_vars = TRUE)
  }), 
  rbind
)

#converting stations into sf object to allow us to join it to the counry data
usa_station_points <- just_usa %>% st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(totalpop_sf_county))

#joining weather stations with census counties
station_census_tracks <- st_join(usa_station_points, totalpop_sf_county, largest = TRUE, left = FALSE)

#used to convert temparues
c_to_f <- function(c){
  ((c/10)*9/5)+32
}


#used to find the average tempature for the surroundings areas
get_neigboor_avg_temp <- function(row_id, new_data) {
  near_area <- adj[[row_id]]
  avg_temp <- new_data %>% 
    filter(row_id %in% near_area) %>%
    st_drop_geometry() %>%
    select(tmax_avg) %>%
    unlist() %>%
    mean(na.rm = TRUE)
  return(avg_temp)
}

weather_element <- "TMAX"
date_value <- as.Date("2018-03-21")

#getting one days worth of data to the census tract info    
single_day <- weather_data %>%
  filter(date == date_value, element == weather_element) %>%
  left_join(station_census_tracks, by = c("id" = "id")) %>%
  mutate(tmax = c_to_f(value))

#getting the average tempature by specific track  
value_by_tract <- single_day %>% 
  group_by(NAME.y) %>%
  summarise(tmax_avg = mean(tmax, na.rm = TRUE))

#joining the county shape files with the tempatures 
value_and_geo <- totalpop_sf_county %>% 
  left_join(value_by_tract) %>%
  mutate(row_id = 1:nrow(.))

#building the unfilled map
unfilled_map <- generate_temp_map(value_and_geo)

#saving the map
ggsave(filename = "plots/missing-value-map.png", plot = unfilled_map, units = "in", height = 8, width = 8, device = "png")

#creates vector of each counry and the it's nearest neihborhood
adj <- st_touches(value_and_geo)  

#filter for all the trackis with a missing tempature
na_temp_geo <- value_and_geo %>% 
  filter(is.na(tmax_avg))

#run through each neihboorhood and return the average values for 
missing_temps <- na_temp_geo$row_id %>% 
  lapply(get_neigboor_avg_temp, new_data = value_and_geo) %>%
  unlist() %>% 
  data.frame(filled_temp = ., row_id = na_temp_geo$row_id)

#take those missing values and add them back into the map
filled_temp_data<- value_and_geo %>% 
  left_join(missing_temps) %>%
  mutate(tmax_avg = ifelse(is.na(tmax_avg), filled_temp, tmax_avg))


filled_temp_data_a <- filled_temp_data

#building the unfilled map
unfilled_map_stage_one <- generate_temp_map(filled_temp_data_a)

#saving the map
ggsave(filename = "step-one-missing-value-map.png", plot = unfilled_map_stage_one, units = "in", height = 8, width = 8, device = "png")


#looking for missing rows
rest_of_missing_temp <- filled_temp_data_a %>% filter(is.na(tmax_avg)) %>% nrow()

#looping through the rest of the data to find missing valeues

#each loop takes one more pass at filling in missing data.

#if there are tracts that are in the middle of tracts that 
if(rest_of_missing_temp != 0) {
  print("Need to loop through the nearest hoods and find average tempature.")
}

break_num <- 0

while(rest_of_missing_temp != 0) {
  print("Still processing missing counties")
  
  looped_na_temp_geo <- filled_temp_data %>% 
    filter(is.na(tmax_avg))
  
  looped_missing_temp <- looped_na_temp_geo$row_id %>% 
    lapply(get_neigboor_avg_temp, new_data = filled_temp_data) %>%
    unlist() %>% 
    data.frame(looped_filled_temp_value = ., row_id = looped_na_temp_geo$row_id)
  
  filled_temp_data <- filled_temp_data %>% 
    left_join(looped_missing_temp, by = c("row_id" = "row_id")) %>%
    mutate(tmax_avg = ifelse(is.na(tmax_avg), looped_filled_temp_value, tmax_avg)) %>%
    select(-looped_filled_temp_value)
  
  rest_of_missing_temp <- filled_temp_data %>% filter(is.na(tmax_avg)) %>% nrow()
  
  break_num <- break_num + 1
  if(break_num == 4) {
    break
  }
  
}

#building the unfilled map
full_map <- generate_temp_map(filled_temp_data) 

#saving the map
ggsave(filename = "plots/not-missing-value-map.png", plot = full_map, units = "in", height = 8, width = 8, device = "png")