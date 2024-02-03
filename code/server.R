#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike_df = city_weather_bike_df %>% 
                        group_by(CITY_ASCII) %>%
                        arrange(desc(BIKE_PREDICTION)) %>%
                        slice(1)
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      #Render the city overview map
      output$city_weather_bike_df =renderLeaflet ({
        leaflet() %>% addTiles() %>% addMarkers(label=city_weather_bike_df$CITY_ASCII,lng=city_bike_map$LNG, lat=city_bike_map$LAT, 
                                                popup=city_weather_bike_df$LABEL,options = popupOptions(closeButton = FALSE))})
    }
    else {
      #Render the specific city map
      selected_city=reactive({ city %>% filter(CITY_ASCII==input$city_dropdown) }) # reactive give a function output not df. So need to
      # add () at the end.
      
      selected_city_5_day=reactive({city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)})
      
      output$city_weather_bike_df =renderLeaflet ({
        leaflet() %>% addTiles() %>% setView(lng=selected_city()$LNG, lat=selected_city()$LAT, zoom=15) %>% 
          addMarkers(lng=selected_city()$LNG, lat=selected_city()$LAT, 
                     popup=selected_city()$DETAILED_LABEL)
      })
    } 
  })
  # Then render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    # Complete this function to render a leaflet map
    leaflet() %>% addTiles () %>% 
    addMarkers (lng = cities_max_bike_df$LNG, lat = cities_max_bike_df$LAT,
                      popup = cities_max_bike_df$LABEL )
    
  })
  # If All was selected from dropdown, then render a leaflet map with circle markers
    # and popup weather LABEL for all five cities
  selected_city=reactive({ city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown) })
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})
