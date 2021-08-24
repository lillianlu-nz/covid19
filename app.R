library(shiny)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(chron)
library(lubridate)

# LOAD DATA

# locations <- read_csv("locations-of-interest.csv")
locations = read_csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/locations-of-interest/august-2021/locations-of-interest.csv")

locations$date = as.Date(substring(locations$Start, 1, 10), "%d/%m/%Y")
locations$starttime = format(as.POSIXct(substring(locations$Start, 13, 22),format='%I:%M %p'),format="%H:%M:%S")
locations$endtime = format(as.POSIXct(substring(locations$End, 13, 22),format='%I:%M %p'),format="%H:%M:%S")
locations$start_datetime = ymd_hms(paste0(locations$date, locations$starttime))
locations$end_datetime = ymd_hms(paste0(locations$date, locations$endtime))
locations$info = paste("Event started at ", locations$Start, " and ended at ", locations$End, ". If you were in this location during this time period: ", locations$Advice)
citylist = unique(locations$City)

# deal with "Added" data - couldn't convert to date directly due to different entry format
added1 = locations[,c(1,10)]
added1$updatetime = as.Date(substring(added1$Added, 1, 10), "%d/%m/%Y")

added2 = locations[,c(1,10)]
added2$updatetime = as.Date(substring(added1$Added, 1, 10), "%Y-%m-%d")

joinAdded = left_join(added1, added2, "id")
joinAdded$updated = paste(joinAdded$`updatetime.x`,joinAdded$`updatetime.y`)
joinAdded$updated = gsub('NA', '', joinAdded$updated)
joinAdded$updated = as.Date(joinAdded$updated, "%Y-%m-%d")

# Add cleaned "addeded" data to the data file
locations$update_time = joinAdded$updated
updatelist = unique(locations$update_time)

# action button
text_about <- "This app was recreated using the Ministry of Health data from GitHub: https://github.com/minhealthnz. Use the 'Date Updated On/After' filter to 
see locations newly added to the map. Note: this app was not designed for mobile; try flipping your phone for a better view. Feel free to provide any feedback or contact me: lillianlu.nz@gmail.com"

# BEGIN UI

ui <- bootstrapPage(
    
    title = "New Zealand COVID-19 Locations of Interest Map",
    #set theme
    theme = shinythemes::shinytheme('simplex'),
    
    leaflet::leafletOutput('map', width = '100%', height = '100%'),
    
    # panel on top of output
    absolutePanel(top = 10, right=10, draggable =T, id = 'controls',
                  
                  selectInput("city", 
                              "Select City",
                              choices =  c(citylist, "All"),
                              selected = "All", multiple = T),
                  
                  sliderInput('time', 'Select Hour Range', 0, 24, c(0, 24)),
                  
                  dateRangeInput(
                      'date_range', 'Select Date Range', "2021-08-10", "2021-08-20"
                  ),
                  selectInput("updatetime",
                              "Data Updated On/After",
                              choices =  updatelist, selected = NA),
                  actionButton('show_about', 'About this app')
    ),
    
    tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;opacity:0.8;padding:20px;}
  ")
)

# BEGIN SERVER

server <- function(input, output, session) {

    observeEvent(input$show_about, {
        showModal(modalDialog(text_about, title = 'About'))
    })
  
  location_filter <- reactive({
    if (input$city == 'All') {
      locations
    }
    else {
      locations %>%
        filter(City %in% input$city)
    }
  })
  
  update_filter <- reactive({
    if (input$updatetime == "NA") {
      location_filter()
    }
    else {
      location_filter() %>%
        filter(update_time >= input$updatetime)
    }
  })
   
    output$map <- leaflet::renderLeaflet({
      update_filter() %>% 
            filter(
            hour(start_datetime) >= input$time[1] &
            hour(start_datetime) <= input$time[2] &
            hour(end_datetime) >= input$time[1] &
            hour(end_datetime) <= input$time[2] &
            date >= input$date_range[1] &
            date <= input$date_range[2]) %>%
            leaflet() %>% 
            setView( 174, -40, zoom = 6)  %>% addProviderTiles(providers$CartoDB.Positron) %>% 
            addMarkers(~LNG, ~LAT, popup = ~info, label = ~Event)
    })
    
    session$onSessionEnded(function() {
      stopApp()
    })
}

shinyApp(ui, server)

# Reference link:
# https://learn.datacamp.com/courses/building-web-applications-with-shiny-in-r
# https://rstudio.github.io/leaflet/markers.html
