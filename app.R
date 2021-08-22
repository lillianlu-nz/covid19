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

locations <- read_csv("locations-of-interest.csv")
locations$date = as.Date(substring(locations$Start, 1, 10), "%d/%m/%Y")
locations$starttime = format(as.POSIXct(substring(locations$Start, 13, 22),format='%I:%M %p'),format="%H:%M:%S")
locations$endtime = format(as.POSIXct(substring(locations$End, 13, 22),format='%I:%M %p'),format="%H:%M:%S")
locations$start_datetime = ymd_hms(paste0(locations$date, locations$starttime))
locations$end_datetime = ymd_hms(paste0(locations$date, locations$endtime))
locations$info = paste("Event started at ", locations$Start, " and ended at ", locations$End, ". If you were in this location during this time period: ", locations$Advice)
citylist = unique(locations$City)

text_about <- "This app was recreated using the Ministry of Health data from GitHub: https://github.com/minhealthnz, dated from 10 to 20 August 2021. 
It was not designed for mobile; try flip your phone to get a better view. Feel free to provide any feedback or contact me: lillianlu.nz@gmail.com"

ui <- bootstrapPage(
    
    title = "COVID-19 Locations of Interest (10-20 August 2021)",
    #set theme
    theme = shinythemes::shinytheme('simplex'),
    
    # only one output
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
                  actionButton('show_about', 'About this app')
    ),
    
    tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;opacity:0.8;padding:20px;}
  ")
)
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
   
    output$map <- leaflet::renderLeaflet({
        locations %>% 
            filter(
            hour(start_datetime) >= input$time[1] &
            hour(start_datetime) <= input$time[2] &
            hour(end_datetime) >= input$time[1] &
            hour(end_datetime) <= input$time[2] &
            date >= input$date_range[1] &
            date <= input$date_range[2]) %>%
            leaflet() %>% 
            setView( 174, -40, zoom = 6)  %>% addTiles() %>% 
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