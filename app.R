library(shiny)
library(readr)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(chron)
library(lubridate)
library(leafpop)

# LOAD DATA ----

locations = read_csv("https://raw.githubusercontent.com/minhealthnz/nz-covid-data/main/locations-of-interest/august-2021/locations-of-interest.csv")

# date time formatting
locations$date = as.Date(substring(locations$Start, 1, 10), "%d/%m/%Y")
locations$uploadDate = as.Date(substring(locations$Added, 1, 10))
locations$startTime = format(as.POSIXct(substring(locations$Start, 13, 22),format='%I:%M %p'),format="%H:%M:%S")
locations$endTime = format(as.POSIXct(substring(locations$End, 13, 22),format='%I:%M %p'),format="%H:%M:%S")
locations$startDatetime = ymd_hms(paste0(locations$date, locations$startTime))
locations$endDatetime = ymd_hms(paste0(locations$date, locations$endTime))
locations$Duplicated = duplicated(locations$Event)

# var for filter options
citylist = unique(locations$City)
updatelist = unique(locations$uploadDate)

# action button
text_about <- "This app was recreated using the Ministry of Health data from GitHub: 
https://github.com/minhealthnz. Use the 'Date Updated On/After' filter to 
see locations newly added to the map. Locations duplicated or close to each other indicating 
a higher risk of virus transmission. Feel free to provide any feedback or contact me: lillianlu.nz@gmail.com"


# BEGIN SERVER ----

server <- function(input, output, session) {

# published code ----
observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = 'About'))
})

# location_filter <- reactive({
#   if (input$city == 'All') {
#     locations
#     # locations()
#   }
#   else {
#     locations %>%
#       filter(City %in% input$city)
#     # locations() %>%
#     #   filter(City %in% input$city)
#   }
# })
  
  update_filter <- reactive({
    if (input$updatetime == "NA") {
      locations
    }
    else {
      locations %>%
        filter(uploadDate >= input$updatetime)
    }
  })
   
    output$map <- leaflet::renderLeaflet({

      # getColor <- function(locations) {
      #   sapply(locations$Duplicated, function(Duplicated) {
      #     if(Duplicated == T) {
      #       "orange"
      #     } 
      #     else {
      #       "blue"
      #     } })
      # }
      # 
      # icons <- awesomeIcons(
      #   icon = 'user',
      #   iconColor = 'White',
      #   library = 'fa',
      #   markerColor = getColor(update_filter()))
      
      icons <- awesomeIcons(
        icon = 'user',
        iconColor = 'White',
        library = 'fa',
        markerColor = "blue")
      
      # map viz configuration
      update_filter() %>% 
            filter(
            hour(startDatetime) >= input$time[1] &
            hour(startDatetime) <= input$time[2] &
            hour(endDatetime) >= input$time[1] &
            hour(endDatetime) <= input$time[2] &
            date >= input$date_range[1] &
            date <= input$date_range[2]) %>%
            leaflet() %>% 
            setView( 176, -41, zoom = 6)  %>% 
            addProviderTiles("CartoDB.VoyagerLabelsUnder") %>% 
            addAwesomeMarkers(~LNG, ~LAT,
                              icon = icons,
                       popup = leafpop::popupTable(update_filter()[,2:7],
                                                   row.numbers = F),
                       label = ~paste0(Event, " - click to view details"),
                       clusterOptions = markerClusterOptions()
                       )
    })
    
    session$onSessionEnded(function() {
      stopApp()
    })
}


# BEGIN UI ----

ui <- bootstrapPage(
  
  title = "New Zealand COVID-19 Locations of Interest Map",
  #set theme
  theme = shinythemes::shinytheme('simplex'),
  
  leaflet::leafletOutput('map', width = '100%', height = '100%'),
  
  # panel on top of output
  
  absolutePanel(
    top = 10, right = 10, style = "z-index:500;",
    actionButton('show_about', 'About this tool', height = '50%')
  ),
  
  absolutePanel(
    bottom = 10, right=10, width = 200, height = 250, style = "z-index:500;",
    sliderInput('time', 'Select Hour Range', 0, 24, c(0, 24)),
    
    dateRangeInput(
      'date_range', 'Select Date Range', max(updatelist)-15, max(updatelist)
    ),
    selectInput("updatetime",
                "Data Updated On/After",
                choices =  updatelist, selected = max(updatelist)-5)
  ),
  
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
  ")
)

shinyApp(ui, server)
