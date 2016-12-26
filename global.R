# load data function
# data is loaded from level_1_data.RData file, which is populated every night
# with a cron/automated process from all data stored in the level 1.
# In order to load the data showing a beautiful progress indicator we create a
# function to show and hide the different divs using shinyjs package

# libraries
library(shiny)
library(shinyjs)
library(sapfluxnetQC1)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(viridis)

# function data load
load_and_show <- function() {
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading and crunching data", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress$close())
  
  progress$inc(1/3, detail = "Metadata")
  load('level_1_metadata.RData', envir = .GlobalEnv)
  
  progress$inc(1/3, detail = "Data")
  # load('level_1_data.RData', envir = .GlobalEnv)
  
  progress$inc(1/3, detail = "Completed!")
}

# functions map

## popup function
site_popup <- function(site, lat, lng) {
  selected_site <- site_md[site_md[['si_code']] == site, ]
  popup_text <- as.character(tagList(
    tags$h4(selected_site[['si_code']]),
    tags$strong(selected_site[['si_country']]), tags$br(),
    # sprintf('Paper: %s', tags$a(href = selected_site$si_paper)), tags$br(),
    tags$a(selected_site[['si_paper']], href = selected_site[['si_paper']]), tags$br(),
    sprintf('IGBP: %s', selected_site[['si_ibgp']]), tags$br(),
    sprintf('Elevation above sea level: %s', selected_site[['si_elev']]), tags$br(),
    sprintf('In Dendroglobal? %s', selected_site[['si_dendro_network']]), tags$br(),
    sprintf('In Fluxnet? %s', selected_site[['si_flux_network']]), tags$br()
  ))
  leafletProxy('map') %>%
    addPopups(lng, lat, popup_text, layerId = site)
}

## render map function
site_map <- function() {
  renderLeaflet({
    
    # leaflet
    leaflet(data = site_md) %>%
      addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
               attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
               options = tileOptions(noWrap = FALSE)) %>%
      setView(lng = 15, lat = 35, zoom = 2) #%>%
      # clearMarkers() %>%
      # addCircleMarkers(lng = ~si_long, lat = ~si_lat, layerId = ~si_code,
      #                  radius = 10,
      #                  fillOpacity = 0.7,
      #                  fillColor = "#FDE725",
      #                  stroke = FALSE)
  })
}
