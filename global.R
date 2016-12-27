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
library(stringr)

# modules
source('modules.R')

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


## ggplot themes
bar_minimal_theme <- function(base_size = 10, base_family = "Lato") {
  half_line <- base_size/2
  theme(line = element_line(colour = "#5C97BF", size = 1,
                            linetype = 1, lineend = "butt"),
        rect = element_rect(fill = NA, colour = "#5C97BF",
                            size = 1, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "#5C97BF", size = base_size,
                            lineheight = 0.9, hjust = 0.5,
                            vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE),
        axis.line = element_blank(),
        # axis.line.x = element_line(),
        # axis.line.y = element_line(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line*2.5),
                                   vjust = 1),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line*2),
                                   hjust = 1),
        axis.ticks.y = element_line(colour = "#5C97BF", size = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(-half_line, "pt"),
        axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                    b = 0.8 * half_line/2)),
        axis.title.y = element_text(angle = 90,
                                    margin = margin(r = 0.8 * half_line,
                                                    l = 0.8 * half_line/2)),
        legend.background = element_rect(colour  = NA, fill = ),
        legend.spacing = unit(1, "pt"),
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(1, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0.5),
        legend.title.align = 0,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "top",
        legend.box = NULL,
        panel.background = element_blank(),
        panel.border = element_blank(),
        # panel.grid = element_blank(),
        # panel.grid.major = element_line(colour = "black", size = rel(0.3),
        #                                 linetype = 2),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#5C97BF", size = rel(1),
                                          linetype = 1),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        strip.background = element_rect(size = rel(0.3)),
        strip.text = element_text(colour = "grey10", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line,
                                                    b = half_line)),
        strip.text.y = element_text(angle = -90,
                                    margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_blank(),
        plot.title = element_text(size = rel(1.2),
                                  margin = margin(b = half_line * 1.2)),
        plot.margin = margin(half_line, half_line, half_line, half_line),
        
        complete = TRUE)
}

# header wiht custom link and logo
# took from here :
# http://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
# Takes a location 'href', an image location 'src', a loading gif 'loadingsrc'
# height, width and alt text, and produces a loading logo that activates while
# Shiny is busy
loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(href = href,
           div(class = "busy",  
               img(src = loadingsrc,height = height, width = width, alt = alt),
               span('loading')),
           div(class = 'notbusy',
               img(src = src, height = height, width = width, alt = alt),
               span('SAPFLUXNET Progress Report'))
    )
  )
}


# custom header
custom_header <- dashboardHeader(
  title = loadingLogo(
    href = 'http://sapfluxnet.creaf.cat',
    src = 'sfn_logo.png',
    loadingsrc = 'pie_loader.svg',
    height = '32px', width = '32px',
    alt = 'Go to project web'
  ),
  titleWidth = 400
)

custom_header$children[[3]]$children[[3]]$children <- tags$ul(
  class = 'nav navbar-nav',
  tags$li(
    class = 'dropdown messages-menu',
    tags$a(
      class = 'contributor-link',
      icon('info-circle', class = 'fa-lg'),
      href = 'https://github.com/sapfluxnet/sapfluxnet-public/wiki',
      span("Contribute to SAPFLUXNET")
    )
  )
)
