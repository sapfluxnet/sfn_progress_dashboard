# libraries
library(shiny)

# global script
source('global.R')

shinyServer(function(input, output) {
  
  # data
  load_and_show()
  
  # sites value box
  output$site_number <- renderValueBox({
    
    site_number_value <- length(unique(site_md[['si_code']]))
    
    valueBox(
      value = site_number_value,
      subtitle = 'Sites',
      icon = icon('dot-circle-o', lib = 'font-awesome'),
      color = 'aqua',
      width = 4
    )
  })
  
  # countries value box
  output$countries <- renderValueBox({
    
    countries_value <- length(unique(site_md[['si_country']]))
    
    valueBox(
      value = countries_value,
      subtitle = 'Countries',
      icon = icon('globe', lib = 'font-awesome'),
      color = 'light-blue',
      width = 4
    )
  })
  
  # contributors value box
  output$contributors <- renderValueBox({
    
    contributors_value <- length(
      unique(
        c(
          unique(
            paste(site_md[['si_contact_firstname']],
                  site_md[['si_contact_lastname']], sep = ' ')
          ),
          unique(
            paste(site_md[['si_addcontr_firstname']],
                  site_md[['si_addcontr_lastname']], sep = ' ')
          ))
      )
    )
    
    valueBox(
      value = contributors_value,
      subtitle = 'Contributors',
      icon = icon('users', lib = 'font-awesome'),
      color = 'blue',
      width = 4
    )
  })
  
  # site_map
  output$map <- site_map()
  
  # popup observe
  observe({
    leafletProxy('map') %>% clearPopups()
    
    event <- input$map_marker_click
    
    if (is.null(event)) {
      return()
    }
    
    isolate({
      site_popup(event$id, event$lat, event$lng)
    })
  })
  
  # country table
  output$countryTable <- DT::renderDataTable({
    site_md %>%
      select(si_country, si_code) %>%
      arrange(si_country, si_code) %>%
      DT::datatable(
        colnames = c('Country code', 'Site code'),
        extensions = 'Scroller',
        options = list(
          dom = 'ti',
          scrollY = 400,
          scroller = TRUE
        ),
        selection = list(target = 'row')
      )
  })
  
  # rows selected observer to fill markers in leaflet
  observe({
    rows_selected <- input$countryTable_rows_selected
    
    if (is.null(rows_selected)) {
      leafletProxy('map', data = site_md) %>% clearMarkers() %>%
        addCircleMarkers(lng = ~si_long, lat = ~si_lat, layerId = ~si_code,
                         radius = 6,
                         fillOpacity = 0.7,
                         fillColor = '#FDE725',
                         stroke = FALSE)
      return()
    }
    
    # palette
    # color_data <- rep('unsel', length(site_md[['si_code']]))
    # color_data[rows_selected] <- 'sel'
    # 
    # pal <- colorFactor(c('#D91E18FF', '#FFFFFFFF'),
    #                    color_data,
    #                    alpha = TRUE)
    
    selected_map_data <- site_md %>%
      slice(rows_selected)
    
    leafletProxy('map', data = selected_map_data) %>% clearMarkers() %>%
      addCircleMarkers(lng = ~si_long, lat = ~si_lat, layerId = ~si_code,
                       radius = 6,
                       fillOpacity = 0.8,
                       fillColor = '#CF000F',
                       stroke = FALSE)
    
  })

})
