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
  
  # biomes plot
  # reactive event to capture selected rows
  biomes_plot_reactive <- eventReactive(
    ignoreNULL = FALSE,
    eventExpr = input$biomesTable_rows_selected,
    valueExpr = {
      rows_selected <- input$biomesTable_rows_selected
      
      if (is.null(rows_selected)) {
        rows_selected <- 1:length(unique(site_md[['si_biome']]))
      }
      
      biomes_plot <- site_md %>%
        arrange(si_biome) %>%
        filter(si_biome %in% unique(.[['si_biome']])[rows_selected]) %>%
        vis_location_biome() +
        theme_sfn()
      
      ggiraph(code = {print(biomes_plot)},
              width = 0.95, width_svg = 8.25, height_svg = 4.13, zoom_max = 5,
              tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
              hover_css = "fill-opacity:.4",
              selected_css = "stroke:black;r:4pt;")
    }
  )
  
  output$biomesPlot <- renderggiraph({
    biomes_plot_reactive()
  })
  
  # biomes table
  output$biomesTable <- DT::renderDataTable({
    site_md %>%
      group_by(si_biome) %>%
      summarise(n = n()) %>%
      DT::datatable(
        colnames = c('Biome', 'Number of sites'),
        extensions = 'Scroller',
        options = list(
          dom = 'ti',
          scrollY = 400,
          scroller = TRUE,
          scrollCollapse = TRUE
        ),
        selection = list(target = 'row')
      )
  })
  
  # biomes SITE  table
  output$biomesSitesTable <- DT::renderDataTable({
    
    rows_selected <- input$biomesTable_rows_selected
    
    if (is.null(rows_selected)) {
      rows_selected <- 1:length(unique(site_md[['si_biome']]))
    }
    
    site_md %>%
      arrange(si_biome) %>%
      filter(si_biome %in% unique(.[['si_biome']])[rows_selected]) %>%
      select(si_code, si_biome) %>%
      DT::datatable(
        colnames = c('Site code', 'Biome'),
        extensions = 'Scroller',
        options = list(
          dom = 'ti',
          scrollY = 150,
          # scroller = TRUE,
          scrollCollapse = TRUE,
          paging = FALSE
        ),
        selection = list(target = 'row')
      )
  })
  
  output$site_number_2 <- renderValueBox({
    
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
  output$plants <- renderValueBox({
    
    plants_value <- length(plant_md[['pl_sens_meth']])
    
    valueBox(
      value = plants_value,
      subtitle = 'Plants',
      icon = icon('tree', lib = 'font-awesome'),
      color = 'light-blue',
      width = 4
    )
  })
  
  # countries value box
  output$methods <- renderValueBox({
    
    methods_value <- length(unique(plant_md[['pl_sens_meth']]))
    
    valueBox(
      value = methods_value,
      subtitle = 'Sap flow methods',
      icon = icon('wrench', lib = 'font-awesome'),
      color = 'blue',
      width = 4
    )
  })
  
  # methods plot
  output$methodsPlot <- renderggiraph({
    
    total <- length(plant_md[['pl_sens_meth']])
    
    methods_plot <- plant_md %>%
      group_by(pl_sens_meth, si_code) %>%
      summarise(perc = (n()/total)*100) %>%
      # mutate(perc = (n/sum(n))*100) %>%
      ggplot(aes(x = pl_sens_meth, y = perc, fill = pl_sens_meth,
                 tooltip = si_code)) +
      geom_bar_interactive(stat = 'identity') +
      scale_fill_viridis(discrete = TRUE) +
      labs(x = 'Method', y = '%') +
      theme_sfn() +
      theme(
        legend.position = 'none'
      )
    
    ggiraph(code = {print(methods_plot)},
            width = 0.95, width_svg = 8.25, height_svg = 4.13, zoom_max = 5,
            tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
            hover_css = "fill-opacity:.4",
            selected_css = "stroke:black;r:4pt;")
  })
  
  # methods table
  output$methodsTable <- DT::renderDataTable({
    
    meth_x_site <- plant_md %>%
      group_by(pl_sens_meth) %>%
      summarise(Plants = n())
    
    plant_md %>%
      group_by(pl_sens_meth, si_code) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      group_by(pl_sens_meth) %>%
      summarise(Sites = n()) %>%
      select(Sites) %>%
      bind_cols(meth_x_site, .) %>%
      ungroup() %>%
      mutate(plant_perc = paste0('(', round((Plants/sum(Plants))*100,1), ' %', ')'),
             site_perc = paste0('(', round((Sites/sum(Sites))*100, 1), ' %', ')'),
             Relation = round(Plants/Sites, 1)) %>%
      tidyr::unite(plant, Plants, plant_perc, sep = ' ') %>%
      tidyr::unite(site, Sites, site_perc, sep = ' ') %>%
      DT::datatable(
        colnames = c('Method', 'Plants', 'Sites', 'Prop.'),
        extensions = 'Scroller',
        options = list(
          dom = 'ti',
          scrollY = 400,
          # scroller = TRUE,
          scrollCollapse = TRUE,
          paging = FALSE
        ),
        selection = list(target = 'row')
      )
  })
  
})
