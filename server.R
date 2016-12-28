# libraries
library(shiny)

shinyServer(function(input, output) {
  
  #### data ####
  load_and_show()
  
  
  #### modules (for repeated value boxes) ####
  callModule(sitesvb, 'sites_1', data = site_md)
  callModule(sitesvb, 'sites_2', data = site_md)
  # callModule(sitesvb, 'sites_3', data = site_md)
  # callModule(sitesvb, 'sites_4', data = site_md)
  # callModule(sitesvb, 'sites_5', data = site_md)
  callModule(countriesvb, 'countries_1', data = site_md)
  # callModule(countriesvb, 'countries_2', data = site_md)
  # callModule(contributorsvb, 'contributors_1', data = site_md)
  callModule(contributorsvb, 'contributors_2', data = site_md)
  
  #### site_map ####
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
  
  #### country table ####
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
          scrollX = '100%',
          scroller = TRUE
        ),
        selection = list(target = 'row')
      ) %>%
      formatStyle(
        0,
        target = 'row',
        backgroundColor = '#E4F1FE'
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
  
  #### biomes valuebox ####
  output$biomes <- renderValueBox({
    biome_number_value <- length(unique(site_md[['si_biome']]))
    valueBox(
      value = biome_number_value,
      subtitle = 'Biomes',
      icon = icon('cloud', lib = 'font-awesome'),
      color = 'light-blue',
      width = 4
    )
  })
  
  #### biomes plot ####
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
        bar_minimal_theme() +
        theme(
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.line.x = element_line(colour = "#5C97BF", size = rel(1),
                                     linetype = 1),
          axis.line.y = element_line(colour = "#5C97BF", size = rel(1),
                                     linetype = 1)
        )
      
      ggiraph(code = {print(biomes_plot)},
              width = 0.95, width_svg = 8.25, height_svg = 4.13,
              tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
              hover_css = "fill-opacity:.4",
              selected_css = "stroke:black;r:4pt;")
    }
  )
  
  output$biomesPlot <- renderggiraph({
    biomes_plot_reactive()
  })
  
  #### biomes table ####
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
          scrollX = '100%',
          scroller = TRUE,
          scrollCollapse = TRUE
        ),
        selection = list(target = 'row')
      ) %>%
      formatStyle(
        0,
        target = 'row',
        backgroundColor = '#E4F1FE'
      )
  })
  
  #### biomes SITE  table ####
  output$biomesSitesTable <- DT::renderDataTable({
    
    rows_selected <- input$biomesTable_rows_selected
    
    if (is.null(rows_selected)) {
      return()
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
          scrollX = '100%',
          # scroller = TRUE,
          scrollCollapse = TRUE,
          paging = FALSE
        ),
        selection = list(target = 'row')
      ) %>%
      formatStyle(
        0,
        target = 'row',
        backgroundColor = '#E4F1FE'
      )
  })
  
  #### plants value box ####
  output$plants <- renderValueBox({
    
    plants_value <- length(plant_md[['pl_sens_meth']])
    
    valueBox(
      value = plants_value,
      subtitle = 'Plants',
      icon = icon('tree', lib = 'font-awesome'),
      color = 'aqua',
      width = 4
    )
  })
  
  #### methods value box ####
  output$methods <- renderValueBox({
    
    methods_value <- length(unique(plant_md[['pl_sens_meth']]))
    
    valueBox(
      value = methods_value,
      subtitle = 'Sap flow methods',
      icon = icon('wrench', lib = 'font-awesome'),
      color = 'light-blue',
      width = 4
    )
  })
  
  #### methods plot ####
  # reactive event to capture selected rows
  methods_plot_reactive <- eventReactive(
    ignoreNULL = FALSE,
    eventExpr = input$methodsTable_rows_selected,
    valueExpr = {
      
      rows_selected <- input$methodsTable_rows_selected
      
      if (is.null(rows_selected)) {
        total <- length(plant_md[['pl_sens_meth']])
        
        methods_plot <- plant_md %>%
          group_by(pl_sens_meth, si_code) %>%
          summarise(perc = (n()/total)*100) %>%
          ggplot(aes(x = pl_sens_meth, y = perc, fill = pl_sens_meth,
                     tooltip = si_code)) +
          geom_bar_interactive(stat = 'identity') +
          scale_fill_viridis(discrete = TRUE) +
          labs(x = 'Method', y = '% of total plants') +
          bar_minimal_theme() +
          theme(
            legend.position = 'none'
          )
        
        ggiraph(code = {print(methods_plot)},
                width = 0.95, width_svg = 8.25, height_svg = 4.13,
                tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
                hover_css = "fill-opacity:.4",
                selected_css = "stroke:black;r:4pt;")
      } else {
        methods_plot <- plant_md %>%
          arrange(pl_sens_meth) %>%
          filter(pl_sens_meth %in% unique(.[['pl_sens_meth']])[rows_selected]) %>%
          ggplot(aes(x = si_code, fill = pl_sens_meth,
                     tooltip = pl_sens_meth)) +
          geom_bar_interactive(stat = 'count') +
          scale_fill_viridis(discrete = TRUE) +
          labs(x = 'Site', y = 'Plants') +
          bar_minimal_theme() +
          theme(
            legend.position = 'none',
            axis.text.x = element_text(angle = 90)
          )
        
        ggiraph(code = {print(methods_plot)},
                width = 0.95, width_svg = 8.25, height_svg = 4.13,
                tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
                hover_css = "fill-opacity:.4",
                selected_css = "stroke:black;r:4pt;")
      }
    }
  )
  
  output$methodsPlot <- renderggiraph({
    methods_plot_reactive()
  })
  
  
  # output$methodsPlot <- renderggiraph({
  #   
  #   total <- length(plant_md[['pl_sens_meth']])
  #   
  #   methods_plot <- plant_md %>%
  #     group_by(pl_sens_meth, si_code) %>%
  #     summarise(perc = (n()/total)*100) %>%
  #     ggplot(aes(x = pl_sens_meth, y = perc, fill = pl_sens_meth,
  #                tooltip = si_code)) +
  #     geom_bar_interactive(stat = 'identity') +
  #     scale_fill_viridis(discrete = TRUE) +
  #     labs(x = 'Method', y = '% of total plants') +
  #     bar_minimal_theme() +
  #     theme(
  #       legend.position = 'none'
  #     )
  #   
  #   ggiraph(code = {print(methods_plot)},
  #           width = 0.95, width_svg = 8.25, height_svg = 4.13,
  #           tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
  #           hover_css = "fill-opacity:.4",
  #           selected_css = "stroke:black;r:4pt;")
  # })
  
  #### methods table ####
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
          scrollX = '100%',
          # scroller = TRUE,
          scrollCollapse = TRUE,
          paging = FALSE
        ),
        selection = list(target = 'row')
      ) %>%
      formatStyle(
        0,
        target = 'row',
        backgroundColor = '#E4F1FE'
      )
  })
  
  #### species valuebox ####
  output$species <- renderValueBox({
    specie_number_value <- length(unique(species_md[['sp_name']]))
    valueBox(
      value = specie_number_value,
      subtitle = 'Species',
      icon = icon('leaf', lib = 'font-awesome'),
      color = 'aqua',
      width = 4
    )
  })
  
  # genus valuebox
  output$genus <- renderValueBox({
    genus_number_value <- length(unique(
      str_trim(str_extract(species_md[['sp_name']], '([^\\s]+)'))
    ))
    valueBox(
      value = genus_number_value,
      subtitle = 'Genus',
      icon = icon('code-fork', lib = 'font-awesome'),
      color = 'light-blue',
      width = 4
    )
  })
  
  #### species plot ####
  output$speciesPlot <- renderggiraph({
    species_plot <- species_md %>%
      group_by(sp_name, si_code) %>%
      summarise(n = sum(sp_ntrees)) %>%
      ggplot(aes(x = reorder(sp_name, n, sum), y = n, fill = si_code)) +
      geom_bar_interactive(aes(tooltip = si_code, data_id = si_code),
                           stat = 'identity') +
      viridis::scale_fill_viridis(discrete = TRUE) +
      labs(x = '', y = 'Number of plants', title = 'Species') +
      coord_flip() +
      bar_minimal_theme() +
      theme(
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#5C97BF", size = rel(1),
                                          linetype = 1),
        axis.ticks.y = element_blank()
      )
    
    ggiraph(code = {print(species_plot)},
            width = 0.95, width_svg = 8.25, height_svg = 5,
            tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
            hover_css = "fill-opacity:.4")
  })
  
  #### genus plot ####
  output$genusPlot <- renderggiraph({
    genus_plot <- species_md %>%
      mutate(sp_genus = str_trim(str_extract(sp_name, '([^\\s]+)'))) %>%
      group_by(sp_genus, si_code) %>%
      summarise(n = sum(sp_ntrees)) %>%
      ggplot(aes(x = reorder(sp_genus, n, sum), y = n, fill = si_code)) +
      geom_bar_interactive(aes(tooltip = si_code, data_id = si_code),
                           stat = 'identity') +
      viridis::scale_fill_viridis(discrete = TRUE) +
      labs(x = '', y = 'Number of plants', title = 'Genera') +
      coord_flip() +
      bar_minimal_theme() +
      theme(
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#5C97BF", size = rel(1),
                                          linetype = 1),
        axis.ticks.y = element_blank()
      )
    
    ggiraph(code = {print(genus_plot)},
            width = 0.95, width_svg = 8.25, height_svg = 5,
            tooltip_extra_css = "background-color:#1E8BC3;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;color:white",
            hover_css = "fill-opacity:.4")
  })
  
  #### institutions valuebox ####
  output$institutions <- renderValueBox({
    institution_number_value <- length(
      unique(
        c(site_md[['si_contact_institution']],
          site_md[['si_addcontr_institution']])
      )
    )
    
    valueBox(
      value = institution_number_value,
      subtitle = 'Institutions',
      icon = icon('university', lib = 'font-awesome'),
      color = 'aqua',
      width = 4
    )
  })
  
  #### contributors table ####
  output$contributorsTable <- DT::renderDataTable({
    addcontr_df <- site_md %>%
      arrange(si_addcontr_lastname) %>%
      tidyr::unite(Contributor, si_addcontr_firstname, si_addcontr_lastname, sep = ' ') %>%
      mutate(Institution = si_addcontr_institution,
             Reference = si_paper) %>%
      select(Contributor, Institution, Reference)
    
    site_md %>%
      arrange(si_contact_lastname) %>%
      tidyr::unite(Contributor, si_contact_firstname, si_contact_lastname, sep = ' ') %>%
      mutate(Institution = si_contact_institution,
             Reference = si_paper) %>%
      select(Contributor, Institution, Reference) %>%
      bind_rows(addcontr_df) %>%
      distinct(Contributor, .keep_all = TRUE) %>%
      DT::datatable(
        extensions = 'Scroller',
        options = list(
          dom = 'ti',
          scrollY = 400,
          scrollX = '100%',
          # scroller = TRUE,
          scrollCollapse = TRUE,
          paging = FALSE,
          # fixedColumns = TRUE,
          autoWidth = FALSE,
          columnDefs = list(list(className = 'dt-left',
                                 targets = 1:3),
                            list(targets = 1:2,
                                 width = '20%'),
                            list(targets = 3,
                                 width = '60%'))
        ),
        selection = list(target = 'row')
      ) %>%
      formatStyle(
        0,
        target = 'row',
        backgroundColor = '#E4F1FE'
      )
      
  })
  
})
