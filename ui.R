# libraries
library(shiny)
library(shinydashboard)

dashboardPage(
  
  # dashboard header
  custom_header,
  
  # disAble sidebar
  dashboardSidebar(disable = TRUE),
  
  # dashboard body
  dashboardBody(
    
    # custom css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    fluidRow(
      # general tabox
      tabBox(
        id = 'main', title = '', width = 12, height = NULL,
        
        # intro tab
        tabPanel(
          title = 'Introduction',
          # intro card
          fluidRow(
            column(width = 3),
            column(width = 6,
                   box(
                     title = "Welcome to the progress updates app for SAPFLUXNET!!",
                     width = 12, background = "light-blue",
                     p("We at SAPFLUXNET Project are working hard in the process",
                       " of quality assesing of received data, putting all",
                       " together and building the best global sap flow database for",
                       " your pleasure."),
                     p("In the meantime, take a glance on some metadata that we have already")
                   )),
            column(width = 3)
          ),
          
          # second row, cards with info about the tabs
          fluidRow(
            box(
              title = tags$a("Sites", href = '#tab-7921-2'),
              width = 4, background = "navy",
              p("Here you can see the sites in a map. Feel free to zoom in and",
                " zoom out. Cliking in the points gives you info about the sites",
                " and clicking in the table isolates the selected site in the
                  map for easy inspection")
              
            )
          )
        ),
        
        # site tab
        tabPanel(
          title = 'Sites',
          #valueboxes row
          fluidRow(
            sitesvbOutput("sites_1"),
            countriesvbOutput("countries_1"),
            contributorsvbOutput('contributors_1')
          ),
          # map and table row
          fluidRow(
            # map column
            column(
              width = 8,
              leafletOutput('map', height = '500px', width = 'auto')
            ),
            # table column
            column(
              width = 4,
              DT::dataTableOutput('countryTable', height = '500px', width  = 'auto')
            )
          )
        ),
        
        # Biomes tab
        tabPanel(
          title = 'Biomes',
          
          # valueboxes row
          fluidRow(
            sitesvbOutput("sites_2"),
            countriesvbOutput("countries_2"),
            valueBoxOutput("biomes")
          ),
          
          # biomes plot
          fluidRow(
            # biomes plot column
            column(
              width = 8,
              ggiraphOutput('biomesPlot', height = '450px')
            ),
            
            # biomes table column
            column(
              width = 4,
              DT::dataTableOutput('biomesTable', height = 'auto', width  = 'auto'),
              DT::dataTableOutput('biomesSitesTable', height = 'auto', width  = 'auto')
            )
          )
        ),
        
        # Methods tab
        tabPanel(
          title = 'Methods',
          
          # valueboxes row
          fluidRow(
            sitesvbOutput("sites_3"),
            valueBoxOutput("plants"),
            valueBoxOutput("methods")
          ),
          
          # methods plot
          fluidRow(
            column(
              width = 8,
              ggiraphOutput("methodsPlot", height = '450px')
            ),
            
            column(
              width = 4,
              DT::dataTableOutput('methodsTable', height = 'auto', width  = 'auto'),
              br(),
              p('CHP: Compensation Heat Pulse'),
              p('HD: Constant Heat Dissipation'),
              p('HPTM: Heat Pulse Tmax Method'),
              p('HR: Heat Ratio')
            )
          )
        ),
        
        # Species tab
        tabPanel(
          title = "Species",
          
          # valueboxes row
          fluidRow(
            sitesvbOutput("sites_4"),
            valueBoxOutput("species"),
            valueBoxOutput("genus")
          ),
          
          # plots row
          fluidRow(
            column(
              width = 6,
              ggiraphOutput('speciesPlot', width = '95%', height = '450px')
            ),
            column(
              width = 6,
              ggiraphOutput('genusPlot', width = '95%', height = '450px')
            )
          )
        ),
        
        # Contributors tab
        tabPanel(
          title = 'Contributors',
          
          # valueboxes row
          fluidRow(
            sitesvbOutput("sites_5"),
            valueBoxOutput('institutions'),
            contributorsvbOutput('contributors_2')
          ),
          
          # contributors table
          fluidRow(
            column(
              width = 1
            ),
            column(
              width = 10,
              DT::dataTableOutput('contributorsTable', height = 'auto', width  = 'auto')
            )
          )
        )
      )
    )
  )
)
