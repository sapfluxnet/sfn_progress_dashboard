# libraries
library(shiny)
library(shinydashboard)

dashboardPage(
  
  # dashboard header
  dashboardHeader(
    title = loadingLogo(
      href = 'http://sapfluxnet.creaf.cat',
      src = 'sfn_logo.png',
      loadingsrc = 'pie_loader.svg',
      height = '32px', width = '32px'
    ),
    titleWidth = 400
  ),
  
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
