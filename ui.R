# libraries
library(shiny)
library(shinydashboard)

dashboardPage(
  
  # dashboard header
  dashboardHeader(
    title = "SAPFLUXNET Progress Report",
    titleWidth = 350
  ),
  
  # disAble sidebar
  dashboardSidebar(disable = TRUE),
  
  # dashboard body
  dashboardBody(
    
    fluidRow(
      # general tabox
      tabBox(
        id = 'main', title = '', width = 12, height = '100%',
        
        # site tab
        tabPanel(
          title = 'Sites',
          #valueboxes row
          fluidRow(
            valueBoxOutput("site_number"),
            valueBoxOutput("countries"),
            valueBoxOutput("contributors")
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
        
        # Species tab
        tabPanel(
          title = 'Methods',
          
          # valueboxes row
          fluidRow(
            valueBoxOutput("site_number_2"),
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
        )
      )
    )
  )
)
