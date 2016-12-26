# libraries
library(shiny)
library(shinydashboard)

dashboardPage(
  
  # dashboard header
  dashboardHeader(
    title = "SAPFLUXNET Progress Report"
  ),
  
  # disAble sidebar
  dashboardSidebar(disable = TRUE),
  
  # dashboard body
  dashboardBody(
    
    fluidRow(
      tabBox(
        id = 'main', title = '', width = 12, height = '100%',
        
        # map tab
        tabPanel(
          title = 'Site stats',
          fluidRow(
            valueBoxOutput("site_number"),
            valueBoxOutput("countries"),
            valueBoxOutput("contributors")
          ),
          fluidRow(
            column(
              width = 8,
              leafletOutput('map', height = '500px', width = 'auto')
            ),
            
            column(
              width = 4,
              DT::dataTableOutput('countryTable', height = '500px', width  = 'auto')
            )
          )
        )
      )
    )
  )
)
