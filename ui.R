# libraries
library(shiny)
library(shinydashboard)

dashboardPage(
  
  #### dashboard header ####
  custom_header,
  
  #### disable sidebar ####
  dashboardSidebar(disable = TRUE),
  
  #### dashboard body ####
  dashboardBody(
    
    # custom css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # general tabbox
    tabBox(
      id = 'main', title = '', width = 12, height = NULL,
      
      #### intro tab ####
      tabPanel(
        title = 'Start',
        # intro card
        fluidRow(
          column(width = 3),
          column(width = 6,
                 box(
                   title = "Welcome to the SAPFLUXNET progress report!",
                   width = 12, background = "blue",
                   p("We at SAPFLUXNET Project are working hard putting all the data",
                     " together and building the best global sap flow database for",
                     " your pleasure."),
                   p("In the meantime, take a glance on some metadata that we already have",
                     " by navigating the tabs.")
                 )),
          column(width = 3)
        ),
        
        fluidRow(
          box(
            title = img(src = 'ministry_logo.png', width = '85%'),
            width = 4, background = 'light-blue',
            "The Sapfluxnet project is led by researchers at ",
            a(href = "http://www.creaf.cat/", "CREAF"),
            " and funded by the Spanish Ministry of Economy and Competitiveness",
              " through grant nº CGL2014-55883-JIN"
          ),
          
          box(
            title = icon('spinner', class = 'lg'),
            width = 4, background = 'light-blue',
            h4("Constant updates"),
            "Any time a new dataset is received and processed the updated",
            " metadata will appear in this app.",
            "If you are worried about your dataset not appearing in the app,",
            " this only means that is still processing."
          ),
          
          box(
            title = icon('calendar-o', class = 'lg'),
            width = 4, background = 'light-blue',
            h4("Calling for data still open"),
            "There is still time to send your data if already don't,",
            " please go to the project ",
            a(href = 'https://github.com/sapfluxnet/sapfluxnet-public/wiki/Data-Contribution',
              "wiki"),
            " to obtain info about how to contribute to SAPFLUXNET."
          )
        )
      ),
      
      # site tab ####
      tabPanel(
        title = 'Sites',
        #valueboxes row
        fluidRow(
          sitesvbOutput("sites_1"),
          countriesvbOutput("countries_1"),
          box(
            # title = "Sites tab",
            width = 4, background = "blue",
            p("See the sites in a map. Feel free to zoom in (in some places you can see the plots) and",
              " zoom out. Clicking in the points gives you info about the sites",
              " and clicking in the table isolates the selected site in the
              map for easy inspection")
          )
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
      
      # Biomes tab ####
      tabPanel(
        title = 'Biomes',
        
        # valueboxes row
        fluidRow(
          sitesvbOutput("sites_2"),
          valueBoxOutput("biomes"),
          box(
            # title = "Biomes tab",
            width = 4, background = "blue",
            p("Explore the biomes we have already. Hover the points",
              " to see the site code, and click in the table to view only the",
              " desired biome/s"),
            br()
          )
          
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
      
      # Methods tab ####
      tabPanel(
        title = 'Methods',
        
        # valueboxes row
        fluidRow(
          valueBoxOutput("plants"),
          valueBoxOutput("methods"),
          box(
            # title = "Methods tab",
            width = 4, background = "blue",
            p("A little information about the methods used to obtain the sap",
              " flow data. Hover over the bars to see which sites uses the ",
              "corresponding method. Clicking in the table plots the",
              " number of plants per site for the given method/s")
          )
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
            p('HFD: Heat Field Deformation'),
            p('HPTM: Heat Pulse Tmax Method'),
            p('HR: Heat Ratio'),
            p('TSHB: Trunk Segment Heat Balance')
          )
        )
      ),
      
      # Species tab ####
      tabPanel(
        title = "Species",
        
        # valueboxes row
        fluidRow(
          valueBoxOutput("species"),
          valueBoxOutput("genus"),
          box(
            # title = "Species tab",
            width = 4, background = "blue",
            p("Species and genera plots to dive in the taxonomy. Click on the",
              " bars to highlight the site contribution to the species or genera"),
            br()
          )
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
      
      # Contributors tab ####
      tabPanel(
        title = 'Contributors',
        
        # valueboxes row
        fluidRow(
          valueBoxOutput('institutions'),
          contributorsvbOutput('contributors_2'),
          box(
            # title = "Contributors tab",
            width = 4, background = "blue",
            p("We are nothing without the people who kindly contributed to this",
              " project sending us their data and attending the feedback process."),
            p("Thank you all!")
          )
        ),
        
        # contributors table
        fluidRow(
          column(
            width = 1
          ),
          column(
            width = 10,
            DT::dataTableOutput('contributorsTable', height = 'auto', width  = 'auto'),
            '* Only first contributor for each dataset is shown'
          )
        )
      )
    ),
    
    # page footer ####
    fluidRow(
      column(
        width = 10
      ),
      column(
        width = 2,
        "Made by the SAPFLUXNET Team", br(),
        "with ", a(href = 'https://shiny.rstudio.com/', "shiny"),
        " and ", a(href = 'https://rstudio.github.io/shinydashboard/index.html',
                   "shinydashboard")
      )
    )
  )
)
