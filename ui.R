library("leaflet")
library("shiny")
library('shinythemes')

months <- c("All", month.name)
asylum.in.grouped <- read.csv("data/asylum.in.grouped.csv", stringsAsFactors = FALSE)
time.series <- read.csv("data/time_series.csv", stringsAsFactors = FALSE)

navbarPage("Refugee Tracker", id="nav", theme = shinytheme("superhero"),
           
  tabPanel("Refugee Map",
    div(class="outer",
      tags$head(
          # Include our custom CSS
          includeCSS("styles.css")
      ),
                
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("rmap", width = "100%", height = "100%"),
      
      absolutePanel(id = "controls-refugee", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Refugee Explorer (1959-2015)"),
                    
                    selectInput("Years", "Year", c(1959:2015)),
                    
                    selectInput("Direction", "Refugee Direction", c("Incoming", "Outgoing"))
      )
    )
  ),
  
  tabPanel("Asylum Map",
           div(class="outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
               ),
               
               # If not using custom CSS, set height of leafletOutput to a number instead of percent
               leafletOutput("amap", width = "100%", height = "100%"),
               
               absolutePanel(id = "controls-asylum", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Asylum Seekers (2000-2016)"),
                             
                             selectInput("Decisions", "Decisions",
                                         c("All", "Recognized", "Rejected", "UN-Assisted", "Pending")),
                             
                             selectInput("asYears", "Year", c(2000:2016))
                             
                             # Should I do monthly? Will be confusing because cannot select decision input.
                             # selectInput("Months", "Month", months)
               )
           )
  ),
  
  tabPanel("Ranking",
      sidebarPanel(id = "controls-ranking", class = "panel panel-default", fixed = TRUE,
         draggable = TRUE, top = "auto", left = 60, right = "auto", bottom = "auto",
         width = 60, height = "auto",
                         
         h2("Ranking Explorer"),
                         
         # Input: dropdown menu to select the year
         selectInput(input = 'year', label = "Select Year:", 
            choices = c(1951:2016)),
                         
         # Input: Use dropdown to select unit of measurement
         selectInput(input = 'rDirection', label = "Select outgoing or incoming refugee data:", 
                     choices = list('In' = 'In', 
                                   'Out'= 'Out')),
        
         # Input: Use dropdown to select unit of measurement
         selectInput(input = 'rType', label = "Select type of refugee:", 
                     choices = list('Refugees (incl. refugee-like situations)' = 'Refugees (incl. refugee-like situations)',
                                   "Returnees" = 'Returnees',
                                   "Returned IDPs" = 'Returned IDPs',
                                   "Stateless" = 'Stateless',
                                   "Asylum-seekers" = 'Asylum-seekers',
                                   "Others of concern" = 'Others of concern',
                                   'Internally displaced persons'= 'Internally displaced persons'))
      ),
      dataTableOutput("ranking")
  ),
  
  tabPanel("Time-Series",
           sidebarLayout(
             sidebarPanel(id = "controls-graph", class = "panel panel-default",
                           
                           h2("Graph Explorer"),
                           
                           # Input: dropdown menu to select the year
                           selectInput(input = 'gCountry', label = "Select Country:", 
                                       choices = unique(time.series$Country...territory.of.asylum.residence)),
                           
                           selectInput(input = 'gType', label = "Select type of refugee:", 
                                       choices = list('Refugees (incl. refugee-like situations)' = 'Refugees (incl. refugee-like situations)',
                                                      "Returnees" = 'Returnees',
                                                      "Returned IDPs" = 'Returned IDPs',
                                                      "Stateless" = 'Stateless',
                                                      "Asylum-seekers" = 'Asylum-seekers',
                                                      "Others of concern" = 'Others of concern',
                                                      'Internally displaced persons'= 'Internally displaced persons'))
             ),
             mainPanel (
               plotOutput("graph")
             )
           )
  ),
  
   tabPanel("Country Summary", 
           div(class="summary", style = "overflow-y:scroll; max-height: 600px",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
               ),
               
               htmlOutput("reftitle"),
               
               h3("Refugee Demographics"),
               plotOutput("refbar", height = 550, width = 500),
               htmlOutput("bartext"),
               
               h3("Refugees Coming in Country for Asylum"),
               tableOutput('in.country'),
               htmlOutput('in.text'),
               
               h3("Refugees Leaving Country Seeking Asylum"),
               tableOutput('out.country'),
               htmlOutput('out.text'),
               
               absolutePanel(id = "controls-summary", class = "panel panel-default", fixed = FALSE,
                             draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Country Summary"),
                             
                             selectInput("countryInput", "Select Country",
                                         choices = unique(as.character(asylum.in.grouped$country))),
                             
                             selectInput("yearInput", "Select Year", c(2001:2016))
               )
               
           )
    )
)
