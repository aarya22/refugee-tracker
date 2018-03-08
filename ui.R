library("leaflet")
library("shiny")

vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("Refugee-Tracker", id="nav",
           
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
          # Include our custom CSS
          includeCSS("styles.css")
      ),
                
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("ZIP explorer"),
                    
                    selectInput("color", "Color", vars),
                    selectInput("size", "Size", vars, selected = "adultpop")
                    
                    # plotOutput("histCentile", height = 200),
                    # plotOutput("scatterCollegeIncome", height = 250)
      )
    )
  ),
  
  tabPanel("Ranking", tableOutput("ranking"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
         width = 330, height = "auto",
                         
         h2("ZIP explorer"),
                         
         # Input: dropdown menu to select the year
         selectInput(input = 'year', label = "Select Year:", 
            choices = c(1951:2016)),
                         
    # Input: Use dropdown to select unit of measurement
    selectInput(input = 'Direction', label = "Select outgoing or incoming refugee data:", 
                choices = list('In' = 'In', 
                               'Out'= 'Out')),
    
    # Input: Use dropdown to select unit of measurement
    selectInput(input = 'Type', label = "Select type of refugee:", 
                choices = list('Refugees (incl. refugee-like situations)' = 'Refugees (incl. refugee-like situations)',
                               "Returnees" = 'Returnees',
                               "Returned IDPs" = 'Returned IDPs',
                               "Stateless" = 'Stateless',
                               "Asylum-seekers" = 'Asylum-seekers',
                               "Others of concern" = 'Others of concern',
                               'Internally displaced persons'= 'Internally displaced persons'))
      )
    ),
  tabPanel("Graph", plotOutput("graph"),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         
                         h2("ZIP explorer"),
                         
                         # Input: dropdown menu to select the year
                         selectInput(input = 'Country', label = "Select Country:", 
                                     choices = list('Australia' = 'Australia', 
                                                    'Austria' = 'Austria', 
                                                    'Belgium' = 'Belgium', 
                                                    'Canada' = 'Canada', 
                                                    'Switzerland' = 'Switzerland', 
                                                    'Germany' = 'Germany', 
                                                    'Denmark' = 'Denmark', 
                                                    'Spain' = 'Spain', 
                                                    'France' = 'France', 
                                                    'United Kingdom' = 'United Kingdom', 
                                                    'Greece' = 'Greece', 
                                                    'China, Hong Kong SAR' = 'China, Hong Kong SAR', 
                                                    'Italy' = 'Italy', 
                                                    'Morocco' = 'Morocco', 
                                                    'Luxembourg' = 'Luxembourg', 
                                                    'Netherlands' = 'Netherlands', 
                                                    'Norway' = 'Norway', 
                                                    'Tunisia' = 'Tunisia', 
                                                    'Turkey' = 'Turkey', 
                                                    'United States of America' = 'United States of America', 
                                                    "Sweden" = 'Sweden')),
                         
                         selectInput(input = 'Type', label = "Select type of refugee:", 
                                     choices = list('Refugees (incl. refugee-like situations)' = 'Refugees (incl. refugee-like situations)',
                                                    "Returnees" = 'Returnees',
                                                    "Returned IDPs" = 'Returned IDPs',
                                                    "Stateless" = 'Stateless',
                                                    "Asylum-seekers" = 'Asylum-seekers',
                                                    "Others of concern" = 'Others of concern',
                                                    'Internally displaced persons'= 'Internally displaced persons'))
           )
  )
)
