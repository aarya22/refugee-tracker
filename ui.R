library("leaflet")
library("shiny")
library('shinythemes')

vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("Refugee Tracker", id="nav", theme = shinytheme("superhero"),
           
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
  
  #ui for Information tab
  tabPanel("Information",
           titlePanel("Asylum Seeker Data by Country"),
           
           sidebarPanel(
             
             sliderInput("yearInput", "Choose Year",
                         min = 2001, max = 2016,
                         value = 2001, sep = ""),
             
             selectInput("countryInput", "Select Country",
                         choices = unique(as.character(asylum.in.grouped$country))
             )
            
           ),
           
           h3("Refugees Coming in Country for Asylum"),
           tableOutput('in.country'),
           textOutput('in.text'),
           
           h3("Refugees Leaving Country Seeking Asylum"),
           tableOutput('out.country'),
           textOutput('out.text')
           
 )
)