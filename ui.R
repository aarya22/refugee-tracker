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
  
  tabPanel("Information",
           sidebarPanel(
             selectInput("yearInput", "Choose Year",
                        choices = c("2000", "2001", "2002", "2003", "2004", "2005", "2006",
                         "2007", "2008", "2009", "2010", "2011", "2012", "2013",
                         "2014", "2015", "2016")
              )
             
           ),
           textOutput('unHelped')
           
           
           
           
 )
)