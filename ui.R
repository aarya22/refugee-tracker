library("leaflet")
library("shiny")

months <- c("All", month.name)

navbarPage("Refugee-Tracker", id="nav",
           
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
  
   tabPanel("Country Summary",
           div(class="summary",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css")
               ),
               
               htmlOutput("reftitle"),
               
               plotOutput("refbar", height = 500, width = 500),
               
               absolutePanel(id = "controls-summary", class = "panel panel-default", fixed = TRUE,
                             draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h2("Country Summary"),
                             
                             selectInput("Country", "Country",
                                         c("Afghanistan", "Sierra Leone")),
                             
                             selectInput("sumYears", "Year", c(2001:2016))
               )
           )
       )
  
)