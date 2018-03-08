library("leaflet")
library("shiny")
library("dplyr")
library("geojsonio")
library('plotly')

server <- function(input, output) {
  
  asylum.in.grouped <- read.csv("data/asylum.in.grouped.csv", stringsAsFactors = FALSE)
  asylum.out.grouped <- read.csv("data/asylum.out.grouped.csv", stringsAsFactors = FALSE)
  
  world <- geojsonio::geojson_read("json/countries.geo.json", what = "sp")
  
  # Examples to demonstrate lines
  mydf <- data.frame(Observation = c("A", "B"),
                     InitialLat = c(62.469722,48.0975),
                     InitialLong = c(6.187194, 16.3108),
                     NewLat = c(51.4749, 51.4882),
                     NewLong = c(-0.221619, -0.302621),
                     stringsAsFactors = FALSE)
  
  mydf2 <- data.frame(group = c("A", "B"),
                      lat = c(mydf$InitialLat, mydf$NewLat),
                      long = c(mydf$InitialLong, mydf$NewLong))
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(world) %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/aarya22/cje65iy6k03db2rs272v1h8ld/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYWFyeWEyMiIsImEiOiJjamU2NWNvNWUwMDVrMzNvY3Q0NGluZ3o0In0.v3dVCyNNhVq-vFtL237dGw",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.33251, lat = -7.324879e+00, zoom = 3) %>%
      addPolylines(data = mydf2, lng = ~long, lat = ~lat, group = ~group) %>%
      addPolygons(
         weight = 1,
         opacity = 0.7,
         dashArray = "3",
         color = "#d6d6d6",
         fillOpacity = 0.3,
         highlight = highlightOptions(
           weight = 5,
           color = "#666",
           dashArray = "",
           fillOpacity = 0.7,
           bringToFront = TRUE)
      )
  })
  
#Information tab
  
  #reactive data for asylum seekers coming into country
  filtered.in <- reactive({
    asylum.in.grouped <- filter(asylum.in.grouped, year == input$yearInput
                                & country == input$countryInput) 
  })
  
  #reactive data for asylum seekers leaving country
  filtered.out <- reactive({
    asylum.out.grouped <- filter(asylum.out.grouped, year == input$yearInput &
                                   origin == input$countryInput) 
  })

  #output text explaining data for asylum seekers coming in 
  output$in.text <- renderText({

    paste0("In the beginning of ", input$yearInput,  
          ", there were about ", filtered.in()$people.in,
           " asylum seekers that were from ", input$countryInput, 
          " who were seeking asylum in a different country. Out of those seekers,",
          "the United Nations provided assitance to about ", 
          round(filtered.in()$un.help.percent, 2),
          "% of the refugees.  Note: The column \" un.helped \" displays the total number",
          " of refugees that the UN helped in that year for that country. 
          If table is empty, there is no data found for that  year or country")
    
    
  })
  
  #output text explaining data for asylum seekers leaving country
  output$out.text <- renderText({
    
    paste0("In the beginning of ", input$yearInput,  
           ", there were about ", filtered.out()$people.out,
           " asylum seekers that came to ", input$countryInput, 
           " who were seeking asylum. Out of those seekers,",
           "the United Nations provided assitance to about ", filtered.out()$un.help.percent,
           "% of the refugees.  Note: The column \" un.helped \" displays the total number",
           " of refugees that the UN helped in that year for that country.")
    
    
  })

  #output data for asylum seekers coming into country
  output$in.country <- renderTable({
    filter.in <-filtered.in()
    names(filter.in)[4:5] <- c("# of People Entering Country",
                               "# of People that UN Helped")
    select(filter.in, "# of People Entering Country", "# of People that UN Helped")
    
  })
  
  #output data for asylum seekers leaving country
  output$out.country <- renderTable({
    
    filter.out <- filtered.out()    
    names(filter.out)[4:5] <- c("# of People Leaving Country",
                                                  "# of People that UN Helped")
    select(filter.out, "# of People Leaving Country",
           "# of People that UN Helped")
    
  })
}
  
