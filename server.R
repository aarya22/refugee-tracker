library("leaflet")
library("shiny")
library("dplyr")
library("geojsonio")
library("maps")
library("countrycode")
library("ggplot2")

server <- function(input, output) {
  
  source("points_to_line.R")
  
  # Read in necessary csv files
  refugee <- read.csv("data/resettlement_map.csv", stringsAsFactors = FALSE)
  asylum <- read.csv("data/asylum_seekers_map.csv", stringsAsFactors = FALSE)
  demo1 <- read.csv("data/demographics_agg.csv", stringsAsFactors = FALSE)
  world.cities <- read.csv("data/world.cities.csv", stringsAsFactors = FALSE)
  asylum.in.grouped <- read.csv("data/asylum.in.grouped.csv", stringsAsFactors = FALSE)
  asylum.out.grouped <- read.csv("data/asylum.out.grouped.csv", stringsAsFactors = FALSE)
  time.series <- read.csv('data/time_series.csv', stringsAsFactors = FALSE, fileEncoding
                          = "UTF-8-BOM")
  time.series[time.series=="*"]<-NA
  time.series <- na.omit(time.series)

  # Read in json file for map
  world <- geojsonio::geojson_read("json/countries.geo.json", what = "sp")
  
  # Create the refugee map
  output$rmap <- renderLeaflet({
    leaflet(world) %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/aarya22/cje65iy6k03db2rs272v1h8ld/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYWFyeWEyMiIsImEiOiJjamU2NWNvNWUwMDVrMzNvY3Q0NGluZ3o0In0.v3dVCyNNhVq-vFtL237dGw",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.33251, lat = -7.324879e+00, zoom = 3) %>%
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
           fillOpacity = 0.7)
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
  output$in.text <- renderUI({

    text <- paste0("In the beginning of ", input$yearInput,  
                  ", there were about ", round(filtered.in()$people.in, 2),
                   " asylum seekers that were from ", input$countryInput, 
                  " who were seeking asylum in a different country. Out of those seekers,",
                  "the United Nations provided assitance to about ", 
                  round(filtered.in()$un.help.percent, 2),
                  "% of the refugees.  Note: The column \" # of People that UN Helped \" displays the total number",
                  " of refugees that the UN helped in that year for that country. 
                  If table is empty, there is no data found for that  year or country.")
    HTML(paste("<h4>",text,"</h4>", sep=""))
    
  })
  
  #output text explaining data for asylum seekers leaving country
  output$out.text <- renderUI({
    
    text <- paste0("In the beginning of ", input$yearInput,  
           ", there were about ", round(filtered.out()$people.out, 2),
           " asylum seekers that came to ", input$countryInput, 
           " who were seeking asylum. Out of those seekers, ",
           "the United Nations provided assitance to about ", round(filtered.out()$un.help.percent, 2),
           "% of the refugees.  Note: The column \" # of People that UN Helped \" displays the total number",
           " of refugees that the UN helped in that year for that country.")
    HTML(paste("<h4>",text,"</h4>", sep=""))
    
    
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

  # Ranking
    
  # How to organize the dataframe
  # 1. Combine all of the values for the same countries regardless of the type of refugeee for EACH YEAR
  # 2. Sort by descending order (Highest to lowest)
  new.order <- arrange(time.series, Value) 
  #new.order(complete.cases(1,),)
    
  # Create a table
  output$ranking <- renderDataTable({
    # If user chooses in direction
    if (input$rDirection == 'In') {
      # Filter the columns of interest
      in.year <- filter(time.series, Year == input$year, Population.type == input$rType)
  
      if (nrow(in.year) < 1){
        showNotification("No data available", type = "error")
        return()
      }
      in.year[,5] <- sapply(in.year[,5], as.numeric)
      in.year <- aggregate(x = in.year$Value, by = list(in.year$Year, in.year$Country...territory.of.asylum.residence), FUN = sum)
      colnames(in.year)[2] <- "Country"
      colnames(in.year)[3] <- "Value"
      in.data <- arrange(in.year, desc(Value))
      #in.data <- in.data[1:20,]
      order.Value <- in.data$Value
      in.data$Rank <- NA
      in.data$Rank <- 1:nrow(in.data)
      in.data[,3] <- sapply(in.data[,3], as.character)
      in.data <- select(in.data, Rank, Country, Value)
      return(in.data)
      
      # User clicks "outgoing" in the widget
    } else {
      # Filter the columns of interest
      #browser()
      out.year <- filter(time.series, Year == input$year, Population.type == input$rType)
      if (nrow(out.year) < 1) {
        showNotification("No data available", type = "error")
        return()
      }
      out.year[,5] <- sapply(out.year[,5], as.numeric)
      out.year <- aggregate(x = out.year$Value, by = list(out.year$Year, out.year$Country...territory.of.asylum.residence), FUN = sum)
      colnames(out.year)[2] <- "Country"
      colnames(out.year)[3] <- "Value"
      out.data <- arrange(out.year, desc(Value))
      #out.data <- out.data[1:20, ]
      order.Value <- out.data$Value
      out.data$Rank <- NA
      out.data$Rank <- 1:nrow(out.data)
      colnames(out.data)[2] <- "Origin"
      colnames(out.data)[3] <- "Leaving"
      out.data[,3] <- sapply(out.data[,3], as.character)
      out.data <- select(out.data, Rank, Origin, Leaving)
      return(out.data)
    }
  })
  
  # Graph
  output$graph <- renderPlot({
    
    # Filter out given values
    graph.values <- filter(time.series, time.series$Country...territory.of.asylum.residence == input$gCountry,
                           time.series$Population.type == input$gType)
    
    # If no values throw error
    if(nrow(graph.values) < 1) {
      showNotification("No data available", type = "error")
      return()
    }
    
    graph.values[,5] <- sapply(graph.values[,5], as.numeric)
    
    graph.values.grouped <- aggregate(x = graph.values$Value, by = list(graph.values$Year), FUN = sum)%>%
      na.omit(graph.values.grouped)
    colnames(graph.values.grouped)[1] <- "Year"
    colnames(graph.values.grouped)[2] <- "Value"
    
    # Set themes for the plot
    t <- paste(input$gCountry, " (", input$gType,")", sep = "")
    mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                    axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                    axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

    p <- ggplot(data = graph.values.grouped) +
          geom_point(mapping = aes(x = Year, y = Value), color = "blue") +
          geom_line(mapping = aes(x = Year, y = Value), color = "blue")
    
    p <- p + mytheme + labs(title = t, y=input$gType, x = "Year") + theme_minimal()
    print(p)
  })

  # Gets the coordinates of a given country
  get_coords <- function(country) {
    host.coords <- filter(world.cities, country.etc == country, capital == "1") %>% select(lat, long)
    return(host.coords)
  }
  
  # Takes a query from the refugee file and transforms it into a dataframe
  getLatLong <- function(origin.countries, origin.coords, host.coords) {
    
    # Make Long/Lat dataframe
    origin.coords <- as.data.frame(origin.coords)[1,]
    originLat <- unname(unlist(origin.coords[ , grepl( "lat" , names( origin.coords ) ) ]))
    originLong <- unname(unlist(origin.coords[ , grepl( "long" , names( origin.coords ) ) ]))
    hostLat <- rep(host.coords$lat, length(originLat))
    hostLong <- rep(host.coords$long, length(originLong))
    coord.df <- data.frame(group = c(origin.countries),
                           lat = c(originLat, hostLat),
                           long = c(originLong, hostLong))
    return(coord.df)
  }
  
  showRefugeeLines <- function(country, year, direction) {
    
    # Get in/out for a given country, year, direction
    query.in <- as.data.frame(filter(refugee, 
                                     refugee$Country...territory.of.asylum.residence == country, 
                                     refugee$Year==year))
    query.out <- as.data.frame(filter(refugee, 
                                      refugee$Origin == country, 
                                      refugee$Year==year))
    
    # If query.in has no values throw error and is Incoming
    if (length(query.in$Origin) == 0 && direction != "Outgoing") {
      # Have to output no data available
      showNotification("No Incoming Data Available", type = "error")
      return()
    } else if(direction != "Outgoing") {
      
      # Get destination countries and host country
      origin.countries <- query.in$Origin
      country <- query.in$Country...territory.of.asylum.residence[1]
      
      # Get origin countries lat/long
      origin.coords <- lapply(origin.countries, get_coords)
      host.coords <- get_coords(country)
      
      # Remove empty coordinates and respective countries
      empty.vals <- sapply(origin.coords, function(x) dim(x)[1]) > 0
      origin.coords <- origin.coords[empty.vals]
      origin.countries <- origin.countries[empty.vals]
      
      # Get Lat/Long and convert to SpatialLines
      coord.df.in <- getLatLong(origin.countries, origin.coords, host.coords)
      lines.in <- points_to_line(coord.df.in, "long", "lat", "group")
      
      # Get values in order of spatial lines
      query.in <- query.in[query.in$Origin %in% names(lines.in),]
      lines.in.vec <- match(names(lines.in), query.in$Origin)
      q <- query.in[lines.in.vec,]
      dec <- as.numeric(unlist(q$Value))
      
      # Convert from iso3c to country.name
      cnames <- countrycode(q$Origin, "iso3c", "country.name")
      
      # Make labels with country names and values
      labels = sprintf("<strong>%s</strong><br/>%g refugees incoming", cnames, dec) %>% lapply(htmltools::HTML)
      
      # Clear all lines before adding new ones
      leafletProxy("rmap") %>% clearGroup("in-lines") %>% clearGroup("out-lines") 
      leafletProxy("rmap") %>% addPolylines(data=lines.in, group = "in-lines", color = "red",
                                            highlight = highlightOptions(
                                              weight = 10,
                                              color = "#666",
                                              dashArray = "",
                                              fillOpacity = 0.5,
                                              bringToFront = TRUE),
                                            label = labels,
                                            labelOptions = labelOptions(
                                              style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")
                                           )
    }
    
    # If query.out has no values throw error and is Outgoing
    if (length(query.out$Origin) == 0 && direction != "Incoming") {
      # Have to output no data available
      showNotification("No Outgoing Data Available", type="error")
      return()
    } else if(direction != "Incoming") {
      
      # Get host countries and selected country
      origin.countries <- query.out$Country...territory.of.asylum.residence
      country <- query.out$Origin[1]
      
      # Get origin countries lat/long
      origin.coords <- lapply(origin.countries, get_coords)
      host.coords <- get_coords(country)
      
      # Remove empty coordinates and respective countries
      empty.vals <- sapply(origin.coords, function(x) dim(x)[1]) > 0
      origin.coords <- origin.coords[empty.vals]
      origin.countries <- origin.countries[empty.vals]
      origin.dat <- query.out$Value[empty.vals]
      dec <- as.numeric(unlist(origin.dat))
      
      # Get Lat/Long and SpatialLines
      coord.df.out <- getLatLong(origin.countries, origin.coords, host.coords)
      lines.out <- points_to_line(coord.df.out, "long", "lat", "group")
      
      # Get values in order of spatial lines
      query.out <- query.out[query.out$Country...territory.of.asylum.residence %in% names(lines.out),]
      lines.out.vec <- match(names(lines.out), query.out$Country...territory.of.asylum.residence)
      q <- query.out[lines.out.vec,]
      dec <- as.numeric(unlist(q$Value))
      
      # Convert from iso3c to country.name
      cnames <- countrycode(q$Country...territory.of.asylum.residence, "iso3c", "country.name")
      
      # Make labels with country names and value
      labels = sprintf("<strong>%s</strong><br/>%g refugees outgoing", cnames, dec) %>% lapply(htmltools::HTML)
      
      # Clear all lines before adding new ones
      leafletProxy("rmap") %>% clearGroup("out-lines") %>% clearGroup("in-lines")
      leafletProxy("rmap") %>% addPolylines(data=lines.out, group = "out-lines", color = "blue",
                                            highlight = highlightOptions(
                                              weight = 10,
                                              color = "#666",
                                              dashArray = "",
                                              fillOpacity = 0.5,
                                              bringToFront = TRUE),
                                            label = labels,
                                            labelOptions = labelOptions(
                                              style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")
                                           ) 
    }
  }
  
  # If click on a country in the refugee map, show refugee lines
  observe({
    event <- input$rmap_shape_click
    if (is.null(event))
      return()
    country <- map.where(x = event$lng, y = event$lat)
    country <- countrycode(country, origin = "country.name", destination = "iso3c")
    isolate({
      showRefugeeLines(country, input$Years, input$Direction)
    })
  })
  
  # Create the asylum map
  output$amap <- renderLeaflet({
    leaflet(world) %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/aarya22/cje65iy6k03db2rs272v1h8ld/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiYWFyeWEyMiIsImEiOiJjamU2NWNvNWUwMDVrMzNvY3Q0NGluZ3o0In0.v3dVCyNNhVq-vFtL237dGw",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -73.33251, lat = -7.324879e+00, zoom = 3) %>%
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
          fillOpacity = 0.7)
      )
  })
  
  # If click on a country in the asylum map, show asylum lines
  observe({
    event <- input$amap_shape_click
    if (is.null(event))
      return()
    country <- map.where(x = event$lng, y = event$lat)
    country <- countrycode(country, origin = "country.name", destination = "iso3c")
    isolate({
      showAsylumLines(country, input$asYears, input$Decisions)
    })
  })
  
  showAsylumLines <- function(country, year, decision) {
    
    # Get necessary column of data
    if (decision == "All") {
      decision = "Total.decisions" 
    } else if (decision == "Recognized") {
      decision = "recognized"
    } else if (decision == "Rejected") {
      decision = "rejected"
    } else if (decision == "UN-Assisted") {
      decision = "un.assist"
    } else if (decision == "Pending") {
      decision = "pending"
    }
    
    # Filter given information and remove NA's and 0 values
    query <- as.data.frame(filter(asylum, 
                                  asylum$Country...territory.of.asylum.residence == country, 
                                  asylum$Year==year) %>% select(Year, Country...territory.of.asylum.residence, Origin, decision))
    query <- query[complete.cases(query), ]
    names(query)[4] <- "decision"
    query <- filter(query, decision > 0)
    
    # if query is 0 length, throw error
    if (length(query$Origin) == 0) {
      showNotification("No Data Available", type = "error")
      return()
    }
    
    # Aggregate for the given data on all Countries
    query <- aggregate(. ~ Country...territory.of.asylum.residence + Origin + Year, data = query, sum)
    
    # Get countries of origin for asylum seekers and host country
    origin.countries <- query$Origin
    country <- query$Country...territory.of.asylum.residence[1]
    
    # Get origin countries lat/long
    origin.coords <- lapply(origin.countries, get_coords)
    host.coords <- get_coords(country)
    
    # Remove empty coordinates and respective countries
    empty.vals <- sapply(origin.coords, function(x) dim(x)[1]) > 0
    origin.coords <- origin.coords[empty.vals]
    origin.countries <- origin.countries[empty.vals]
    
    origin.dat <- query$decision[empty.vals]
    dec <- as.numeric(unlist(origin.dat))
    
    # Get Lat/Long and SpatialLines
    coord.df <- getLatLong(origin.countries, origin.coords, host.coords)
    lines <- points_to_line(coord.df, "long", "lat", "group")
    
    # Get order of values in SpatialLines
    query <- query[query$Origin %in% names(lines),]
    lines.vec <- match(names(lines), query$Origin)
    q <- query[lines.vec,]
    dec <- as.numeric(unlist(q$decision))
    
    # Convert from iso3c to country name
    cnames <- countrycode(q$Origin, "iso3c", "country.name")
    
    # Make labels for lines
    labels = sprintf("<strong>%s</strong><br/>%g applications", cnames, dec) %>% lapply(htmltools::HTML)
    
    # Clear lines before adding new lines
    leafletProxy("amap") %>% clearGroup("as-lines") 
    leafletProxy("amap") %>% addPolylines(data=lines, group = "as-lines", color = "green",
                                          highlight = highlightOptions(
                                            weight = 10,
                                            color = "#666",
                                            dashArray = "",
                                            fillOpacity = 0.5,
                                            bringToFront = TRUE),
                                          label = labels,
                                          labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")
                                          )
  }
  
  # Outputs bar plot for refugees in a given country and year
  output$refbar <- renderPlot({
    # Get necessary data 
    r <- filter(demo1, demo1$Country == input$countryInput, demo1$Year == input$yearInput)
    
    # If query is not 1, throw error
    if (nrow(r) != 1) {
      showNotification("No Refugee Demographics Available", type = "error")
      return()
    }
    
    # Set labels and create bar plot
    slices <- as.numeric(r[,4:9])
    lbls <- c("Female (0-17)", "Female (18+)", "Female (Unknown)", "Male (0-17)", "Male (18+)", "Male (Unknown)")
    title <- paste("Refugee Demographics of", input$countryInput,"in", input$yearInput)
    color <- c("red", "red", "red", "blue", "blue", "blue")
    bar <- barplot(slices,xlab = "Category",ylab = "# of Refugees",col = color,
                   main = title)
    text(x=bar[,1], y=-1, adj=c(1, 1), lbls, cex=0.8, srt=45, xpd=TRUE)
  })
  
  # Make title for summary page
  output$reftitle <- renderUI({
    title <- paste(input$countryInput, "in", input$yearInput)
    HTML(paste("<h1>",title,"</h1>", sep=""))
  })
  
  # Make description for bar chart
  output$bartext <- renderUI({
    text <- paste("The histogram displays data about the age demographics of the people
                  coming into",input$countryInput,"in", input$yearInput,
                  "The age groups represented in the data are from 0-17 years, 18+ years, 
                   and years that are unknown because they were not reported for that population.")
    HTML(paste("<h4>",text,"</h4>", sep=""))
  })
  
}
