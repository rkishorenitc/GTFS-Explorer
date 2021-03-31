options(shiny.maxRequestSize=50*1024^2)
# Define a server for the Shiny app
function(input, output) {
  gtfs_file <- reactive({
    x <- read_gtfs(input$selectFile$datapath)
    return(x)
  })
  
  keyfigures <- reactive({
    req(input$selectFile)
    data <- gtfs_file()
    modes <- length(unique(data$routes$route_type))
    routes <- length(unique(data$routes$route_id))
    stops <- length(unique(data$stops$stop_id))
    trips <- length(unique(data$trips$trip_id))
    keyfigures <- list("modes" = HTML(paste(format(modes, big.mark = " "))),
                       "routes" = HTML(paste(format(routes, big.mark = " "))),
                       "stops" = HTML(paste(format(stops, big.mark = " "))),
                       "trips" = HTML(paste(format(trips, big.mark = " "))))
    return(keyfigures)
  })
  
  summary <- reactive({
    req(input$selectFile)
    data <- gtfs_file()
    routes <- data$routes
    pattern <- data$stop_times
    stops <- data$stops[c(-6,-7,-8,-9)]
    trips <- data$trips
    trips <- unique(trips[c(1,3,6)])
    pattern <- pattern[c(-2,-3,-6,-7,-8,-9)]
    pattern <- unique(pattern)
    pattern <- merge(pattern, stops, by = "stop_id")
    pattern <- merge(pattern, trips, by = "trip_id")
    pattern <- merge(pattern, routes[c(1,3,4,5)], by = "route_id")
    pattern <- unique(pattern[c(-2)])
  })
  
  output$staticBox_modes <- renderValueBox({
    valueBox(
      keyfigures()$modes,
      subtitle = "Modes Covered",
      icon     = icon("filter"),
      color    = "red",
      width    = NULL
    )
  })
  
  output$staticBox_stops <- renderValueBox({
    valueBox(
      keyfigures()$stops,
      subtitle = "Stops Covered",
      icon     = icon("map-marker"),
      color    = "red",
      width    = NULL
    )
  })
  
  output$staticBox_routes <- renderValueBox({
    valueBox(
      keyfigures()$routes,
      subtitle = "Routes Covered",
      icon     = icon("route"),
      color    = "red",
      width    = NULL
    )
  })
  
  output$staticBox_trips <- renderValueBox({
    valueBox(
      keyfigures()$trips,
      subtitle = "Planned Trips",
      icon     = icon("bus"),
      color    = "red",
      width    = NULL
    )
  })
  
  output$stopmap <- renderLeaflet({
    req(input$selectFile)
    data <- gtfs_file()
    stops <- data$stops
    leaflet(data = stops) %>%
      addTiles() %>%
      addMarkers(lng = ~stop_lon,
                 lat = ~stop_lat,
                 layerId = ~stop_name,
                 clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T),
                 popup = paste("Stop ID: ", stops$stop_id, "<br>",
                               "Stop Name: ", stops$stop_name, "<br>",
                               "Zone: ", stops$zone_id))
    
  })
  
  output$ModeDropDown <- renderUI({
    req(input$selectFile)
    data <- summary()
    selectInput(inputId = "selectMode", label = "Select Mode:", c("All", c(unique(as.character(data$route_type)))))
  })
  
  output$RouteDropDown <- renderUI({
    req(input$selectMode)
    if (input$selectMode != "All"){
      data <- summary()
      data <- subset(data, route_type == input$selectMode)
    }
    else {
      data <- summary()
    }
    selectInput(inputId = "selectRoute", label = "Select Route:", c(unique(as.character(data$route_short_name))))
  })
  
  output$DirectionDropDown <- renderUI({
    req(input$selectMode)
    req(input$selectRoute)
    data <- summary()
    data <- subset(data, route_short_name == input$selectRoute)
    selectInput(inputId = "selectDirection", label = "Select Direction:", c(unique(as.character(data$direction_id))))
  })
  
  output$routemap <- renderLeaflet({
    req(input$selectMode)
    req(input$selectRoute)
    req(input$selectDirection)
    data <- summary()
    data <- subset(data, route_short_name == input$selectRoute)
    data <- subset(data, direction_id == input$selectDirection)
    leaflet(data = data) %>%
      addTiles() %>%
      addMarkers(lng = ~stop_lon,
                 lat = ~stop_lat,
                 layerId = ~stop_name,
                 popup = paste("Stop ID: ", data$stop_id, "<br>",
                               "Stop Name: ", data$stop_name, "<br>",
                               "Stop Sequence: ", data$stop_sequence))
    
  })
  
  output$map <- renderLeaflet({
    req(input$selectFile)
    data <- gtfs_file()
    stops <- data$stops
    leaflet(data = stops) %>%
      addTiles() %>%
      addMarkers(lng = ~stop_lon,
                 lat = ~stop_lat,
                 layerId = ~stop_name,
                 clusterOptions = markerClusterOptions(zoomToBoundsOnClick = F),
                 popup = paste("Stop ID: ", stops$stop_id, "<br>",
                               "Stop Name: ", stops$stop_name, "<br>",
                               "Zone: ", stops$zone_id))
    
  })
  
  # mapdata <- reactive({
  #   req(input$map_marker_click$id)
  #   selectedStop <- input$map_marker_click$id
  #   return(selectedStop)
  # })

  output$departurePlot <- renderPlot({
    req(input$selectFile)
    gtfs <- gtfs_file()
    selectedStop <- as.character(input$map_marker_click$id)
    selectedDate <- as.character(input$date)
    gtfs <- gtfs %>% set_hms_times()
    # get the id of the first stop in the trip's stop sequence
    first_stop_id <- gtfs$stop_times %>% 
      group_by(trip_id) %>% 
      summarise(stop_id = stop_id[which.min(stop_sequence)])
    
    # join with the stops table to get the stop_name
    first_stop_names <- left_join(first_stop_id, gtfs$stops, by="stop_id")
    
    # rename the first stop_name as trip_origin
    trip_origins <- first_stop_names %>% select(trip_id, trip_origin = stop_name)
    
    # join the trip origins back onto the trips
    gtfs$trips <- left_join(gtfs$trips, trip_origins, by = "trip_id")
    stop_ids <- gtfs$stops %>% 
      filter(stop_name == selectedStop) %>% 
      select(stop_id)
    departures <- stop_ids %>% 
      inner_join(gtfs$stop_times %>% 
                   select(trip_id, arrival_time_hms, 
                          departure_time_hms, stop_id), 
                 by = "stop_id")
    
    departures <- departures %>% 
      left_join(gtfs$trips %>% 
                  select(trip_id, route_id, 
                         service_id, trip_headsign, 
                         trip_origin), 
                by = "trip_id") 
    departures <- departures %>% 
      left_join(gtfs$routes %>% 
                  select(route_id, 
                         route_short_name), 
                by = "route_id")
    gtfs <- gtfs %>% 
      set_hms_times() %>% 
      set_date_service_table()
    services_on_180823 <- gtfs$.$date_service_table %>% 
      filter(date == selectedDate) %>% select(service_id)
    
    departures_180823 <- departures %>% 
      inner_join(services_on_180823, by = "service_id")
    route_colors <- gtfs$routes %>% select(route_id, route_short_name, route_color)
    route_colors$route_color[which(is.na(route_colors$route_color))] <- "454545"
    route_colors <- setNames(paste0("#", route_colors$route_color), route_colors$route_short_name)
    
    ggplot(departures_180823) + theme_bw() +
      geom_point(aes(y=trip_headsign, x=departure_time_hms, color = route_short_name), size = 0.2) +
      scale_x_time(breaks = seq(0, max(as.numeric(departures$departure_time_hms)), 3600), labels = scales::time_format("%H:%M")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position = "bottom") +
      scale_color_manual(values = route_colors) +
      labs(title = paste0("Departures from ",selectedStop, " on ", selectedDate))

  })

  output$validationTable <- renderTable({
    req(input$selectFile)
    data <- gtfs_file()
    validation_results <- attr(data, "validation_result")
    validation <- as.data.frame(table(validation_results$file, validation_results$validation_details))
    validation <- dcast(validation, Var1~Var2)
    colnames(validation) <- c("Atrribute", "Optional Field Absent", "Optional File Absent", "Undocumented File")
    return(validation)
  })

}