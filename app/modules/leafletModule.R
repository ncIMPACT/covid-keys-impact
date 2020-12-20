
leafletMapUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(outputId = ns("map"))
  
}

leafletMapServer <- function(input, output, session, map_dat = NULL, county = NULL, tab = NULL) {
  
  output$map <- renderLeaflet({
    #labs <- sprintf("<h5>%s</h5>\n<h5>%s</h5>\n<p>%s</p>", map_dat$Admin2, prettyNum(map_dat$cases_per, big.mark = ","), map_dat$Province_State) 
    map_bounds <- st_bbox(map_dat)
    
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax))
  })
  
  observe({
    req(tab() == "map")
    
    new_map_dat <- map_dat %>%
      filter(county_full_name == county())
    
    map_bounds <- st_bbox(new_map_dat)
    
    leafletProxy(mapId = "map", data = new_map_dat) %>%
      clearShapes() %>%
      flyToBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax)) %>%
      addPolygons()
  })
  
}