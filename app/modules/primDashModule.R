
primDashUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns("map"))
  )
  
}

primDashServer <- function(input, output, session, counties, dat, selection, county, tab) {
  
  output$map <- renderLeaflet({
    map_bounds <- st_bbox(dat)
    
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.Positron, group = "CartoDB Positron",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(provider = providers$CartoDB.Voyager, group = "CartoDB Voyager",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax)) %>%
      addLayersControl(baseGroups = c("CartoDB Voyager", "CartoDB Positron", "CartoDB Dark Matter"),
                       position = "topright", options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    req(tab() == "metricDash")
    
    selected_county <- counties %>%
      filter(namelsad == county())
    
    new_map_dat <- dat %>%
      select(1:5, all_of(selection())) %>%
      rename(variable = 6)
    
    pal <- colorBin(palette = "BuPu", domain = new_map_dat$variable, bins = 6, pretty = FALSE)
    
    map_bounds <- st_bbox(filter(new_map_dat, county_full_name == county()))
    
    leafletProxy(mapId = "map", data = filter(new_map_dat, county_full_name == county())) %>%
      clearShapes() %>%
      clearControls() %>%
      flyToBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax)) %>%
      addPolygons(weight = 1, color = "#151515", fillColor = ~pal(variable),
                  fillOpacity = 0.8, highlightOptions = highlightOptions(color = "#FFFFFF",
                                                                         weight = 2,
                                                                         bringToFront = TRUE)) %>%
      addPolygons(data = selected_county, weight = 3, color = "#ec008b", opacity = 1, fill = FALSE)
    
  })
  
}