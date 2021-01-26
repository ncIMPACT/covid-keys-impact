
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
    
    final_map_dat <- filter(new_map_dat, county_full_name == county())
    
    labs <- glue("<p style='margin: 0; padding: 0; line-height: 15px; font-size: 10px;'><b>{final_map_dat$tract_name}</b></p>
                 <p style='margin: 0; padding: 0; line-height: 20px; font-size: 14px;'>{round(final_map_dat$variable,2)}</p>
                 <p style='margin: 0; padding: 0; line-height: 15px; font-size: 12px;'><b>{selection()}</b></p>")
    
    map_bounds <- st_bbox(final_map_dat)
    
    leafletProxy(mapId = "map", data = final_map_dat) %>%
      clearShapes() %>%
      clearControls() %>%
      flyToBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax)) %>%
      addPolygons(weight = 1, color = "#151515", fillColor = ~pal(variable),
                  fillOpacity = 0.8, highlightOptions = highlightOptions(color = "#FFFFFF",
                                                                         weight = 2,
                                                                         bringToFront = TRUE),
                  label = ~map(labs, HTML)) %>%
      addPolygons(data = selected_county, weight = 3, color = "#ec008b", opacity = 1, fill = FALSE)
    
  })
  
}