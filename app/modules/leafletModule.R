
leafletMapUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(outputId = ns("map"))
  
}

leafletMapServer <- function(input, output, session, map_dat, county, tab, build) {
  
  output$map <- renderLeaflet({
    #labs <- sprintf("<h5>%s</h5>\n<h5>%s</h5>\n<p>%s</p>", map_dat$Admin2, prettyNum(map_dat$cases_per, big.mark = ","), map_dat$Province_State) 
    map_bounds <- st_bbox(map_dat)
    
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax))
  })
  
  my_built <- reactive({ if(is.null(build())) "acs_unemp" else build() })
  
  
  observe({
    req(tab() == "map")
    
    county_index <- composite_dat %>%
      filter(county_full_name == county()) %>%
      st_intersects(composite_dat) %>%
      as.data.frame() %>%
      distinct(col.id)
    
    county_index <- unique(composite_dat[county_index$col.id, ]$county_full_name)
    
    new_map_dat <- map_dat %>%
      filter(county_full_name %in% county_index) %>%
      as_tibble() %>%
      select(-geometry) %>%
      select(tract_geoid, contains(all_of(my_built()))) %>%
      select(tract_geoid, ends_with("score")) %>%
      pivot_longer(names_to = "var", values_to = "score", cols = ends_with("score")) %>%
      group_by(tract_geoid) %>%
      summarise(selected_score = sum(score)) %>%
      left_join(map_dat) %>%
      st_as_sf()
    
    map_bounds <- st_bbox(new_map_dat)
    
    pal <- colorNumeric(palette = "YlGnBu", domain = new_map_dat$selected_score)
    
    leafletProxy(mapId = "map", data = new_map_dat) %>%
      clearShapes() %>%
      clearControls() %>%
      flyToBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax)) %>%
      addPolygons(weight = 0.5, color = "#151515", fillColor = ~pal(selected_score),
                  fillOpacity = 0.8, highlightOptions = highlightOptions(color = "#FFFFFF",
                                                                         weight = 2,
                                                                         bringToFront = TRUE),
                  label = ~selected_score) %>%
      addLegend(position = "bottomright", pal = pal,
                values = ~selected_score, title = "Total Z-Score")
      
  })
  
}