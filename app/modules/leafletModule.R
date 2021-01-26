
leafletMapUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(outputId = ns("map"))
  
}

leafletMapServer <- function(input, output, session, map_dat, county, tab, build) {
  
  output$map <- renderLeaflet({
    
    map_bounds <- st_bbox(map_dat)
    
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
      mutate(status = ifelse(selected_score <= 0, "#28a745", "#6f42c1")) %>%
      st_as_sf()
    
    map_bounds <- st_bbox(new_map_dat)
    
    pal <- colorBin(palette = "BuPu", domain = new_map_dat$selected_score, bins = 6, pretty = FALSE)
    
    labs <- glue("<p style='margin: 0; padding: 0; line-height: 15px; font-size: 10px;'><b>{new_map_dat$tract_name}</b></p>
                 <p style='margin: 0; padding: 0; line-height: 20px; color: {new_map_dat$status}; font-size: 14px;'>{round(new_map_dat$selected_score,2)}</p>
                 <p style='margin: 0; padding: 0; line-height: 15px; font-size: 12px; color: {new_map_dat$status};'><b>Custom Built Total Z-Score</b></p>")
    
    leafletProxy(mapId = "map", data = new_map_dat) %>%
      clearShapes() %>%
      clearControls() %>%
      flyToBounds(lng1 = as.numeric(map_bounds$xmin), lat1 = as.numeric(map_bounds$ymin),
                  lng2 = as.numeric(map_bounds$xmax), lat2 = as.numeric(map_bounds$ymax)) %>%
      addPolygons(weight = 1, color = "#151515", fillColor = ~pal(selected_score),
                  fillOpacity = 0.8, highlightOptions = highlightOptions(color = "#FFFFFF",
                                                                         weight = 2,
                                                                         bringToFront = TRUE),
                  label = ~map(labs, HTML)) %>%
      addLegend(position = "bottomright", pal = pal,
                values = ~selected_score, title = "Total Z-Score", labFormat = labelFormat(between = " to ", transform = function(x) round(as.numeric(x), digits = 2)))
      
  })

}