library(echarts4r)

e_common(font_family = "Source Sans Pro")

# counties object should be passed as a distinct character vector
echartUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("heading")),
    echarts4rOutput(ns("chart"))
  )
  
}

echartServer <- function(input, output, session, dat, starts, county, subtitle = NULL, show = NULL) {
  
  output$heading <- renderUI({
    value_dat <- dat %>%
      filter(county_full_name == county()) %>%
      select(contains(starts)) %>%
      select(ends_with("score")) %>%
      pull(1) %>%
      mean(na.rm = T)
    
    status <- ifelse(value_dat <= 0, "#28a745", "#ffc107")
    text_color <- ifelse(value_dat <= 0, "white", "black")
    main_icon <- ifelse(value_dat <= 0, "arrow-down", "arrow-up")
    
    tags$div(style=glue("background-color:{status}; color:{text_color};"),
             tags$p(class="mx-2", style="text-align: center;", glue("{subtitle} Census Tract Average Z-Score: {round(value_dat, digits=2)}"), icon(name = main_icon)))
    
  })
  
  output$chart <- renderEcharts4r({
    if(starts == "depend") {
      selected_column <- ifelse(as.integer(show()) == 4, 3, as.integer(show()))
      selected_column <- ifelse(as.integer(show()) == 3, 2, selected_column)
      
      new_dat <- dat %>%
        filter(county_full_name == county()) %>%
        select(tract_name, contains(starts)) %>%
        rename(series = all_of(selected_column)) %>%
        arrange(tract_name)
      
      value_dat <- dat %>%
        filter(county_full_name == county()) %>%
        select(contains(starts)) %>%
        select(ends_with("score")) %>%
        pull(1) %>%
        mean(na.rm = T)
      
      status <- ifelse(value_dat <= 0, "#28a745", "#ffc107")
      
      style <- ifelse(selected_column == 2, "percent", "decimal")
      digits <- ifelse(selected_column == 3, 2, 0)
      
      new_dat %>%
        e_charts(x = tract_name) %>%
        e_bar(serie = series, name = subtitle) %>%
        e_x_axis(show = FALSE) %>%
        e_y_axis(formatter = e_axis_formatter(style = style, digits = digits)) %>%
        e_color(color = status) %>%
        e_tooltip(trigger = "item", formatter = e_tooltip_item_formatter(style = style, digits = digits)) %>%
        e_legend(show = FALSE)
      
    } else {
      new_dat <- dat %>%
        filter(county_full_name == county()) %>%
        select(tract_name, contains(starts)) %>%
        rename(series = as.integer(show())) %>%
        arrange(tract_name)
      
      value_dat <- dat %>%
        filter(county_full_name == county()) %>%
        select(contains(starts)) %>%
        select(ends_with("score")) %>%
        pull(1) %>%
        mean(na.rm = T)
      
      status <- ifelse(value_dat <= 0, "#28a745", "#ffc107")
      
      style <- ifelse(as.integer(show()) == 3, "percent", "decimal")
      digits <- ifelse(as.integer(show()) == 4, 2, 0)
      
      new_dat %>%
        e_charts(x = tract_name) %>%
        e_bar(serie = series, name = subtitle) %>%
        e_x_axis(show = FALSE) %>%
        e_y_axis(formatter = e_axis_formatter(style = style, digits = digits)) %>%
        e_color(color = status) %>%
        e_tooltip(trigger = "item", formatter = e_tooltip_item_formatter(style = style, digits = digits)) %>%
        e_legend(show = FALSE)
    }
    
  })
  
}