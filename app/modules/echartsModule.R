library(echarts4r)

e_common(font_family = "Source Sans Pro")

# counties object should be passed as a distinct character vector
echartUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    bs4ValueBoxOutput(ns("valueBox"), width = 12)
  )
  
}

echartServer <- function(input, output, session, dat, starts, county, subtitle = NULL) {
  
  output$valueBox <- renderbs4ValueBox({
    new_dat <- dat %>%
      filter(county_full_name == county()) %>%
      select(tract_name, starts_with("white_alone")) %>%
      select(tract_name, ends_with("score")) %>%
      rename(series = 2) %>%
      arrange(-series)
    
    chart <- new_dat %>%
      e_charts(tract_name, width = "100%", height = "200px") %>%
      e_bar(serie = series) %>%
      e_tooltip(trigger = "axis") %>%
      e_x_axis(show = FALSE) %>%
      e_y_axis(show = FALSE) %>%
      e_legend(show = FALSE) %>%
      e_color(color = "#0077b5")
    
    value_dat <- dat %>%
      filter(county_full_name == county()) %>%
      select(starts_with(starts)) %>%
      select(ends_with("score")) %>%
      pull(1) %>%
      mean(na.rm = T) %>%
      round(digits = 2)
    
    status <- ifelse(value_dat <= 0, "success", "warning")
    main_icon <- ifelse(value_dat <= 0, "arrow-down", "arrow-up")
    
    bs4ValueBox(
      value = chart,
      subtitle = subtitle,
      status = status,
      icon = main_icon
    )
    
  })
  
}