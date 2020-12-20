
valueBoxUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    bs4ValueBoxOutput(ns("valueBox"), width = 12)
  )
  
}

valueBoxServer <- function(input, output, session, dat, starts, county, subtitle = NULL) {
  
  output$valueBox <- renderbs4ValueBox({
    value_dat <- dat %>%
      filter(namelsad == county()) %>%
      select(starts_with(starts)) %>%
      select(ends_with("score")) %>%
      pull(1) %>%
      mean(na.rm = T) %>%
      round(digits = 2)
    
    per_capita <- dat %>%
      filter(namelsad == county()) %>%
      select(starts_with(starts)) %>%
      select(ends_with("per_capita")) %>%
      pull(1) %>%
      dollar()
    
    status <- ifelse(value_dat <= 0, "success", "warning")
    main_icon <- ifelse(value_dat <= 0, "arrow-down", "arrow-up")
    
    footer = glue()
    
    bs4ValueBox(
      value = h1(value_dat),
      subtitle = subtitle,
      color = status,
      icon = icon(main_icon),
      footer = h4("Per Capita: ", per_capita)
    )
    
  })
  
}