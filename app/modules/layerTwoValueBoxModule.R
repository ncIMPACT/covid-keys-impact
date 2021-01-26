
fix_dat <- function(dat, ends) {
  if(ends == "change" & dat < 1) {
    percent(dat, accuracy = 0.01)
  } else if (ends == "change" & dat >= 1) {
    percent(dat, scale = 1)
  } else {
    number(dat, accuracy = 1)
  }
}

valueBoxTwoUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    bs4ValueBoxOutput(ns("valueBox"), width = 12)
  )
  
}

valueBoxTwoServer <- function(input, output, session, dat, starts, county, subtitle = NULL, footer = NULL) {
  
  output$valueBox <- renderbs4ValueBox({
    value_dat <- dat %>%
      filter(namelsad == county()) %>%
      select(starts_with(starts)) %>%
      select(ends_with("score")) %>%
      pull(1) %>%
      mean(na.rm = T) %>%
      round(digits = 2)
    
    ends <- ifelse(starts %in% c("zhvi", "unemp", "tax_sales", "tax_distribution"), "change", "per")
    
    total_value_dat <- dat %>%
      filter(namelsad == county()) %>%
      select(starts_with(starts)) %>%
      select(ends_with(ends)) %>%
      pull(1) %>%
      fix_dat(ends = ends)
    
    status <- ifelse(value_dat <= 0, "success", "purple")
    main_icon <- ifelse(value_dat <= 0, "arrow-down", "arrow-up")
    
    bs4ValueBox(
      value = h1(total_value_dat),
      subtitle = footer,
      color = status,
      icon = icon(main_icon),
      footer = h4("Z-Score: ", value_dat)
    )
    
  })
  
}