# Load packages
library(shiny)
library(bs4Dash)
library(fresh)
library(tidyverse)
library(scales)
library(glue)
library(echarts4r)

# Load modules
source("modules/get-data.R")
source("modules/valueBoxModule.R")
source("modules/layerTwoValueBoxModule.R")
source("modules/echartsModule.R")

# Create recyclable icon function
info_title <- function(link = NULL, title = NULL) {
  info_icon <- icon("info-circle", class = "h4 mb-0")
  title <- title
  glue("<p class='my-0'>{title}<a class='ml-2 my-0 align-middle' style='color:#343a40;' target='_blank' href='{link}'>{info_icon}</a></p>")
}


ui <- bs4DashPage(
  navbar = bs4DashNavbar(compact = TRUE,
                         selectInput(inputId = "county", selectize = FALSE,
                                     choices = counties$NAMELSAD, label = NULL)),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarHeader(title = "Composite Layers"),
      bs4SidebarMenuItem(text = "Community Resiliency",
                         tabName = "prior",
                         icon = "users"),
      bs4SidebarMenuItem(text = "Financial Support",
                         tabName = "fin-dash",
                         icon = "hand-holding-usd"),
      bs4SidebarMenuItem(text = "Real-Time Metrics",
                         tabName = "real-time",
                         icon = "calendar-alt")
    )
  ),
  body = bs4DashBody(
    tags$style(HTML("body > div > nav > div{display: contents;}")),
    tags$script('window.onresize = function() {
            $(".echarts4r").each(function(){
                var id = $(this).attr("_echarts_instance_");
                window.echarts.getInstanceById(id).resize();
            });
        };'),
    bs4TabItems(
      bs4TabItem(
        tabName = "prior",
        fluidRow(
          bs4Sortable(width = 4,
                      bs4Card(echartUI("chart1"), width = 12, closable = FALSE, title = HTML(info_title(title = "ACS Unemployment Rate",
                                                                                                        link = "https://ncimpact.sog.unc.edu/"))))
        )
      ),
      bs4TabItem(
        tabName = "fin-dash",
        fluidRow(
          bs4Sortable(width = 4,
                      bs4Card(valueBoxUI("vbox1"), width = 12, closable = FALSE, title = HTML(info_title(title = "Uninsured Relief Fund",
                                                                                                         link = 'https://ncimpact.sog.unc.edu/'))),
                      bs4Card(valueBoxUI("vbox2"), width = 12, closable = FALSE, title = "Paycheck Protection Program (PPP)")),
          bs4Sortable(width = 4,
                      bs4Card(valueBoxUI("vbox4"), width = 12, closable = FALSE, title = "NC CRF Hospital Distributions"),
                      bs4Card(valueBoxUI("vbox5"), width = 12, closable = FALSE, title = "Rural Health Clinic Testing Fund"),
                      bs4Card(valueBoxUI("vbox3"), width = 12, closable = FALSE, title = "NC CRF County Distributions")),
          bs4Sortable(width = 4,
                      bs4Card(valueBoxUI("vbox6"), width = 12, closable = FALSE, title = "Provider Relief Fund"),
                      bs4Card(valueBoxUI("vbox7"), width = 12, closable = FALSE, title = "HHS COVID-19 Awards"))
        )
      ),
      bs4TabItem(
        tabName = "real-time",
        fluidRow(
          bs4Sortable(width = 4,
                      bs4Card(valueBoxTwoUI("vbox8"), width = 12, closable = FALSE, title = "Zillow Home Value Percent Change"),
                      bs4Card(valueBoxTwoUI("vbox9"), width = 12, closable = FALSE, title = "Unemployment Rate Percent Change")),
          bs4Sortable(width = 4,
                      bs4Card(valueBoxTwoUI("vbox10"), width = 12, closable = FALSE, title = "Sales Tax Distribution Percent Change"),
                      bs4Card(valueBoxTwoUI("vbox11"), width = 12, closable = FALSE, title = "COVID-19 Cases Per 10,000 Residents")),
          bs4Sortable(width = 4,
                      bs4Card(valueBoxTwoUI("vbox12"), width = 12, closable = FALSE, title = "Unemployment Insurance Claims Per 1,000 Residents"),
                      bs4Card(valueBoxTwoUI("vbox13"), width = 12, closable = FALSE, title = "Taxable Sales Percent Change"))
        )
      )
    )
  ),
  footer = bs4DashFooter(),
  title = "COVID Composite"
)

server <- function(input, output, session) {
  
  input_county <- reactive({ input$county })
  
  ########## LAYER ONE VALUE BOX MODULE CALLS #################################
  callModule(valueBoxServer, "vbox1", dat = layer_one, starts = "hhs_uninsured",
             subtitle = "Uninsured Relief Fund Z-Score", county = input_county)
  
  callModule(valueBoxServer, "vbox2", dat = layer_one, starts = "ppp",
             subtitle = "Paycheck Protection Program Z-Score", county = input_county)
  
  callModule(valueBoxServer, "vbox3", dat = layer_one, starts = "county_crf",
             subtitle = "NC CRF County Distributions Z-Score", county = input_county)
  
  callModule(valueBoxServer, "vbox4", dat = layer_one, starts = "hospital_crf",
             subtitle = "NC CRF Hospital Distributions Z-Score", county = input_county)
  
  callModule(valueBoxServer, "vbox5", dat = layer_one, starts = "rhc_testing",
             subtitle = "Rural Health Clinic Testing Fund Z-Score", county = input_county)
  
  callModule(valueBoxServer, "vbox6", dat = layer_one, starts = "prov_relief",
             subtitle = "Provider Relief Fund Z-Score", county = input_county)
  
  callModule(valueBoxServer, "vbox7", dat = layer_one, starts = "hhs_awards",
             subtitle = "HHS COVID-19 Awards Z-Score", county = input_county)
  
  ###################### LAYER TWO VALUE BOX MODULE CALLS ######################
  callModule(valueBoxTwoServer, "vbox8", dat = layer_two, starts = "zhvi",
             subtitle = "Zillow Home Value Z-Score", county = input_county, footer = "% Change")
  
  callModule(valueBoxTwoServer, "vbox9", dat = layer_two, starts = "unemp",
             subtitle = "Unemployment Rate Change Z-Score", county = input_county, footer = "% Change")
  
  callModule(valueBoxTwoServer, "vbox10", dat = layer_two, starts = "tax_distribution",
             subtitle = "Sales Tax Distribution Change Z-Score", county = input_county, footer = "% Change")
  
  callModule(valueBoxTwoServer, "vbox11", dat = layer_two, starts = "cases",
             subtitle = "COVID-19 Cases Per 10,000 Z-Score", county = input_county, footer = "Cases Per 10,000 Residents")
  
  callModule(valueBoxTwoServer, "vbox12", dat = layer_two, starts = "ui_claims",
             subtitle = "Unemployment Insurance Claims Z-Score", county = input_county, footer = "Claims Per 1,000 Residents")
  
  callModule(valueBoxTwoServer, "vbox13", dat = layer_two, starts = "tax_sales",
             subtitle = "Taxable Sales Change Z-Score", county = input_county, footer = "% Change")
  
  ######################## LAYER THREE VALUE BOX MODULE CALLS ##################
  callModule(echartServer, "chart1", dat = layer_three, county = input_county,
             starts = "acs_unemp", subtitle = "ACS Unemployment Z-Score")
  
}

shinyApp(ui, server)