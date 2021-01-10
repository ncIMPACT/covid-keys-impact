
layer_one <- read_csv("data/layer-one-composite.csv")

layer_two <- read_csv("data/layer-two-composite.csv")

layer_three <- read_csv("data/layer-three-composite.csv")

counties <- read_csv("data/nc-counties.csv")

nc_tracts <- read_rds("data/tracts.rds")

nc_counties <- read_rds("data/nc_counties.rds")

composite_dat <- layer_three %>%
  left_join(layer_two, by = c("county_geoid" = "geoid")) %>%
  select(-c(name, namelsad)) %>%
  left_join(layer_one, by = c("county_geoid" = "geoid")) %>%
  select(-c(name, namelsad)) %>%
  mutate(tract_geoid = as.character(tract_geoid)) %>%
  left_join(nc_tracts, by = c("tract_geoid" = "geoid")) %>%
  st_as_sf(crs = st_crs(nc_tracts))

rm(nc_tracts)

composite_build_selections <- c("ACS Unemployment" = "acs_unemp",
                                "ACS Poverty" = "acs_pov",
                                "ACS School Age Children" = "acs_school",
                                "ACS Broadband Access" = "acs_broadband",
                                "ACS Health Insurance" = "acs_health",
                                "ACS White Alone" = "white_alone",
                                "Dependency Ratio" = "depend",
                                "Zillow Home Value Percent Change" = "zhvi",
                                "Unemployment Rate Change" = "unemp",
                                "Taxable Sales Percent Change" = "tax_sales",
                                "Sales Tax Distribution Percent Change" = "tax_distribution",
                                "COVID-19 Cases Per 10,000 Residents" = "cases_per",
                                "UI Claims Per 1,000 Residents" = "ui_claims",
                                "NC CRF County Distributions" = "county_crf",
                                "NC CRF Hospital Distributions" = "hospital_crf",
                                "HHS Uninsured Relief Fund" = "hhs_uninsured",
                                "HHS Rural Health Clinic Testing Fund" = "rhc_testing",
                                "HHS Provider Relief Fund" = "prov_relief",
                                "HHS COVID-19 Awards" = "hhs_awards",
                                "Paycheck Protection Program" = "ppp")

dashboard_composite_selections <- list("Community Resilience" = 
                                      c("ACS Unemployment" = "acs_unemp_rate",
                                        "ACS Poverty" = "acs_pov_rate",
                                        "ACS School Age Children" = "school_age",
                                        "ACS Broadband Access" = "broadband_rate",
                                        "ACS Health Insurance" = "health_ins_pct",
                                        "ACS White Alone" = "white_alone_pct",
                                        "Dependency Ratio" = "depend_ratio"),
                                    "Real-Time Fluctuations" = 
                                      c("Zillow Home Value Percent Change" = "zhvi_pct_change",
                                        "Unemployment Rate Change" = "unemp_change",
                                        "Taxable Sales Percent Change" = "tax_sales_change",
                                        "Sales Tax Distribution Percent Change" = "tax_distribution_change",
                                        "COVID-19 Cases Per 10,000 Residents" = "cases_per",
                                        "UI Claims Per 1,000 Residents" = "ui_claims_per"),
                                    "Emergency Financial Support" = 
                                      c("NC CRF County Distributions" = "county_crf_per_capita",
                                        "NC CRF Hospital Distributions" = "hospital_crf_per_capita",
                                        "HHS Uninsured Relief Fund" = "hhs_uninsured_per_capita",
                                        "HHS Rural Health Clinic Testing Fund" = "rhc_testing_per_capita",
                                        "HHS Provider Relief Fund" = "prov_relief_per_capita",
                                        "HHS COVID-19 Awards" = "hhs_awards_per_capita",
                                        "Paycheck Protection Program" = "ppp_per_capita"))