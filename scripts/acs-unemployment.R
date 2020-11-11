library(tidyverse)
library(tidycensus)

source(here("api_key/census_api_key.R"))

nc_unem <- get_acs(geography = "tract", variables = "B23025_005",
                   state = 37, geometry = T, summary_var = "B01003_001",
                   key = api_key)
