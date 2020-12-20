
layer_one <- read_csv("data/layer-one-composite.csv")
layer_two <- read_csv("data/layer-two-composite.csv")
layer_three <- read_csv("data/layer-three-composite.csv")
counties <- read_csv("data/nc-counties.csv")
nc_tracts <- read_rds("data/tracts.rds")
composite_dat <- layer_three %>%
  left_join(layer_two, by = c("county_geoid" = "geoid")) %>%
  select(-c(name, namelsad)) %>%
  left_join(layer_one, by = c("county_geoid" = "geoid")) %>%
  select(-c(name, namelsad)) %>%
  mutate(tract_geoid = as.character(tract_geoid)) %>%
  left_join(nc_tracts, by = c("tract_geoid" = "geoid")) %>%
  st_as_sf(crs = st_crs(nc_tracts))
rm(nc_tracts)