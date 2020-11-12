library(tidyverse)
library(here)

county_crf <- read_csv(here("composite/county_crf.csv"), col_types = cols("geoid" = col_character()))
hospital_crf <- read_csv(here("composite/hospital_crf.csv"), col_types = cols("geoid" = col_character()))
hhs_uninsured <- read_csv(here("composite/hhs_uninsured_relief.csv"), col_types = cols("geoid" = col_character()))
hhs_rhc <- read_csv(here("composite/rhc_testing.csv"), col_types = cols("geoid" = col_character()))
hhs_prov <- read_csv(here("composite/hhs_provider_relief.csv"), col_types = cols("geoid" = col_character()))
hhs_awards <- read_csv(here("composite/hhs_awards.csv"), col_types = cols("geoid" = col_character()))

layer_one <- county_crf %>%
  left_join(select(hospital_crf, 3:6)) %>%
  left_join(select(hhs_uninsured, 3:6)) %>%
  left_join(select(hhs_rhc, 3:6)) %>%
  left_join(select(hhs_prov, 3:6)) %>%
  left_join(select(hhs_awards, 3:6)) %>%
  mutate(across(.fns = ~replace_na(.x, 0))) %>%
  select(name, namelsad, geoid, ends_with("total"), ends_with("capita"), ends_with("score"))

write_csv(layer_one, file = here("composite/layer-one-composite.csv"))

layer_one %>%
  select(name, namelsad, geoid, ends_with("score")) %>%
  pivot_longer(cols = ends_with("score"), names_to = "layer_metric", values_to = "score") %>%
  group_by(name, geoid) %>%
  summarise(total = sum(score)) %>%
  arrange(desc(total))
