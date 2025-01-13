# notes ----

## prepare retained catch and discards

## tyler jackson
## 1/10/2025

# load ----

source("./code/observer/scalobservR.R")

# data ----

## catch data 
catch <- load_catch_by_haul("data/observer/catch")
old_catch <- read_csv("./data/observer/old_catch/catch_summary_1992-2008.csv") %>%
  mutate(district = gsub("D16", "YAK", district),
         round_weight = meat_weight / 0.1) %>%
  filter(!is.na(meat_weight)) 

## bycatch data
bycatch <- load_bycatch_by_haul(dir = "data/observer/bycatch", catch)

# retained catch  ----

get_retained_summary(data = bind_rows(old_catch, catch), by = "district", units = "t") %>% 
  # format for gmacs + district
  transmute(district, 
            year = scal_year,
            seas = 1, fleet = 1, sex = 0,
            obs = ret_rw, cv = 0.03,
            type = 1, units = 1, mult = 1, effort = 0, discard_mortality = 0.2) -> retained

# discards ----

get_discards(data = bycatch, by = "district", units = "t") %>% 
  # format for gmacs + district
  transmute(district, 
            year = scal_year,
            seas = 1, fleet = 1, sex = 0,
            obs = discards_rw, cv = discards_rw_cv,
            type = 2, units = 1, mult = 1, effort = 0, discard_mortality = 0.2) -> discards

