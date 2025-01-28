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

## shell height data
shell_height <- load_sh_data(dir = "data/observer/shell_height", catch)


# retained catch  ----

get_retained_summary(data = bind_rows(old_catch, catch %>% filter(scal_year >= 2009)), by = "district", units = "t") %>% 
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


# retained sh ----

# retained shell height composition 
get_sh_composition(shell_height %>% filter(sh >= 30), type = "retained", catch = catch, by = "district") %>%
  # add sh bin
  mutate(bin = floor(sh / 10) * 10,
         bin = ifelse(sh >= 160, 160, bin)) %>%
  group_by(scal_year, district, bin, n_meas) %>%
  summarise(p = sum(p)) %>% ungroup %>%
  right_join(expand_grid(bin = seq(30, 160, 10),
                         distinct(., scal_year, district, n_meas))) %>%
  replace_na(list(p = 0)) %>%
  mutate(bin = factor(bin, levels = seq(30, 160, 10))) %>%
  arrange(bin) %>%
  transmute(district,
            year = scal_year, seas = 1, fleet = 1, sex = 0, type = 1, shell = 0, maturity = 0, 
            nsamp = n_meas,
            size = bin, 
            obs =  sprintf("%.4f", p)) -> retained_sh
  



# discard sh ----

# discard shell height composition 
get_sh_composition(shell_height %>% filter(sh >= 30), type = "discard", bycatch = bycatch, by = "district") %>%
  # add sh bin
  mutate(bin = floor(sh / 10) * 10,
         bin = ifelse(sh >= 160, 160, bin)) %>%
  group_by(scal_year, district, bin, n_meas) %>%
  summarise(p = sum(p)) %>% ungroup %>%
  right_join(expand_grid(bin = seq(30, 160, 10),
                         distinct(., scal_year, district, n_meas))) %>%
  replace_na(list(p = 0)) %>%
  mutate(bin = factor(bin, levels = seq(30, 160, 10))) %>%
  arrange(bin) %>%
  transmute(district,
            year = scal_year, seas = 1, fleet = 1, type = 2, sex = 0, shell = 0, maturity = 0, 
            nsamp = n_meas,
            size = bin, 
            obs =  sprintf("%.4f", p)) -> discard_sh

