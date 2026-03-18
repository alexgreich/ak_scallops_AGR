
# load libraries and set global options ----

## packages
library(tidyverse)
library(scales)
library(magrittr)
#library(FNGr)
library(patchwork)
library(ggpmisc)
library(lme4)

## sourced scripts
### general observer data functions
source("./code/misc/general_observer_data_functions.R")
### functions for mapping
source("./code/misc/adfg_map_functions.R")


# graphic options
theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition
) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}
theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1980:2100), yr, 5)

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))
### cutsom axis ticks for yrs (from FNGr)

# data ----

## metadata
### ghl
ghl <- read_csv("./data/observer/metadata/ghl_revised_timeseries.csv")

## observer/logbook data
### scallop haul data 2009/10 - Present
catch_wiki <- do.call(bind_rows,
                      lapply(paste0("data/observer/catch/", list.files("data/observer/catch/")), read_csv))
## shell heights 2009/10 - Present
shell_height_wiki <- do.call(bind_rows,
                             lapply(paste0("data/observer/shell_height/",
                                           list.files("data/observer/shell_height/")), read_csv))
### bycatch by day 2009/10 - Present
bycatch_wiki <- do.call(bind_rows,
                        lapply(paste0("data/observer/bycatch/", list.files("data/observer/bycatch/", pattern = ".csv")), read_csv))
### crab bycath size data 2009/10 - Present
crab_size_wiki <- do.call(bind_rows,
                          lapply(paste0("data/observer/crab_size/", list.files("data/observer/crab_size/")), read_csv))

### shell height meat weight data
meat_wiki <- do.call(bind_rows,
                     lapply(paste0("data/observer/meat_weight/", list.files("data/observer/meat_weight/")), read_csv))

# data mgmt ----

## clean catch data
catch <- f_clean_catch(catch_wiki)
## clean bycatch data
bycatch <- f_clean_bycatch(bycatch_wiki, catch)
## clean crab size data
crab_size <- f_clean_crab_size(crab_size_wiki)
## clean shell height data
shell_height <- f_clean_sh(shell_height_wiki, catch)
## clean meat weight data
meat <- f_clean_meat(meat_wiki, catch)



# estimate shmw parameters ----

meat %>%
  filter(!is.na(meat_weight)) %>%
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district)) %>%
  group_by(season, district) %>%
  nest() %>%
  
  mutate(pars = purrr::map(data, function(data){
    
    data %>%
      filter(!is.na(shell_height), 
             !is.na(meat_weight)) %>%
      lm(log(meat_weight) ~ log(shell_height), data = .) -> fit
    
    tibble(df = fit$df.residual,
           ln_a = as.numeric(coef(fit))[1],
           ln_a_se = as.numeric(sqrt(diag(vcov(fit))))[1],
           b = as.numeric(coef(fit))[2],
           b_se = as.numeric(sqrt(diag(vcov(fit))))[2]) -> pars
    
    return(pars)
    
  })) %>%
  unnest(pars) %>%
  dplyr::select(-data) -> pars

write_csv(pars, "./output/observer/2022/shmw_pars.csv")

meat %>%
  filter(!is.na(meat_weight),
         district != "KSE") %>%
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district),
         district = case_when(district == "YAK" ~ "Yakutat",
                              district == "KNE" ~ "K. Northeast",
                              district == "KSH" ~ "K. Shelikof",
                              district == "KSW" ~ "K. Southwest",
                              district == "M" ~ "AK Peninsula",
                              district == "O" ~ "Dutch Harbor",
                              district == "Q" ~ "Bering Sea",
                              district == "WKI" ~ "West Kayak Island")) %>%
  ggplot()+
  geom_point(aes(x = log(shell_height), y = log(meat_weight), color = season), alpha = 0.2)+
  geom_smooth(aes(x = log(shell_height), y = log(meat_weight), color = season), method = "lm", se = F)+
  facet_wrap(~district, scales = "free")+
  labs(x = "ln Shell Height (mm)", y = "ln Meat Weight (g)", color = NULL) -> x
ggsave("./figures/ghl_supplement/2022/shmw_est_all_districts.png", plot = x, height = 6, width = 6, units = "in")



# estimation method 1----

## compute estimated meat weight per haul based on sh-mw relationship and retained size comp
## expand to each haul using average meat weight per basket
## sum across hauls
  
# compute retained meat weight
shell_height %>%
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district)) %>%
  filter(!is.na(sh),
         rtnd_disc == "R") %>%
  group_by(season, district, haul_id) %>%
  mutate(nmeas = n()) %>%
  group_by(season, district, haul_id, sh, nmeas) %>%
  summarise(count = n()) %>%
  # join to total scallops caught that haul
  left_join(catch %>%
              dplyr::select(haul_id, scallop_count)) %>%
  # compute sample factor
  mutate(sample_factor = (count / nmeas) * scallop_count) %>%
  # join to shmw parameters
  right_join(pars) %>%
  # estimate mw
  mutate(mw = sample_factor * exp(ln_a) * sh^b) %>%
  # estimate mw per haul in lb
  group_by(haul_id) %>%
  summarise(w_ik = sum(mw, na.rm = T) * 0.00220462) %>%
  # join back to catch data
  right_join(catch %>%
               mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district)) %>%
               filter(f_season_yr(season) >= 2016)) %>%
  mutate(mw_per_basket = w_ik / rtnd_basket) %>%
  
  group_by(season, district, set_date) %>%
  mutate(u_k = mean(mw_per_basket, na.rm = T),
         var_u_k = var(mw_per_basket, na.rm = T) / n()) %>% ungroup %>%

  mutate(rtnd_mw = u_k * rtnd_basket,
         rtnd_mw_var = var_u_k * rtnd_basket^2) %>%
  group_by(season, district) %>%
  # sum across hauls
  summarise(mw_tau = sum(rtnd_mw, na.rm = T),
            mw_tau_var = sum(rtnd_mw_var, na.rm = T),
            mw_l = sum(meat_weight, na.rm = T)) %>%
  mutate(mw_rnl = mw_tau - mw_l,
         se_rnl = (sqrt(mw_tau_var) / mw_tau) * mw_rnl,
         expand_by = "Daily Basket Totals (1)") -> method1

  
# estimation method 2 ----

# compute retained meat weight
shell_height %>%
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district)) %>%
  filter(!is.na(sh),
         rtnd_disc == "R") %>%
  group_by(season, district, haul_id) %>%
  mutate(nmeas = n()) %>%
  group_by(season, district, haul_id, sh, nmeas) %>%
  summarise(count = n()) %>%
  # join to total scallops caught that haul
  left_join(catch %>%
              dplyr::select(haul_id, scallop_count)) %>%
  # compute sample factor
  mutate(sample_factor = (count / nmeas) * scallop_count) %>%
  # join to shmw parameters
  right_join(pars) %>%
  # estimate mw
  mutate(mw = sample_factor * exp(ln_a) * sh^b) %>%
  # estimate mw per haul in lb
  group_by(haul_id) %>%
  summarise(w_ik = sum(mw, na.rm = T) * 0.00220462) %>%
  # join back to catch data
  right_join(catch %>%
               mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district)) %>%
               filter(f_season_yr(season) >= 2016)) %>%
  mutate(mw_per_dh = w_ik / dredge_hrs) %>%
  
  group_by(season, district) %>%
  summarise(u_k = mean(mw_per_dh, na.rm = T),
            var_u_k = var(mw_per_dh, na.rm = T) / n(),
            effort = sum(dredge_hrs, na.rm = T),
            mw_l = sum(meat_weight, na.rm = T)) %>%
  mutate(mw_tau = u_k * effort,
         mw_tau_var = var_u_k * effort^2) %>%
  mutate(mw_rnl = mw_tau - mw_l,
         se_rnl = (sqrt(mw_tau_var) / mw_tau) * mw_rnl,
         expand_by = "Tot. Dredge Hrs (2)") -> method2
  
# plots ----  

bind_rows(method1, method2) %>%
  # remove districts with data issues
  # dutch harbor doesn't have a basket weight in one season, sparse shmw data
  filter(!(district %in%  c("KSE", "O"))) %>%
  mutate(district = case_when(district == "YAK" ~ "Yakutat",
                              district == "KNE" ~ "K. Northeast",
                              district == "KSH" ~ "K. Shelikof",
                              district == "KSW" ~ "K. Southwest",
                              district == "M" ~ "AK Peninsula",
                              district == "O" ~ "Dutch Harbor",
                              district == "Q" ~ "Bering Sea",
                              district == "WKI" ~ "West Kayak Island")) %>%
  # join to full time series of years to make plot more appealing
  right_join(expand_grid(season = unique(.$season), 
                         district = unique(.$district),
                         expand_by = unique(.$expand_by))) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  
  ggplot()+
  geom_point(aes(x = year, y = mw_rnl, color = expand_by), alpha = 0.5)+
  geom_line(aes(x = year, y = mw_rnl, , color = expand_by, group = expand_by), alpha = 0.5)+
  geom_errorbar(aes(x = year, ymin = mw_rnl - 2*se_rnl, ymax =  mw_rnl + 2*se_rnl, color = expand_by),
                width = 0, alpha = 0.5)+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL,  y = "Retained-Not-Landed Meats (lb)", color = NULL)+
  facet_wrap(~district, scales = "free", ncol = 2) -> x
ggsave("./figures/ghl_supplement/2022/mw_rnl.png", plot = x, height = 6, width = 6, units = "in")




# compare with weak meat trend ----

meat %>%
  transmute(season, haul_id,
            wk =  case_when(!is.na(meat_cond) ~ meat_cond,
                            !is.na(meat_condition) ~ meat_condition)) %>%
  filter(!is.na(wk)) %>%
  left_join(catch %>% dplyr::select(haul_id, district)) %>%
  mutate(district = ifelse(district %in% c("C", "WC", "UB"), "M", district)) %>%
  group_by(season, district) %>%
  summarise(prop_wk = sum(wk == 1) / n()) -> wk

# plot to compare to wk
bind_rows(method1, method2) %>%
  left_join(wk) %>%
  # remove districts with data issues
  # dutch harbor doesn't have a basket weight in one season, sparse shmw data
  filter(!(district %in%  c("KSE", "O"))) %>%
  mutate(district = case_when(district == "YAK" ~ "Yakutat",
                              district == "KNE" ~ "K. Northeast",
                              district == "KSH" ~ "K. Shelikof",
                              district == "KSW" ~ "K. Southwest",
                              district == "M" ~ "AK Peninsula",
                              district == "O" ~ "Dutch Harbor",
                              district == "Q" ~ "Bering Sea",
                              district == "WKI" ~ "West Kayak Island")) %>%
  # join to full time series of years to make plot more appealing
  right_join(expand_grid(season = unique(.$season), 
                         district = unique(.$district),
                         expand_by = unique(.$expand_by))) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(season, district) %>%
  mutate(mean_mw_tau = max(mw_tau)) %>%
  ggplot()+
  geom_point(aes(x = year, y = mw_rnl, color = expand_by), alpha = 0.5)+
  geom_line(aes(x = year, y = mw_rnl, , color = expand_by, group = expand_by), alpha = 0.5)+
  
  geom_point(aes(x = year, y = prop_wk * 40e3))+
  geom_line(aes(x = year, y = prop_wk * 40e3, group = 1))+
  # add weak meat trend
  scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~ (. /  40e3), name = "Proportion Weak Meat"))+
  labs(x = NULL,  y = "Retained-Not-Landed Meats (lb)", color = NULL)+
  facet_wrap(~district, scales = "free", ncol = 2) -> x
ggsave("./figures/ghl_supplement/2022/mw_rnl_wk_meat.png", plot = x, height = 6, width = 6, units = "in")
