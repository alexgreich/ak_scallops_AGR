# notes ----

## 2023 observer data summary analysis
## organized by statistic
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2023/5/25

# load libraries and set global options ----

## packages
library(tidyverse)
library(scales)
library(magrittr)
library(patchwork)
library(ggpmisc)
library(lme4)
library(sf)

## sourced scripts
### general observer data functions
source("./code/observer/scallop_obs_functions.R")

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))
### cutsom axis ticks for yrs (from FNGr)

ak <- raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")
can <- raster::getData("GADM", country = c("CAN"), level = 1, path = "./data/maps")
st_as_sf(ak) %>%
  filter(NAME_1 == "Alaska") -> ak
st_as_sf(can) -> can
bind_rows(ak, can) -> land

# data ----

## metadata
### ghl
ghl <- read_csv("./data/observer/metadata/ScallopGHLs_ByFisheryAndDistrict.csv")
cbl <- read_csv("./data/observer/metadata/ScalFisheryCrabBycatchLimitsByDistrict_2009-2023.csv")

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
                   lapply(list.files("data/observer/bycatch", pattern = ".csv", full.names = T), read_csv))
### crab bycath size data 2009/10 - Present  
crab_size_wiki <- do.call(bind_rows,
                     lapply(paste0("data/observer/crab_size/", list.files("data/observer/crab_size/")), read_csv))

### shell height meat weight data
meat_wiki <- do.call(bind_rows,
                     lapply(paste0("data/observer/meat_weight/", list.files("data/observer/meat_weight/")), read_csv))

### haul composition data
haulcomp_wiki <- do.call(bind_rows,
                     lapply(paste0("data/observer/catch_comp/", list.files("data/observer/catch_comp/")), read_csv))


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
meat <- f_clean_meat(meat_wiki)

# fishery month ----

# determine average month of the fishery
catch %>%
  group_by(season, district) %>%
  summarise(month = month(mean(set_date, na.rm = T))) %>%
  write_csv("./output/observer/2024/fishery_avg_month.csv")

# fishery catch tables ----

## fishery performance tables by district (f_fish_stats from general functions)
### KNE
f_fish_stats(catch, c("KNE"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_KNE.csv")
### KSH
f_fish_stats(catch, c("KSH"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_KSH.csv")
### KSW
f_fish_stats(catch, c("KSW"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_KSW.csv")
### Area M
f_fish_stats(catch, c("UB", "WC", "C"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_M.csv")
### Area O
f_fish_stats(catch, c("O"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_O.csv")
### Area Q
f_fish_stats(catch, c("Q"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_Q.csv")
### WKI
f_fish_stats(catch, c("WKI"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_WKI.csv")
### EKI
f_fish_stats(catch, c("EKI"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_EKI.csv")
### YAK
f_fish_stats(catch, c("YAK", "D", "D16"), add_ghl = T, 
             path = "./output/observer/2024/fish_stats_YAK.csv")

# trends in round_weight and number of scallops
catch %>%
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  group_by(district) %>%
  nest() %>% 
  mutate(plot = purrr::map2_chr(data, district, function(data, district){
    
    # compute season totals
    data %>%
      group_by(season) %>%
      summarise(round_weight = sum(round_weight, na.rm = T),
                scallops = sum(scallop_count, na.rm = T)) %>%
      right_join(catch %>%
                  distinct(season)) %>%
      # scale scallops number for second axis
      mutate(scallops = scallops * 0.4) %>%
      pivot_longer(2:3) %>% 
      # plot
      ggplot()+
      geom_point(aes(x = substring(season, 1, 4), y = value, shape = name))+
      geom_line(aes(x = substring(season, 1, 4), y = value, linetype = name, group = name))+
      scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~. / 0.4, labels = scales::comma, name = "Retained Scallops\n"))+
      labs(y = "Retained lb (round)", shape = NULL, x = NULL, linetype = NULL)+
      scale_shape_manual(values = c(16, 1), labels = c("Round lb", "Scallops"))+
      scale_linetype_manual(values = 1:2, labels = c("Round lb", "Scallops"))+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> p1
    
    ggsave(paste0("./figures/observer/2024/retained_catch_", district, ".png"), 
           plot = p1, height = 3, width = 7, units = "in")
  }))


# cpue standardization ----

catch %>%
  # filter for only districts that have surveys
  filter(district %in% c("KSH", "KNE", "YAK", "WKI", "KSW")) %>%
  # nest by district
  group_by(district) %>% 
  nest %>% ungroup %>% #pull(data) %>% .[[3]] -> data
  mutate(core_data = purrr::map(data, function(data) {
    
    data %>% 
      ## add cpue
      mutate(cpue = round_weight / dredge_hrs) %>%  
      ## combine YAK 6 beds
      mutate(bed_code = ifelse(bed_code %in% c("YAK6D", "YAK6Y"), "YAK6", bed_code)) %>%
      ## observed hauls with 13 or 15 ft dredges
      filter(dredge_width %in% c(13, 15, 26, 30), 
             gear_perf == 1,
             cpue > 0) %>% 
      ## remove hauls at the extremities for round weight and depth
      filter(cpue >= quantile(cpue, 0.025, na.rm = T),
             cpue <= quantile(cpue, 0.975, na.rm = T),
             depth >= quantile(depth, 0.025, na.rm = T),
             depth <= quantile(depth, 0.975, na.rm = T)) %>%
      ## extract month and relevel so that it aligns with the season progression
      ## make bed a factor
      ## make season a factor
      mutate(month = month.abb[lubridate::month(set_date)],
             month = factor(month, levels = month.abb[c(7:12, 1:6)]),
             adfg = factor(adfg, levels = c(58200, 40924, 32554, 303)),
             dredge_width = case_when(dredge_width %in% c(15, 30) ~ "15 ft",
                                      dredge_width %in% c(13, 26) ~ "13 ft"),
             dredge_width = factor(dredge_width, levels = c("13 ft", "15 ft")),
             bed = factor(bed_code)) %>%
      transmute(season, round_weight, dredge_hrs, cpue, dredge_width, depth, month, adfg, bed) %>%
      filter(complete.cases(.))
    
  })) %>%
  transmute(district, core_data) -> core 

fit <- list()
for (d in 1:length(core$core_data)) {
  # determine full model scope based on available data
  levels = sapply(lapply(core$core_data[[d]], unique), length)
  tibble(var = names(levels), nlevels = levels) %>%
    filter(!(var %in% c("season", "round_weight", "dredge_hrs", "cpue")),
           nlevels > 1) %>%
    mutate(var = ifelse(var == "depth", "s(depth)", var)) %>%
    pull(var) %>%
    paste("+", ., collapse = " ") %>%
    paste("~season", ., collapse = " ") %>%
    formula(., env = globalenv()) -> full_mod
  
  null = gam(cpue ~ season, family = Gamma(link = log), data = core$core_data[[d]])
  # fit model
  f_step_gam(null, full_scope = list(full_mod)) -> fit[[d]]
  
}
## add back in to object - core
core$fit <- fit
## separate models and results table
core %>%
  mutate(model = purrr::map(fit, function(x){x[[1]]}),
         mod_tab = purrr::map(fit, function(x){x[[2]]})) %>%
  dplyr::select(-fit) -> core

# save output as r data
saveRDS(core, "./output/observer/2024/observer_cpue_std.RDS")

# diagnostics 

core <- readRDS("./output/observer/2024/observer_cpue_std.RDS")

# save plots of dharma residuals
mapply(function(district, model) {
  
  # simulate dharma residuals
  simr <- simulateResiduals(fittedModel = model, plot = F)
  # make qq plot of simulatied residuals - should be uniform
  qq <- wrap_elements(panel = ~plotQQunif(simr, testUniformity = T, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
  # plot residuals against ranked model predictions
  rf <- wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.1, pch = 16), clip = F)
  # save plot
  ggsave(paste("./figures/observer/2024/", district, "_observer_cpue_std_diag.png"), plot = qq + rf, height = 5, width = 9, units = "in")
  
  return(T)
  
}, district = core$district, model = core$model)

# just do one at a time because its 

## YAK
core %>% pull(model) %>% .[[1]] %>% getViz(.) -> yak_viz
plot(sm(yak_viz, 1))+ 
  l_points(color = "grey80") + 
  l_fitLine() +
  l_ciLine() + 
  labs(x = "Depth (fa)") -> depth
plot(pterm(yak_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Bed", y = "f(Bed)") -> bed
plot(pterm(yak_viz, 3))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Vessel", y = "f(Vessel)") +  
  theme(axis.text.x = element_blank()) -> vessel
plot(pterm(yak_viz, 4))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Dredge Width", y = "f(Dredge Width)") -> dredge_width
ggsave("./figures/observer/2024/observer_cpue_std_yak_effects.png",
       plot = gridPrint(depth, bed, vessel, dredge_width, ncol = 2),
       height = 5, width = 6, units = "in")

core %>% pull(model) %>% .[[2]] %>% getViz(.) -> wki_viz

## KSH
core %>% pull(model) %>% .[[3]] %>% getViz(.) -> ksh_viz
plot(sm(ksh_viz, 1))+ 
  l_points(color = "grey80") + 
  l_fitLine() +
  l_ciLine() + 
  labs(x = "Depth (fa)") -> depth
plot(pterm(ksh_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Month", y = "f(Month)") -> month
plot(pterm(ksh_viz, 3))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Dredge Width", y = "f(Dredge Width)") -> dredge_width
ggsave("./figures/observer/2024/observer_cpue_std_ksh_effects.png",
       plot = gridPrint(depth, month, dredge_width, ncol = 2),
       height = 5, width = 6, units = "in")

## KSW
core %>% pull(model) %>% .[[4]] %>% getViz(.) -> ksw_viz
plot(pterm(ksw_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Bed", y = "f(Bed)") -> bed
plot(pterm(ksw_viz, 3))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Dredge Width", y = "f(Dredge Width)") -> dredge_width

ggsave("./figures/observer/2024/observer_cpue_std_ksw_effects.png",
       plot = gridPrint(bed, dredge_width, ncol = 1),
       height = 5, width = 6, units = "in")


## KNE
core %>% pull(model) %>% .[[5]] %>% getViz(.) -> kne_viz
plot(sm(kne_viz, 1))+ 
  l_points(color = "grey80") + 
  l_fitLine() +
  l_ciLine() + 
  labs(x = "Depth (fa)") -> depth
plot(pterm(kne_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Bed", y = "f(Bed)") -> bed
plot(pterm(kne_viz, 4))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Month", y = "f(Month)") -> month
plot(pterm(kne_viz, 3))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Dredge Width", y = "f(Dredge Width)") -> dredge_width

ggsave("./figures/observer/2024/observer_cpue_std_kne_effects.png",
       plot = gridPrint(depth, bed, month, dredge_width, ncol = 2),
       height = 5, width = 6, units = "in")

# get index

core %>% #pull(model) %>% .[[2]] -> model
  mutate(index = purrr::map2(core_data, model, function(core_data, model) {
    loc <- grep("season", names(coef(model)))
    yrs <- unique(core_data$season)
    f_getCPUE_gam(model, loc, yrs)%>%
      mutate(year = as.numeric(as.character(substring(year, 1, 4)))) %>%
      right_join(tibble(year = 2009:2023)) %>% arrange(year)
  })) %>%
  transmute(district, index) %>%
  unnest(index) -> index

# write output
write_csv(index, "./output/observer/2024/observer_cpue_std_index.csv")

## plot index
## KSH
core %>%
  transmute(district, core_data) %>% unnest(core_data) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(district, year) %>%
  summarise(cpue = mean(cpue)) %>% ungroup %>%  
  group_by(district) %>%
  mutate(cpue = cpue / (prod(cpue)^(1/n()))) %>% 
  right_join(index) %>%
  
  filter(district == "KSH") %>%
  
  ggplot()+
  geom_point(aes(x = year, y = cpue, color = "1"))+
  geom_line(aes(x = year, y = cpue, color = "1"))+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = "2"), alpha = 0.2, show.legend = F)+
  geom_point(aes(x = year, y = index, color = "2"))+
  geom_line(aes(x = year, y = index, color = "2"))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Nominal CPUE", "Std CPUE"))+
  scale_fill_manual(values = cb_palette[2])+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, color = NULL, y = "CPUE Index")+
  facet_wrap(~district, ncol = 1) -> x

ggsave("./figures/observer/2024/ksh_observer_cpue_std_index.png", plot = x, height = 3, width = 5, units = "in")

## KNE
core %>%
  transmute(district, core_data) %>% unnest(core_data) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(district, year) %>%
  summarise(cpue = mean(cpue)) %>% ungroup %>%  
  group_by(district) %>%
  mutate(cpue = cpue / (prod(cpue)^(1/n()))) %>% 
  right_join(index) %>%
  
  filter(district == "KNE") %>%
  
  ggplot()+
  geom_point(aes(x = year, y = cpue, color = "1"))+
  geom_line(aes(x = year, y = cpue, color = "1"))+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = "2"), alpha = 0.2, show.legend = F)+
  geom_point(aes(x = year, y = index, color = "2"))+
  geom_line(aes(x = year, y = index, color = "2"))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Nominal CPUE", "Std CPUE"))+
  scale_fill_manual(values = cb_palette[2])+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, color = NULL, y = "CPUE Index")+
  facet_wrap(~district, ncol = 1) -> x

ggsave("./figures/observer/2024/kne_observer_cpue_std_index.png", plot = x, height = 3, width = 5, units = "in")


## KSW
core %>%
  transmute(district, core_data) %>% unnest(core_data) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(district, year) %>%
  summarise(cpue = mean(cpue)) %>% ungroup %>%  
  group_by(district) %>%
  mutate(cpue = cpue / (prod(cpue)^(1/n()))) %>% 
  right_join(index) %>%
  
  filter(district == "KSW") %>%
  
  ggplot()+
  geom_point(aes(x = year, y = cpue, color = "1"))+
  geom_line(aes(x = year, y = cpue, color = "1"))+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = "2"), alpha = 0.2, show.legend = F)+
  geom_point(aes(x = year, y = index, color = "2"))+
  geom_line(aes(x = year, y = index, color = "2"))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Nominal CPUE", "Std CPUE"))+
  scale_fill_manual(values = cb_palette[2])+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, color = NULL, y = "CPUE Index")+
  facet_wrap(~district, ncol = 1) -> x

ggsave("./figures/observer/2024/ksw_observer_cpue_std_index.png", plot = x, height = 3, width = 5, units = "in")

## WKI
core %>%
  transmute(district, core_data) %>% unnest(core_data) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(district, year) %>%
  summarise(cpue = mean(cpue)) %>% ungroup %>%  
  group_by(district) %>%
  mutate(cpue = cpue / (prod(cpue)^(1/n()))) %>% 
  right_join(index) %>%
  
  filter(district == "WKI") %>%
  
  ggplot()+
  geom_point(aes(x = year, y = cpue, color = "1"))+
  geom_line(aes(x = year, y = cpue, color = "1"))+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = "2"), alpha = 0.2, show.legend = F)+
  geom_point(aes(x = year, y = index, color = "2"))+
  geom_line(aes(x = year, y = index, color = "2"))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Nominal CPUE", "Std CPUE"))+
  scale_fill_manual(values = cb_palette[2])+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, color = NULL, y = "CPUE Index")+
  facet_wrap(~district, ncol = 1) -> x

ggsave("./figures/observer/2024/wki_observer_cpue_std_index.png", plot = x, height = 3, width = 5, units = "in")

## YAK
core %>%
  transmute(district, core_data) %>% unnest(core_data) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(district, year) %>%
  summarise(cpue = mean(cpue)) %>% ungroup %>%  
  group_by(district) %>%
  mutate(cpue = cpue / (prod(cpue)^(1/n()))) %>% 
  right_join(index) %>%
  
  filter(district == "YAK") %>%
  
  ggplot()+
  geom_point(aes(x = year, y = cpue, color = "1"))+
  geom_line(aes(x = year, y = cpue, color = "1"))+
  geom_ribbon(aes(x = year, ymin = l95, ymax = u95, fill = "2"), alpha = 0.2, show.legend = F)+
  geom_point(aes(x = year, y = index, color = "2"))+
  geom_line(aes(x = year, y = index, color = "2"))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Nominal CPUE", "Std CPUE"))+
  scale_fill_manual(values = cb_palette[2])+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, color = NULL, y = "CPUE Index")+
  facet_wrap(~district, ncol = 1) -> x

ggsave("./figures/observer/2024/yak_observer_cpue_std_index.png", plot = x, height = 3, width = 5, units = "in")

# fishery extent ----

## maps of fishing effort by district, year
### KNE
catch %>%
  filter(district == "KNE") %>%
  filter(!is.na(set_lat), !is.na(set_lon)) %>%
  mutate(lat = round(set_lat/0.05)*0.05, lon = round(set_lon/0.05)*0.05) %>%
  group_by(season, district) %>%
  mutate(tot_effort = sum(dredge_hrs)) %>%
  group_by(season, district, lat, lon) %>%
  summarise(prop_effort = sum(dredge_hrs) / mean(tot_effort)) %>%
  
  ggplot()+
  geom_sf(data = land)+
  geom_tile(aes(x = lon, y = lat, fill = prop_effort))+
  geom_text_npc(data = drop_na(tmp), aes(npcx = "right", npcy = "top", label = season), check_overlap = T)+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  coord_sf(xlim = c(-153.2, -150), ylim = c(56.5, 58.7))+
  labs(x = NULL, y = NULL, fill = "Proportion \n Effort")+
  facet_wrap(~season)+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border= element_rect(color = 1),
        panel.spacing = unit(0, "lines"),
        strip.text.x = element_blank(),
        strip.background = element_blank()) -> x
ggsave("./figures/observer/2024/effort_map_KNE.png", plot = x, 
       height = 8, width = 7, unit = "in")
### KSH
catch %>%
  filter(district == "KSH") %>%
  filter(!is.na(set_lat), !is.na(set_lon)) %>%
  mutate(lat = round(set_lat/0.05)*0.05, lon = round(set_lon/0.05)*0.05) %>%
  group_by(season, district) %>%
  mutate(tot_effort = sum(dredge_hrs)) %>%
  group_by(season, district, lat, lon) %>%
  summarise(prop_effort = sum(dredge_hrs) / mean(tot_effort)) %>%
  
  ggplot()+
  geom_sf(data = land)+
  geom_tile(aes(x = lon, y = lat, fill = prop_effort))+
  geom_text_npc(data = drop_na(tmp), aes(npcx = "right", npcy = "top", label = season), check_overlap = T)+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  coord_sf(xlim = c(-155, -152.8), ylim = c(58, 59))+
  labs(x = NULL, y = NULL, fill = "Proportion \n Effort")+
  facet_wrap(~season)+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border= element_rect(color = 1),
        panel.spacing = unit(0, "lines"),
        strip.text.x = element_blank(),
        strip.background = element_blank()) -> x
ggsave("./figures/observer/2024/effort_map_KSH.png", plot = x, 
       height = 8, width = 7, unit = "in")
### KSW
catch %>%
  filter(district == "KSW") %>%
  filter(!is.na(set_lat), !is.na(set_lon)) %>%
  mutate(lat = round(set_lat/0.05)*0.05, lon = round(set_lon/0.05)*0.05) %>%
  group_by(season, district) %>%
  mutate(tot_effort = sum(dredge_hrs)) %>%
  group_by(season, district, lat, lon) %>%
  summarise(prop_effort = sum(dredge_hrs) / mean(tot_effort)) %>%
  
  ggplot()+
  geom_sf(data = land)+
  geom_tile(aes(x = lon, y = lat, fill = prop_effort))+
  geom_text_npc(data = drop_na(tmp), aes(npcx = "right", npcy = "top", label = season), check_overlap = T)+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  coord_sf(xlim = c(-156.4, -154.3), ylim = c(56, 58))+
  labs(x = NULL, y = NULL, fill = "Proportion \n Effort")+
  facet_wrap(~season)+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border= element_rect(color = 1),
        panel.spacing = unit(0, "lines"),
        strip.text.x = element_blank(),
        strip.background = element_blank()) -> x
ggsave("./figures/observer/2024/effort_map_KSW.png", plot = x, 
       height = 8, width = 7, unit = "in")
### WKI
catch %>%
  filter(district == "WKI") %>%
  filter(!is.na(set_lat), !is.na(set_lon)) %>%
  mutate(lat = round(set_lat/0.05)*0.05, lon = round(set_lon/0.05)*0.05) %>%
  group_by(season, district) %>%
  mutate(tot_effort = sum(dredge_hrs)) %>%
  group_by(season, district, lat, lon) %>%
  summarise(prop_effort = sum(dredge_hrs) / mean(tot_effort)) %>%
  
  ggplot()+
  geom_sf(data = land)+
  geom_tile(aes(x = lon, y = lat, fill = prop_effort))+
  geom_text_npc(data = drop_na(tmp), aes(npcx = "right", npcy = "top", label = season), check_overlap = T)+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  coord_sf(xlim = c(-145.2, -144), ylim = c(59.5, 60.1))+
  labs(x = NULL, y = NULL, fill = "Proportion \n Effort")+
  facet_wrap(~season)+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border= element_rect(color = 1),
        panel.spacing = unit(0, "lines"),
        strip.text.x = element_blank(),
        strip.background = element_blank()) -> x
ggsave("./figures/observer/2024/effort_map_WKI.png", plot = x, 
       height = 8, width = 7, unit = "in")

### YAK
catch %>%
  filter(district == "YAK") %>%
  filter(!is.na(set_lat), !is.na(set_lon)) %>%
  mutate(lat = round(set_lat/0.05)*0.05, lon = round(set_lon/0.05)*0.05) %>%
  group_by(season, district) %>%
  mutate(tot_effort = sum(dredge_hrs)) %>%
  group_by(season, district, lat, lon) %>%
  summarise(prop_effort = sum(dredge_hrs) / mean(tot_effort)) %>%
  
  ggplot()+
  geom_sf(data = land)+
  geom_tile(aes(x = lon, y = lat, fill = prop_effort))+
  geom_text_npc(data = drop_na(tmp), aes(npcx = "right", npcy = "top", label = season), check_overlap = T)+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.05, 0.10, 0.15))+
  coord_sf(xlim = c(-144.5, -136.5), ylim = c(57, 60.5))+
  labs(x = NULL, y = NULL, fill = "Proportion \n Effort")+
  facet_wrap(~season)+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border= element_rect(color = 1),
        panel.spacing = unit(0, "lines"),
        strip.text.x = element_blank(),
        strip.background = element_blank()) -> x
ggsave("./figures/observer/2024/effort_map_YAK.png", plot = x, 
       height = 8, width = 7, unit = "in")




## extent of round weight catch (f_extent_catch from general_observer_data_functions.R)
catch %>%
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  filter(!is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = c(-district, -season)) %>%
  group_by(district) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(district, season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  right_join(ghl %>%
               f_add_season() %>%
               mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district),
                      district = ifelse(district %in% c("D", "D16", "YAK"), "YAK", district)) %>%
               dplyr::select(season, district)) %>%
  filter(substring(season, 1, 4) >= 2009) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  nest(data = -district) %>%
  filter(!district %in% c("KSEM", "KSE", "KAM")) %>%
  mutate(plot = purrr::map2_chr(data, district, function(data, district) {
    
    # plot by district
    data %>%
      ggplot(aes(x = substring(season, 1, 4), y = value, linetype = metric, group = metric))+
      geom_point()+
      geom_line()+
      scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
      labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
      theme(legend.position = "bottom") -> x
    ggsave(paste0("./figures/observer/2024/cpue_extent_", district, ".png"), plot = x, 
           height = 3, width = 7, unit = "in")  
  }))


# bycatch ----

## get bycatch by day
bycatch %>%
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  group_by(season, district, set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            mt_wt = sum(mt_wt, na.rm = T),
            bairdi_count = sum(bairdi_count, na.rm = T),
            opilio_count = sum(opilio_count, na.rm = T),
            dungeness_count = sum(dungeness_count, na.rm = T),
            halibut_count = sum(halibut_count, na.rm = T),
            king_count = sum(king_count, na.rm = T)) -> bycatch_by_day

## total crab bycatch and bycatch:meatweight ratio by Season, District
### compute stats in large tibble to subset by district
bycatch_by_day %>%
  filter(!is.na(set_date)) %>%
  group_by(season, district) %>%
  summarise(total_effort = sum(dredge_hrs),
            total_sample = sum(sample_hrs),
            tanner_rate = sum(bairdi_count) / total_sample,
            total_tanner = tanner_rate * total_effort,
            tanner_ratio = total_tanner / sum(mt_wt, na.rm = T),
            snow_rate = sum(opilio_count) / total_sample,
            total_snow = snow_rate * total_effort,
            snow_ratio = total_snow / sum(mt_wt, na.rm = T),
            dungeness_rate = sum(dungeness_count) / total_sample,
            total_dungeness = dungeness_rate * total_effort,
            dungeness_ratio = total_dungeness / sum(mt_wt, na.rm = T),
            halibut_rate = sum(halibut_count) / total_sample,
            total_halibut = halibut_rate * total_effort,
            halibut_ratio = total_halibut  / sum(mt_wt, na.rm = T),
            total_king = sum(king_count),
            king_ratio = total_king / sum(mt_wt, na.rm = T),
            retained_mt = sum(mt_wt, na.rm = T)) %>%
  right_join(ghl %>%
               f_add_season %>%
               mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
               mutate(district = ifelse(district %in% c("YAK", "D", "D16"), "YAK", district)) %>%
               group_by(season, district) %>%
               summarise(ghl = sum(ghl, na.rm = T))) %>%
  left_join(cbl %>% 
              rename(district = scaldistrict) %>%
              f_add_season %>%
              mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
              mutate(district = ifelse(district %in% c("YAK", "D", "D16"), "YAK", district)) %>%
              group_by(season, district) %>%
              summarise(tanner_cbl = sum(bairdi_bycatch_limit, na.rm = T),
                        snow_cbl = sum(opilio_and_hybrid_bycatch_limit, na.rm = T), 
                        king_cbl = sum(king_bycatch_limit, na.rm = T))) %>%
  filter(!is.na(district),
         !district %in% c("KAM", "E", "R")) %>%
  group_by(district) %>%
  nest %>%
  
  mutate(crab_bycatch = purrr::map2(data, district, function(data, district) {
    
    # table and plots of non-target species bycatch ----
    data %>%
      select(season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio, total_snow, snow_ratio, total_dungeness, dungeness_ratio, total_halibut, 
             halibut_ratio) %>%
      filter(as.numeric(substring(season, 1, 4)) >= 2009) %>%
      arrange(season) %T>%
      # save output table
      write_csv(paste0("./output/observer/2024/bycatch_summary_", district, ".csv")) %>%
      pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
      mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                                 species == "total_dungeness" ~ "Dungeness crab",
                                 species == "total_tanner" ~ "Tanner crab",
                                 species == "total_king" ~ "Red king crab",
                                 species == "total_snow" ~ "Snow crab"),
             species = factor(species, c("Tanner crab", "Red king crab", "Snow crab", "Dungeness crab",
                                         "Pacific halibut"))) %>%
      ggplot(aes(x = substring(season, 1, 4), y = total, color = species, group = species))+
      geom_point()+
      geom_line()+
      labs(x = NULL, y = "Total catch (count)", color = NULL)+
      scale_y_continuous(labels = scales::comma)+
      scale_color_manual(values = cb_palette[c(1, 2, 4, 5, 6)])+
      facet_wrap(~species, scales = "free_y", ncol = 1)+
      theme(legend.position = "none") -> x
    ggsave(paste0("./figures/observer/2024/bycatch_totals_", district, ".png"), plot = x,
           height = 6, width = 7, units = "in") 
    
    # non-target species bycatch ratio ----
    
    data %>%
      dplyr::select(season, total_tanner, total_snow, total_dungeness, total_king, total_halibut,
                    retained_mt) %>%
      filter(as.numeric(substring(season, 1, 4)) >= 2009) %>%
      pivot_longer(c(total_tanner, total_snow, total_dungeness, total_king, total_halibut), 
                   names_to = "species", values_to = "total") %>%
      mutate(species = case_when(species == "total_tanner" ~ "Tanner crab",
                                 species == "total_snow" ~ "Snow crab",
                                 species == "total_dungeness" ~ "Dungeness crab",
                                 species == "total_king" ~ "Red king crab",
                                 species == "total_halibut" ~ "Pacific Halibut")) %>%
      ggplot(aes(x = substring(season, 1, 4), y = total / retained_mt, group = species, color = species))+
      geom_point()+
      geom_line()+
      scale_colour_manual(values = cb_palette[1:6])+
      labs(x = NULL, y = "Bycatch Ratio \n (number bycatch : lbs scallop meat)", color = NULL)+
      theme(legend.position = "bottom") -> x
    ggsave(paste0("./figures/observer/2024/tanner_bycatch_ratio_area_", district, ".png"), plot = x,
           height = 3, width = 7, units = "in")  
      
  }))

# discards ----

bycatch %>%
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  group_by(season, district, adfg, set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            disc_count = sum(disc_count, na.rm = T),
            disc_wt = sum(disc_wt, na.rm = T),
            broken_wt = sum(broken_wt, na.rm = T),
            rem_disc_wt = sum(rem_disc_wt, na.rm = T), .groups = "drop") -> discards_by_day


discards_by_day %>%
  #filter(district == "M") %>%
  group_by(district) %>%
  nest() %>% #pull(data) %>% .[[1]] -> data
  mutate(discards = purrr::map2_chr(data, district, function(data, district){
    # compute discards
    data %>%
      group_by(season) %>%
      summarise(effort = sum(dredge_hrs, na.rm = T),
                discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt, na.rm = T) / sum(sample_hrs, na.rm = T),
                discard_lb = discard_rate_lb * effort,
                disc_per_lb = sum(disc_count, na.rm = T) / sum(disc_wt, broken_wt, na.rm = T),
                discard_rate_num = (sum(disc_count, na.rm = T) + disc_per_lb * sum(rem_disc_wt, na.rm = T)) / sum(sample_hrs, na.rm = T),
                discard_num = discard_rate_num * effort) %>%
      left_join(catch %>%
                  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
                  rename(dist = district) %>%
                  filter(dist == district) %>%
                  group_by(season) %>%
                  summarise(round_weight = sum(round_weight, na.rm = T))) %>%
      # compute discard lbs ratio and overwrite object 'tmp'
      mutate(discard_ratio = discard_lb / round_weight,
             discard_M_lbs = discard_lb * 0.2,
             discard_M_num = discard_num * 0.2) %>%
      right_join(ghl %>%
                   mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
                   mutate(district = ifelse(district %in% c("YAK", "D", "D16"), "YAK", district)) %>%
                   f_add_season() %>%
                   dplyr::select(season) %>%
                   distinct %>%
                   filter(as.numeric(substring(season, 1, 4)) >= 2009)) -> disc
    
    # save discard summary table
    disc %>%
      dplyr::select(season, round_weight, discard_lb, discard_num, discard_ratio, discard_rate_lb, discard_rate_num,
                    discard_M_lbs, discard_M_num) %>%
      arrange(season) %>%
      write_csv(paste0("./output/observer/2024/discard_summary_", district, ".csv"))
    
    # plots ----
    disc %>%
      # fill in unfished years as space holders
      mutate(year = as.numeric(substring(season, 1, 4))) %>%
      arrange(year) -> pdisc
    
    # discard number and lbs
    pdisc %>%
      ggplot()+
      geom_point(aes(x = factor(year), y = discard_lb))+
      geom_line(aes(x = factor(year), y = discard_lb, group = 1))+
      labs(x = NULL, y = "Scallop Discards (lbs)")+
      scale_y_continuous(labels = scales::comma)+
      theme(legend.position = "none") -> x
    pdisc %>%
      ggplot()+
      geom_point(aes(x = factor(year), y = discard_num))+
      geom_line(aes(x = factor(year), y = discard_num, group = 1))+
      labs(x = NULL, y = "Scallop Discards (count)")+
      scale_y_continuous(labels = scales::comma)+
      theme(legend.position = "none") -> y
    ggsave(paste0("./figures/observer/2024/scallop_discards_", district, ".png"), 
           plot = x / y, height = 6, width = 7, units = "in")
    
    # discard ratio
    pdisc %>%
      ggplot(aes(x = factor(year), y = discard_ratio, 
                 group = district, color = district))+
      geom_point()+
      geom_line()+
      geom_hline(yintercept = 1, linetype = 2)+
      scale_colour_manual(values = cb_palette[1:5])+
      labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
      theme(legend.position = "bottom") -> x
    ggsave(paste0("./figures/observer/2024/scallop_discard_ratio_", district, ".png"), 
           plot = x, height = 3, width = 7, units = "in")
    
    # intact vs broken
    data %>%
      right_join(ghl %>%
                   f_add_season %>%
                   dplyr::select(season) %>% distinct) %>%
      filter(as.numeric(substring(season, 1, 4)) >= 2009) %>%
      ggplot()+
      geom_boxplot(aes(x = substring(season, 1, 4), y = disc_wt / broken_wt), fill = cb_palette[2], alpha = 0.3)+
      geom_hline(yintercept = 1, linetype = 2)+
      labs(x = NULL, y = "Intact : broken discard ratio") -> x
    ggsave(paste0("./figures/observer/2024/scallop_discard_broken_intact_ratio_", district, ".png"), 
           plot = x, height = 3, width = 7, units = "in")
  }))

# clappers ----
## compute clapper number
bycatch %>%
  filter(!(district %in% c("KNE", "KSH", "KSW", "KSE", "KSEM", "YAK",
                           "WKI", "EKI", "KAMN", "KAMS") & adfg %in% c(303, 32554, 40924, 54966))) %>%
  # all ak pen districts to area M
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  group_by(season, district, set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            clapper_count = sum(clapper_count, na.rm = T), .groups = "drop") %>%
  group_by(season, district) %>%
  summarise(effort = sum(dredge_hrs),
            clapper_rate = sum(clapper_count) / sum(sample_hrs),
            clapper_est = clapper_rate * effort) %>%
  group_by(district) %>%
  nest() %>%
  mutate(plot = purrr::map2_chr(data, district, function(data, district) {
    
    data %>%
      arrange(season) %>%
      write_csv(paste0("./output/observer/2024/clappers_", district, ".csv"))
    
    data %>%
      ggplot(aes(x = substring(season, 1, 4), y = clapper_rate, group = 1))+
      geom_point()+
      geom_line()+
      scale_y_continuous(labels = comma)+
      labs(x = NULL, y = "Clapper rate (count / dredge hour)") -> x
    ggsave(paste0("./figures/observer/2024/clappers_", district, ".png"), plot = x,
           height = 3, width = 7, units = "in")
  }))
  
# plot clapper catch in KSH ----

# clappers by vessel
bycatch %>%
  filter(district == "KSH") %>%
  group_by(season, district, adfg, set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            clapper_count = sum(clapper_count, na.rm = T), .groups = "drop") -> cbv
  
cbv %>%
  group_by(season, district, adfg) %>%
  summarise(effort = sum(dredge_hrs),
            clapper_rate = sum(clapper_count) / sum(sample_hrs),
            clapper_est = clapper_rate * effort) %>%
  right_join(expand_grid(season = unique(.$season),
                        adfg = unique(.$adfg))) %>%
  ggplot()+
  geom_point(aes(x = substring(season, 3, 4), y = clapper_rate, color = factor(adfg)))+
  geom_line(aes(x = substring(season, 3, 4), y = clapper_rate, group = factor(adfg), color = factor(adfg)))+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, color = NULL, y = "Clapper rate (count / dredge hour)")+
  scale_color_manual(values = cb_palette[2:6], labels = c("Polar Sea", "Arctic Hunter", "Ocean Hunter", "Kilkenny", "Provider"))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))-> x
ggsave("./figures/observer/2024/clappers_rate_by_vessel_ksh.png", plot = x, height = 4, width = 6, units = "in")

bycatch %>%
  filter(district == "KSH", season == "2023/24", sample_hrs > 0) %>%
  ggplot()+
  geom_point(aes(x = set_date, y = clapper_count/sample_hrs, color = factor(adfg)))+
  labs(x = NULL, color = NULL, y = "Clapper rate (count / dredge hour)")+
  scale_color_manual(values = cb_palette[2:3], labels = c("Ocean Hunter", "Provider"))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> x
ggsave("./figures/observer/2024/clappers_rate_by_vessel_and_date_ksh.png", plot = x, height = 4, width = 6, units = "in")  
  

bycatch %>%
  filter(district == "KSH", season == "2023/24", sample_hrs > 0) %>%
  ggplot()+
  geom_point(aes(x = est_rnd_wt/dredge_hrs, y = clapper_count/sample_hrs, color = factor(adfg)))+
  labs(x = "CPUE (round wt / dredge hr)", color = NULL, y = "Clapper rate (count / dredge hour)")+
  scale_color_manual(values = cb_palette[2:3], labels = c("Ocean Hunter", "Provider"))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> x
ggsave("./figures/observer/2024/clappers_rate_by_cpue_ksh.png", plot = x, height = 4, width = 6, units = "in")  


# map
bycatch %>%
    filter(district == "KSH", sample_hrs > 0, clapper_count > 0, season %in% c("2022/23", "2023/24")) %>%
    transmute(season, set_lat, set_lon, clapper_cpue = clapper_count/sample_hrs) -> clap_dat


ggplot()+
  geom_sf(data = land)+
  geom_point(data = filter(catch, season %in% c("2022/23", "2023/24")), aes(x = set_lon, y = set_lat, color = factor(adfg)), alpha = 0.5)+
  scale_color_manual(values = cb_palette[2:3], labels = c("Ocean Hunter", "Provider"))+
  facet_wrap(~season)+
  labs(x = expression(paste(Longitude^o,~'W')), y = expression(paste(Latitude^o,~'N')), color = NULL)+
  coord_sf(xlim = c(-154, -153), ylim = c(58.4, 58.8))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))+
  facet_wrap(~season, ncol = 1)-> x
ggsave("./figures/observer/2024/vessel_dredge_loc_ksh.png", plot = x, height = 6, width = 6, units = "in")
  
ggplot()+
  geom_sf(data = land)+
  geom_point(data = clap_dat, aes(x = set_lon, y = set_lat, size = clapper_cpue, color = clapper_cpue), alpha = 0.5)+
  scale_color_gradient(low = "#F0E442", high = "#D55E00")+
  guides(color = guide_legend(), size = guide_legend())+
  scale_size_continuous()+
  facet_wrap(~season)+
  labs(x = expression(paste(Longitude^o,~'W')), y = expression(paste(Latitude^o,~'N')), 
       color = "Clapper CPUE \n # / hr", size = "Clapper CPUE \n # / hr")+
  coord_sf(xlim = c(-154, -153), ylim = c(58.4, 58.8))+
  facet_wrap(~season, ncol = 1) -> x

ggsave("./figures/observer/2024/clappers_map_ksh.png", plot = x, height = 6, width = 6, units = "in")




# plot clapper catch in KNE ----

# clappers by vessel
bycatch %>%
  filter(district == "KNE") %>%
  group_by(season, district, adfg, set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            clapper_count = sum(clapper_count, na.rm = T), .groups = "drop") -> cbv

cbv %>%
  group_by(season, district, adfg) %>%
  summarise(effort = sum(dredge_hrs),
            clapper_rate = sum(clapper_count) / sum(sample_hrs),
            clapper_est = clapper_rate * effort) %>%
  right_join(expand_grid(season = unique(.$season),
                         adfg = unique(.$adfg))) %>%
  ggplot()+
  geom_point(aes(x = substring(season, 3, 4), y = clapper_rate, color = factor(adfg)))+
  geom_line(aes(x = substring(season, 3, 4), y = clapper_rate, group = factor(adfg), color = factor(adfg)))+
  scale_y_continuous(labels = scales::comma)+
  labs(x = NULL, color = NULL, y = "Clapper rate (count / dredge hour)")+
  scale_color_manual(values = cb_palette[2:6], labels = c("Polar Sea", "Arctic Hunter", "Ocean Hunter", "Kilkenny", "Provider"))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1))-> x
ggsave("./figures/observer/2024/clappers_rate_by_vessel_kne.png", plot = x, height = 4, width = 6, units = "in")

bycatch %>%
  filter(district == "KNE", season == "2023/24", sample_hrs > 0) %>%
  ggplot()+
  geom_point(aes(x = set_date, y = clapper_count/sample_hrs, color = factor(adfg)))+
  labs(x = NULL, color = NULL, y = "Clapper rate (count / dredge hour)")+
  scale_color_manual(values = cb_palette[2:3], labels = c("Ocean Hunter", "Provider"))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> x
ggsave("./figures/observer/2024/clappers_rate_by_vessel_and_date_kne.png", plot = x, height = 4, width = 6, units = "in")  


bycatch %>%
  filter(district == "KNE", season == "2023/24", sample_hrs > 0) %>%
  ggplot()+
  geom_point(aes(x = est_rnd_wt/dredge_hrs, y = clapper_count/sample_hrs, color = factor(adfg)))+
  labs(x = "CPUE (round wt / dredge hr)", color = NULL, y = "Clapper rate (count / dredge hour)")+
  scale_color_manual(values = cb_palette[2:3], labels = c("Ocean Hunter", "Provider"))+
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) -> x
ggsave("./figures/observer/2024/clappers_rate_by_cpue_ksh.png", plot = x, height = 4, width = 6, units = "in")  


# map
bycatch %>%
  filter(district == "KNE", sample_hrs > 0, clapper_count > 0, season %in% c("2022/23", "2023/24")) %>%
  transmute(season, set_lat, set_lon, clapper_cpue = clapper_count/sample_hrs) -> clap_dat


ggplot()+
  geom_sf(data = land)+
  geom_point(data = filter(catch, season %in% c("2022/23", "2023/24")), aes(x = set_lon, y = set_lat, color = factor(adfg)), alpha = 0.5)+
  scale_color_manual(values = cb_palette[2:3], labels = c("Ocean Hunter", "Provider"))+
  facet_wrap(~season)+
  labs(x = expression(paste(Longitude^o,~'W')), y = expression(paste(Latitude^o,~'N')), color = NULL)+
  coord_sf(xlim = c(-153.2, -150), ylim = c(56.5, 58.7))+
  theme(legend.position = c(1, 0), legend.justification = c(1, 0))+
  facet_wrap(~season, ncol = 1)-> x
ggsave("./figures/observer/2024/vessel_dredge_loc_kne.png", plot = x, height = 7, width = 7, units = "in")

ggplot()+
  geom_sf(data = land)+
  geom_point(data = clap_dat, aes(x = set_lon, y = set_lat, size = clapper_cpue, color = clapper_cpue), alpha = 0.5)+
  scale_color_gradient(low = "#F0E442", high = "#D55E00")+
  guides(color = guide_legend(), size = guide_legend())+
  scale_size_continuous()+
  facet_wrap(~season)+
  labs(x = expression(paste(Longitude^o,~'W')), y = expression(paste(Latitude^o,~'N')), 
       color = "Clapper CPUE \n # / hr", size = "Clapper CPUE \n # / hr")+
  coord_sf(xlim = c(-153.2, -150), ylim = c(56.5, 58.7))+
  facet_wrap(~season, ncol = 1) -> x

ggsave("./figures/observer/2024/clappers_map_kne.png", plot = x, height = 7, width = 7, units = "in")






# shell height composition ----

## create dataset with SH and appropriate weights
bycatch %>%
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  # compute a daily discard rate (lbs/dregde hr)
  group_by(season, district, set_date) %>%
  summarise(disc_wt = sum(disc_wt, broken_wt, rem_disc_wt),
            sample = sum(sample_hrs)) %>%
  group_by(season, district) %>%
  mutate(disc_rate = ifelse(sample != 0, 
                            disc_wt / sample, 
                            sum(disc_wt) / sum(sample))) %>%
  # join to catch data by haul
  dplyr::select(season, district, set_date, disc_rate) %>%
  right_join(catch %>%
               mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)), 
             by = c("season", "district", "set_date")) %>%
  # estimate discards by haul
  mutate(disc_est_lbs = dredge_hrs * disc_rate) %>%
  # estimate weights for shell height histogram (prop of annual catch)
  dplyr::select(season, district, haul_id, round_weight, disc_est_lbs) %>%
  pivot_longer(c(round_weight, disc_est_lbs), 
               names_to = "rtnd_disc", values_to = "wt_lbs") %>%
  mutate(rtnd_disc = ifelse(rtnd_disc == "round_weight", "R", "D"),
         w = wt_lbs / sum(wt_lbs, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(haul_id, rtnd_disc, w) %>%
  right_join(shell_height %>% filter(rtnd_disc != "M")%>%
               mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)), 
             by = c("haul_id", "rtnd_disc") ) -> size_wts
write_csv(size_wts, "./output/observer/2024/fishery_size_comp_wts.csv")

size_wts %>%
  group_by(district) %>%
  nest() %>%
  mutate(plot = purrr::map2_chr(data, district, function (data, district){
    
    # fake a ridgeline plot so that proportion can be weighted
    data %>%
      filter(rtnd_disc %in% c("R", "D")) %>%
      # round sh to the 5
      mutate(sh_bin = (floor(sh/5) * 5)) %>%
      group_by(season) %>%
      mutate(sum_w = sum(w, na.rm = T)) %>%
      group_by(season, sh_bin, rtnd_disc) %>%
      summarise(prop = sum(w, na.rm = T) / mean(sum_w)) %>%
      right_join(expand_grid(season = unique(.$season),
                             sh_bin = seq(50, 200, 5),
                             rtnd_disc = c("R", "D"))) %>%
      replace_na(list(prop = 0)) -> tmp
    # add the edge of bin so you can plot bars
    bind_rows(tmp, tmp %>%
                mutate(sh_bin = sh_bin + 5 - 1e-10)) %>%
      ggplot()+
      geom_area(aes(x = sh_bin, y = prop, fill = rtnd_disc), color = "grey40", alpha = 0.5)+
      geom_text_npc(aes(npcx = "left", npcy = 0.3, label = season), check_overlap = T)+
      facet_wrap(~season, ncol = 1, dir = "v", )+
      labs(x = "Shell Height (mm)", y = NULL, fill = NULL)+
      scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))-> x
    
    if(district %in% c("KSE", "KSEM", "EKI", "C")) {
      x + theme(panel.border= element_blank(),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_line(size = 0.1, color = "grey70"),
            axis.ticks.y = element_blank(),
            panel.background = element_blank()) -> x
      ggsave(paste0("./figures/observer/2024/sh_comp_", district, ".png"), plot = x,
             height = 2, width = 4, units = "in")
      
      } else {
              x + theme(panel.border= element_blank(),
                        panel.spacing = unit(-1, "lines"),
                        strip.background = element_blank(),
                        strip.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.line.y = element_blank(),
                        axis.line.x = element_line(size = 0.1, color = "grey70"),
                        axis.ticks.y = element_blank(),
                        panel.background = element_blank()) -> x
        nyrs = length(unique(data$season))
        ggsave(paste0("./figures/observer/2024/sh_comp_", district, ".png"), plot = x,
               height = (8/11) * nyrs, width = 4, units = "in")
            }
    
    
  }))



  

# retention curves ----

shell_height %>% 
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  filter(!district %in% c("KSE", "KSEM", "O")) %>%
  group_by(district) %>%
  nest %>% #pull(data) %>% .[[6]] -> data
  mutate(ret = purrr::map2_chr(data, district, function(data, district){
    
    #filter data and define response
    data %>% 
      filter(rtnd_disc != "M") %>%
      mutate(retained = as.numeric(rtnd_disc == "R")) -> tmp
    
    # fit binomial model with year effect
    glm(retained ~ sh + season, family = "binomial", data = tmp) -> fit
    
    # retention curves
    tmp %>%
      mutate(response = predict(fit, type = "response")) %>%
      ggplot()+
      geom_point(aes(x = sh, y = retained), shape = 124, color = "grey50")+
      geom_line(aes(x = sh, y = response, group = season))+
      labs(x = "Shell Height (mm)", y = "Probability of Retention") -> x
    
    # plot trend in 50% retention
    tibble(season = unique(tmp$season),
           r50 = as.numeric(c(coef(fit)[1], coef(fit)[1] + coef(fit)[-1:-2]) / -coef(fit)[2]),
           r10 = (log(1/0.1-1) + as.numeric(c(coef(fit)[1], coef(fit)[1] + coef(fit)[-1:-2]))) / -coef(fit)[2]) %>% 
      pivot_longer(c(r50, r10), names_to = "quantile", values_to = "est") %>%
      ggplot(aes(x = substring(season, 1, 4), y = est, color = quantile, group = quantile))+
      geom_point()+
      geom_line()+
      labs(x = NULL, y = "Shell Height (mm)", color = NULL)+
      scale_color_manual(values = c("grey60", "black"), labels = c(expression(Ret[10]), expression(Ret[50])))+
      theme(legend.position = c(1,1),
            legend.justification = c(1,1)) -> y
    ggsave(paste0("./figures/observer/2024/retention_curve_", district, ".png"), plot = x / y,
           height = 6, width = 6, units = "in")
    
  }))
  
  
# gonads ----

# plot of gonad condition by proportion 
meat %>% 
  # extract month
  mutate(month = month(mdy(sample_date)),
         month = factor(month.name[month], c(month.name[7:12], month.name[1:6])),
         district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  filter(!is.na(gonad)) %>%
  group_by(district, season, month) %>% #ungroup %>%count(gonad)
  mutate(tot = n(),
         gonad = case_when(gonad == 0 ~ "Immature",
                           gonad == 1 ~ "Empty",
                           gonad == 2 ~ "Intitial Recovery",
                           gonad == 3 ~ "Filling",
                           gonad == 4 ~ "Full",
                           gonad == 5 ~ "Cannot Determine"))  %>%
  group_by(district, gonad, season, month, tot) %>%
  summarise(prop = n()) %>%
  mutate(prop = prop / tot) %>%
  group_by(district) %>%
  nest() %>%
  mutate(plot = purrr::map2_chr(data, district, function(data, district) {
    
    data %>%
    ggplot()+
      geom_bar(aes(x = substring(season, 1, 4), y = prop, fill=factor(gonad)), stat = "identity")+
      scale_fill_manual(values = cb_palette[c(2, 3, 4, 6, 7, 8)])+
      guides(fill=guide_legend(nrow=2,byrow=TRUE))+
      facet_wrap(~month, ncol = 2)+
      labs(x = NULL, y = "Proportion", fill = NULL) -> x
    ggsave(paste0("./figures/observer/2024/gonad_", district, ".png"), plot = x, 
           height = 5, width = 7, units = "in")
    
  }))


# meat weight ~ shell height ----

## plot meat weight ~ shell height by district
meat %>% 
  mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  group_by(district) %>%
  nest() %>%
  mutate(plot = purrr::map2_chr(data, district, function(data, district) {
    
    data %>%
      ggplot()+
      geom_point(aes(x = log(shell_height), y = log(meat_weight), color = season), alpha = 0.1)+
      geom_smooth(aes(x = log(shell_height), y = log(meat_weight), color = season), method = "lm", se = F)+
      labs(x = "ln Shell height(mm)", y = "ln Meat Weight (g)", color = NULL)+
      theme(legend.position = c(1, 0),
            legend.justification = c(1, 0)) -> x
    ggsave(paste0("./figures/observer/2024/mwsh_", district, ".png"), plot = x, 
           height = 3, width = 5, units = "in")
    
  }))


# meat weight ~ round weight ----

## plot of all data
catch %>%
  ggplot()+
  geom_point(aes(x = round_weight, y = meat_weight), color = cb_palette[1], alpha = 0.2)+
  geom_smooth(aes(x = round_weight, y = meat_weight), color = cb_palette[6], method = "loess", se = F)+
  geom_smooth(aes(x = round_weight, y = 0.1 * round_weight), color = 1, method = "lm", se = F, linetype = 2)+
  coord_cartesian(ylim = c(0, 600), expand = 0)+
  labs(x = "Round weight (lbs)", y = "Meat weight (lbs)") -> x
ggsave("./figures/observer/2024/mw_rw_scatterplot.png", plot = x,
       height = 4, width = 6, units = "in")


# voilin plots by district and season
catch %>%
  filter(!(district %in% c("C", "KSEM")),
         !is.na(district)) %>%
  mutate(district = factor(district, levels = c("KNE", "KSH", "KSW", "KSE", "UB", 
                                                "O", "Q", "WKI", "EKI", "YAK"))) %>%
  ggplot()+
  geom_violin(aes(x = substring(season, 1, 4), y = meat_weight / round_weight), adjust = 1.5, fill = cb_palette[4])+
  geom_boxplot(aes(x = substring(season, 1, 4), y = meat_weight / round_weight), outlier.shape = NA, width = 0.2)+
  coord_cartesian(ylim = c(0, 0.2))+
  geom_hline(yintercept = 0.1, linetype = 2)+
  facet_wrap(~district, ncol = 2)+
  labs(x = NULL, y = "Meat weight : Round weight")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./figures/observer/2024/mw_rw_violin.png", plot = x,
       height = 8, width = 7, units = "in")


