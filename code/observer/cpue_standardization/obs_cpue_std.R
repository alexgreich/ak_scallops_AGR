# notes ----
## observer cpue std using GAM
## tyler jackson
## 12/1/2025

# load ----

source("./code/observer/scalobservR.R")
source("../BSAI_crab_assessments/AIGKC/code/aigkc_functions.R")

# data ----

## observer/logbook data
### scallop haul data 2009/10 - Present
catch_wiki <- do.call(bind_rows,
                      lapply(list.files("./data/observer/catch/", full.names = T), read_csv))

## clean catch data
#catch <- f_clean_catch(catch_wiki)

catch <- load_catch_by_haul("./data/observer/catch/", district = NULL, database_pull = F, clean = T) 

# fit data ----

catch %>%
  # filter for only districts that have surveys
  filter(district %in% c("KSH", "KNE", "YAK", "WKI", "EKI")) %>%
  # combine EKI and YAK, since EKI is the same bed as YAKB
 # mutate(district = ifelse(district == "EKI", "YAK", district)) %>%
  # combine area M districts
  #mutate(district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
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


## fit models in a for loop --- unclear why I cannot get purrr::map to work with f_step_gam()
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
saveRDS(core, "./output/observer/2025/assessment_cpue_std.RDS")

# diagnostics ---- 

core <- readRDS("./output/observer/2025/assessment_cpue_std.RDS")

# save plots of dharma residuals
mapply(function(district, model) {
    
    # simulate dharma residuals
    simr <- simulateResiduals(fittedModel = model, plot = F)
    # make qq plot of simulatied residuals - should be uniform
    qq <- wrap_elements(panel = ~plotQQunif(simr, testUniformity = T, testOutliers = F, testDispersion = F, cex = 0.2), clip = F)
    # plot residuals against ranked model predictions
    rf <- wrap_elements(panel = ~plotResiduals(simr, smoothScatter = F, cex = 0.1, pch = 16), clip = F)
    # save plot
    ggsave(paste("./figures/observer/2025/", district, "_assessment_cpue_std_diag.png"), plot = qq + rf, height = 5, width = 9, units = "in")
    
    return(T)
    
  }, district = core$district, model = core$model)


# marginal effects ----
# just do one at a time because its 

## YAK
core %>% pull(model) %>% .[[1]] %>% getViz(.) -> yak_viz
plot(sm(yak_viz, 1))+ 
  l_points(color = "grey80") + 
  l_fitLine() +
  l_ciLine() + 
  labs(x = "Depth (fa)") -> depth
plot(pterm(yak_viz, 5))+ 
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
plot(pterm(yak_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Month", y = "f(Month)") -> month
ggsave("./figures/observer/2025/assessment_cpue_std_yak_effects.png",
       plot = gridPrint(depth, bed, vessel, dredge_width, month, ncol = 2),
       height = 8, width = 6, units = "in")

## WKI
core %>% pull(model) %>% .[[2]] %>% getViz(.) -> wki_viz
plot(sm(wki_viz, 1))+ 
  l_points(color = "grey80") + 
  l_fitLine() +
  l_ciLine() + 
  labs(x = "Depth (fa)") -> depth
ggsave("./figures/observer/2025/assessment_cpue_std_wki_effects.png",
       plot = gridPrint(depth),
       height = 3, width = 3, units = "in")

## EKI
core %>% pull(model) %>% .[[3]] %>% getViz(.) -> eki_viz
plot(pterm(eki_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Month", y = "f(Month)") -> month
ggsave("./figures/observer/2025/assessment_cpue_std_eki_effects.png",
       plot = gridPrint(month),
       height = 3, width = 3, units = "in")

## KSH
core %>% pull(model) %>% .[[5]] %>% getViz(.) -> ksh_viz
# plot(sm(ksh_viz, 1))+ 
#   l_points(color = "grey80") + 
#   l_fitLine() +
#   l_ciLine() + 
#   labs(x = "Depth (fa)") -> depth
plot(pterm(ksh_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Month", y = "f(Month)") -> month
# plot(pterm(ksh_viz, 3))+ 
#   l_points(col = "grey80") + 
#   l_ciBar(width = 0.5) + l_fitPoints() +
#   labs(x = "Dredge Width", y = "f(Dredge Width)") -> dredge_width
ggsave("./figures/observer/2025/assessment_cpue_std_ksh_effects.png",
       plot = gridPrint(month),
       height = 3, width = 3, units = "in")

## KNE
core %>% pull(model) %>% .[[4]] %>% getViz(.) -> kne_viz
plot(sm(kne_viz, 1))+ 
  l_points(color = "grey80") + 
  l_fitLine() +
  l_ciLine() + 
  labs(x = "Depth (fa)") -> depth
plot(pterm(kne_viz, 2))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Bed", y = "f(Bed)") -> bed
plot(pterm(kne_viz, 3))+ 
  l_points(col = "grey80") + 
  l_ciBar(width = 0.5) + l_fitPoints() +
  labs(x = "Month", y = "f(Month)") -> month
# plot(pterm(kne_viz, 3))+ 
#   l_points(col = "grey80") + 
#   l_ciBar(width = 0.5) + l_fitPoints() +
#   labs(x = "Dredge Width", y = "f(Dredge Width)") -> dredge_width

ggsave("./figures/observer/2025/assessment_cpue_std_kne_effects.png",
       plot = gridPrint(depth, bed, month, ncol = 2),
       height = 6, width = 6, units = "in")

# get index ----

core %>% #pull(model) %>% .[[2]] -> model
  mutate(index = purrr::map2(core_data, model, function(core_data, model) {
    loc <- grep("season", names(coef(model)))
    yrs <- unique(core_data$season)
    f_getCPUE_gam(model, loc, yrs)%>%
      mutate(year = as.numeric(as.character(substring(year, 1, 4)))) %>%
      right_join(tibble(year = 1993:2024)) %>% arrange(year)
  })) %>%
  transmute(district, index) %>%
  unnest(index) -> index

# write output
write_csv(index, "./output/observer/2025/assessment_cpue_std_index.csv")

## plot index
core %>%
  transmute(district, core_data) %>% unnest(core_data) %>%
  mutate(year = as.numeric(as.character(substring(season, 1, 4)))) %>%
  group_by(district, year) %>%
  summarise(cpue = mean(cpue)) %>% ungroup %>%  
  group_by(district) %>%
  mutate(cpue = cpue / (prod(cpue)^(1/n()))) %>% 
  right_join(index) %>%
  mutate(district = factor(district, levels = c("KSH", "KNE", "WKI", "EKI", "YAK"))) %>% 
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
  facet_wrap(~district, ncol = 1) -> p

ggsave("./figures/observer/2025/assessment_cpue_std_index.png", plot = p, height = 8, width = 6, units = "in")
