# notes ----
## dredge survey functions
## tyler jackson
## tyler.jackson@alaska.gov
## 2/10/2024

######### all estimate and plot functions exclude kamishak for now. will be added if surveyed again #########

# load ----

if(!("tidyverse" %in% rownames(installed.packages()))) {install.packages("tidyverse")}
if(!("patchwork" %in% rownames(installed.packages()))) {install.packages("patchwork")}
if(!("scales" %in% rownames(installed.packages()))) {install.packages("scales")}
if(!("ggpmisc" %in% rownames(installed.packages()))) {install.packages("ggpmisc")}

library(tidyverse)
library(patchwork)
library(ggpmisc)

# plot options -----

# graphic options
theme_sleek <- function(base_size = 12) {
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size) +
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
yraxis <- tickr(tibble(yr = 1980:2100), yr, 2)

# f_load_strata() ----

## load station information
## args: file_path - path to strata data (DredgeSurveyStationTable.csv) file including extension, default = "./data/dredge_survey/DredgeSurveyStationTable.csv"
 
f_load_strata <- function(file_path = "./data/dredge_survey/DredgeSurveyStationTable.csv") {
  read_csv(file_path) %>%
    mutate(district_full = case_when(district == "EKI" ~ "East Kayak Island",
                                     district == "KAM" ~ "Kamishak",
                                     district == "KNE" ~ "Kodiak Northeast",
                                     district == "KSH" ~ "Kodiak Shelikof",
                                     district == "WKI" ~ "West Kayak Island",
                                     district == "YAK" ~ "Yakutat")) %>%
    # filter for only active stations
    filter(status == "A") -> out
  return(out)
}

# f_load_clean_logbook() ----

## load and format logbook data
## args: file_path - path to logbook data csv including extension, default = "./data/dredge_survey/logbook/DredgeSurvey_FishLogData_RegularSurveyHauls.csv"

f_load_clean_logbook <- function(file_path = "./data/dredge_survey/logbook/DredgeSurvey_FishLogData_RegularSurveyHauls.csv", drop = T) {
  
  read.csv(file_path) %>%
    as_tibble() %>%
    filter(haul_type == 10,
           tow != 19010054, # double tow, thanks Quinn!
           perform %in% 1:4) %>% # quality tows
    mutate(lat = (lat_start + lat_end) / 2,
           lon = (lon_start + lon_end) / 2,
           area_swept = distance_nm * 0.00131663,
           depth_avg = ifelse(is.na(depth_avg), (depth_start + depth_end) / 2, depth_avg)) %>%
    rename(year = cruise_year) -> out
  if(drop == T){
    out %>%
      dplyr::select(year, cruise, tow, haul, perform, district, bed_code, bed_name, lat, lon, depth_avg, area_swept) -> out
  }
  
  return(out)
  
}


# f_load_clean_catch() ----

## args: file_path - path to logbook data csv including extension, default = "./data/dredge_survey/catch/DredgeSurvey_CatchData_RegularSurveyHauls.csv"
##       tows - cleaned logbook data (i.e., output of f_load_clean_logbook())
##       spp - option (filter for species of choice, scallops = 74120)

f_load_clean_catch <- function(file_path = "./data/dredge_survey/catch/DredgeSurvey_CatchData_RegularSurveyHauls.csv",
                               tows,
                               spp = NULL) {
  x <- read.csv(file_path)
  
  # remove clappers and empty shells
  x %>%
    filter(!(comname %in% c("Empty shells", "Clapper", "Clappers", "Empty_Shells, Scallop", "Empty Scallop Shells"))) %>%
    mutate(comname = ifelse(grepl("eathervane", comname), "weathervane scallop", comname)) %>%
    
    # temporarily give small scallops a different rcode for ease of data summary
    
    mutate(rcode = ifelse((rcode == 74120 & samp_grp == 2), rcode + 9999999, rcode)) -> x
  
  # deal with all whole hauled samples
  x %>%
    filter(whole_haul == "Y") %>%
    group_by(tow, rcode, samp_grp) %>%
    summarise(samp_cnt = sum(samp_cnt, na.rm = T),
              samp_wt = sum(samp_wt, na.rm = T)) -> tmp
  
  # deal with non-whole hauled samples
  # join and include non-catch hauls
  # compute cpue
  x %>%
    filter(whole_haul == "N") %>%
    dplyr::select(tow, rcode, samp_cnt, samp_wt) %>%
    group_by(tow) %>%
    mutate(sub_wt = sum(samp_wt, na.rm = T)) %>%
    left_join(tmp %>%
                ungroup() %>%
                filter(rcode == 99997) %>%
                dplyr::select(tow, samp_wt) %>%
                rename(bulk_wt = samp_wt), by = "tow") %>%
    mutate(bulk_wt = bulk_wt + sub_wt) %>%
    mutate(samp_wt = bulk_wt / sub_wt * samp_wt,
           samp_cnt = bulk_wt / sub_wt * samp_cnt) %>%
    dplyr::select(-sub_wt, -bulk_wt) %>%
    bind_rows(tmp) %>%
    right_join(expand_grid(rcode = unique(x$rcode), 
                           tow = unique(tows$tow)) %>%
                 mutate(samp_grp = case_when(rcode == 74120 ~ 1,
                                             rcode == (74120 + 9999999) ~ 2)), 
               by = c("rcode", "tow", "samp_grp"))  %>%
    filter(rcode != 99997) %>%
    replace_na(list(samp_wt = 0)) %>%
    mutate(samp_cnt = ifelse(samp_wt == 0, 0, samp_cnt)) %>%
    right_join(tows, by = "tow") %>%
    mutate(cpue_cnt = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_cnt / (0.83*area_swept), samp_cnt / area_swept),
           cpue_kg = ifelse(rcode %in% c(74120, 74120 + 9999999), samp_wt / (0.83*area_swept), samp_wt / area_swept)) %>%
    ungroup %>%
    dplyr::select(6, 7, 1, 8:16, 2, 5, 3:4, 17:18) %>%
    # change rcode of small scallops back
    mutate(rcode = ifelse(rcode == (74120 + 9999999), 74120, rcode)) -> out
  
  if(!is.null(spp)) {
    filter(out, rcode == spp) -> out
  }
  
  return(out)
  
}
# f_load_clean_shaw() -----

## args: file_path - path to logbook data csv including extension, default = "./data/dredge_survey/specimen/DredgeSurvey_ScallopBioData_RegularSurveyHauls.csv"
##       tows - cleaned logbook data (i.e., output of f_load_clean_logbook())

f_load_clean_shaw <- function(file_path = "./data/dredge_survey/specimen/DredgeSurvey_ScallopBioData_RegularSurveyHauls.csv",
                              tows) {
  
  read.csv(file_path) %>%
    filter(tow %in% tows$tow) %>%
    as_tibble() %>%
    rename(year = cruise_year) %>%
    filter(!is.na(sex)) -> out
  
  return(out)
  
}

# f_load_clean_shad() -----

## args: file_path - path to logbook data csv including extension, default = "./data/dredge_survey/specimen/DredgeSurvey_ScallopBioData_RegularSurveyHauls.csv"
##       catch - cleaned catch data (i.e., output of f_load_clean_catch())

f_load_clean_shad <- function(file_path = "./data/dredge_survey/specimen/DredgeSurvey_ScallopBioData_RegularSurveyHauls.csv",
                              catch) {
  
  read.csv(file_path) %>%
    filter(tow %in% catch$tow) %>%
    as_tibble() %>%
    filter(samp_grp %in% c(1, 2),
           !is.na(shell_height)) %>%
    dplyr::select(-whole_wt, -sex, -shell_num, -gonad, -meat_condition, -mud_blister,
                  -shell_worm, -shell_retained, -meat_wt) %>%
    rename(year = cruise_year) %>%
    group_by(year, tow, samp_grp, shell_height, damage) %>%
    summarise(count = n()) %>%
    group_by(tow, samp_grp) %>%
    mutate(n_measured = sum(count)) %>% ungroup %>%
    # join to catch data to comput sample_fraction 
    # no data that are not in standard, good perfomance hauls
    left_join(catch %>%
                filter(rcode == 74120) %>%
                dplyr::select(year, cruise, tow, haul, district, bed_code, bed_name, lat, lon, depth_avg,
                              area_swept, rcode, samp_grp, samp_cnt),
              by = c("tow", "samp_grp", "year")) %>%
    mutate(sample_factor = samp_cnt * (count / n_measured)) %>%
    dplyr::select(year, cruise, tow, haul, district, bed_code, bed_name, lat, lon, depth_avg, area_swept,
                  rcode, samp_grp, shell_height, damage, sample_factor) -> out 
  return(out)
  
}

# f_est_abundance() ----
# args: catch - cleaned catch data (output of f_load_clean_catch)
#       strata - cleaned strata data (output of f_load_strata)
#       spp - species race code, default = 74120, if not scallops make sure `write_csv` and `plot` are FALSE
#       by - stratifying variable, default = "bed_name", alternative = "bed_code"
#       write_csv - T/F, default = T
#       plot - T/F, default = T
#       size_range - vector of size range, example: c(75, 100). Use NA for no min and/or max limit c(75, NA)
#       csv_dir - path to directory to store csv file
#       plot_dir - Path to directory to store plots
f_est_abundance <- function(catch, strata, spp = 74120, by = "bed_name", write_csv = T, plot = T,
                            size_range, shad, csv_dir = NULL, plot_dir = NULL) {
  
  # create output directories
  if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # get bed area 
  strata %>% # strata loaded in custom functions
    ## compute bed area
    group_by(bed_code, bed_name, district) %>%
    summarise(area_nm2 = sum(sq_nmi_alb),
              n_stations = n()) %>% ungroup -> bed_area
  
  # abundance (by sample group) ----
  if(spp == 74120 & missing(size_range)) {
    
  catch %>%
    filter(rcode == spp) %>%
    left_join(bed_area, by = c("bed_code", "bed_name", "district")) %>%
    group_by_at(vars(c("year", "samp_grp", "district", by))) %>%
    summarise(cpue = mean(cpue_cnt, na.rm = T),
              abundance = mean(cpue_cnt, na.rm = T) * mean(area_nm2),
              se = sqrt(var(cpue_cnt, na.rm = T) / n() * mean(area_nm2)^2),
              cv = sqrt(var(cpue_cnt, na.rm = T) / n()) / mean(cpue_cnt, na.rm = T)) %>% ungroup %>%
    # add lognormal confience intervals
    mutate(l95 = abundance * exp(-1.96 * sqrt(log(1 + cv^2))),
           u95 = abundance * exp(1.96 * sqrt(log(1 + cv^2)))) -> out
  
  # write csv output
  if(write_csv == T) {write_csv(out, file.path(csv_dir, "abundance_est.csv"))}
  # plot output
  if(plot == T){
    # plots (separately by district, samp grp for cute sizing)
    # abundance in millions
    out$abundance <- out$abundance / 1e6
    out$l95 <- out$l95 / 1e6; out$u95 <- out$u95 / 1e6
    ## ksh (only plots ksh1)
    out %>%
      filter(district == "KSH",
             !(eval(as.symbol(by)) %in% c("KSH2", "KSH3")),
             samp_grp == 1) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)", title = "KSH1")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5)) -> ksh1_1
    ggsave(file.path(plot_dir, "abundance_ksh1_samp_grp1.png"), plot = ksh1_1, width = 5, height = 3, units = "in")
    out %>%
      filter(district == "KSH",
             !(eval(as.symbol(by)) %in% c("KSH2", "KSH3")),
             samp_grp == 2) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)", title = "KSH1")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5)) -> ksh1_2
    ggsave(file.path(plot_dir, "abundance_ksh1_samp_grp2.png"), plot = ksh1_2, 
           width = 5, height = 3, units = "in")
    # kne (not kne4)  
    out %>%
      filter(district == "KNE",
             !(eval(as.symbol(by)) %in% c("KNE4")),
             samp_grp == 1) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne_1
    ggsave(file.path(plot_dir, "abundance_kne_samp_grp1.png"), plot = kne_1, 
           width = 7, height = 6.5, units = "in")
    out %>%
      filter(district == "KNE",
             !(eval(as.symbol(by)) %in% c("KNE4")),
             samp_grp == 2) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne_2
    ggsave(file.path(plot_dir, "abundance_kne_samp_grp2.png"), plot = kne_2, 
           width = 7, height = 6.5, units = "in")
    # wki   
    out %>%
      filter(district == "WKI",
             samp_grp == 1) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)", title = "WKI")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5)) -> wki_1
    ggsave(file.path(plot_dir, "abundance_wki_samp_grp1.png"), plot = wki_1, width = 5, height = 3, units = "in")
    out %>%
      filter(district == "WKI",
             samp_grp == 2) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)", title = "WKI")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5)) -> wki_2
    ggsave(file.path(plot_dir, "abundance_wki_samp_grp2.png"), plot = wki_2, 
           width = 5, height = 3, units = "in")
    # eki   
    out %>%
      filter(district == "EKI",
             samp_grp == 1) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)", title = "EKI")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5)) -> eki_1
    ggsave(file.path(plot_dir, "abundance_eki_samp_grp1.png"), plot = eki_1, width = 5, height = 3, units = "in")
    out %>%
      filter(district == "EKI",
             samp_grp == 2) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)", title = "EKI")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5)) -> eki_2
    ggsave(file.path(plot_dir, "abundance_eki_samp_grp2.png"), plot = eki_2, 
           width = 5, height = 3, units = "in")
    # yak  
    out %>%
      filter(district == "YAK",
             samp_grp == 1) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak_1
    ggsave(file.path(plot_dir, "abundance_yak_samp_grp1.png"), plot = yak_1, 
           width = 7, height = 6.5, units = "in")
    out %>%
      filter(district == "YAK",
             samp_grp == 2) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance))+
      geom_line(aes(x = year, y = abundance, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = "Abundance (millions)")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak_2
    ggsave(file.path(plot_dir, "abundance_yak_samp_grp2.png"), plot = yak_2, 
           width = 7, height = 6.5, units = "in")
    # plot all beds sampled in most recent surevy
    yr <- max(out$year)
    out %>%
      filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by)))),
             samp_grp == 1) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance, color = district))+
      geom_line(aes(x = year, y = abundance, color = district))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
      labs(x = NULL, y = "Abundance (millions)")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_color_brewer(palette = "Set2")+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_1
    ggsave(file.path(plot_dir, paste0("abundance_", yr, "_samp_grp1.png")), plot = yr_1, 
           width = 7, height = 8, units = "in")
    out %>%
      filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by)))),
             samp_grp == 2) %>%
      ggplot()+
      geom_point(aes(x = year, y = abundance, color = district))+
      geom_line(aes(x = year, y = abundance, color = district))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
      labs(x = NULL, y = "Abundance (millions)")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_color_brewer(palette = "Set2")+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_2
    ggsave(file.path(plot_dir, paste0("abundance_", yr, "_samp_grp2.png")), plot = yr_2, 
           width = 7, height = 8, units = "in")
  }
  
  }
  
  # abundance (by specific size range) ----
  if(spp == 74120 & !missing(size_range)) {
    
    if(missing(shad)){stop("must include shad data")}
    size_range[is.na(size_range)] <- 0
    sizes <- c(0 + size_range[1], 1e6 - size_range[2]) 
     
    shad %>%
      filter(rcode == 74120,
             shell_height >= sizes[1], shell_height <= sizes[2]) %>%
      left_join(bed_area, by = c("bed_code", "bed_name", "district")) %>%
      group_by_at(vars(c("year", "samp_grp", "district", by, "tow", "area_nm2"))) %>%
      summarise(cpue_cnt = sum(sample_factor) / mean(0.83*area_swept)) %>% ungroup %>%
      group_by_at(vars(c("year", "district", by))) %>%
      summarise(cpue = mean(cpue_cnt, na.rm = T),
                abundance = mean(cpue_cnt, na.rm = T) * mean(area_nm2),
                se = sqrt(var(cpue_cnt, na.rm = T) / n() * mean(area_nm2)^2),
                cv = sqrt(var(cpue_cnt, na.rm = T) / n()) / mean(cpue_cnt, na.rm = T)) %>% ungroup %>%
      # add lognormal confience intervals
      mutate(l95 = abundance * exp(-1.96 * sqrt(log(1 + cv^2))),
             u95 = abundance * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
      mutate(size_grp = paste0(ifelse(size_range[1] == 0, NA, size_range), "_", ifelse(size_range[2] == 0, NA, size_range))) %>%
      dplyr::select(1:3, 10, 4:9) -> out
    
    # write csv output
    if(write_csv == 1) {write_csv(out, file.path(csv_dir, paste0("abundance_size_", unique(out$size_grp), "_est.csv")))}
    # plot output
    if(plot == T) {
      # plots (separately by district, samp grp for cute sizing)
      # abundance plotted in millions
      out$abundance <- out$abundance / 1e6
      out$l95 <- out$l95 / 1e6; out$u95 <- out$u95 / 1e6
      ## ksh (only plots ksh1)
      out %>%
        filter(district == "KSH",
               !(eval(as.symbol(by)) %in% c("KSH2", "KSH3"))) %>%
        ggplot()+
        geom_point(aes(x = year, y = abundance))+
        geom_line(aes(x = year, y = abundance, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = "Abundance (millions)", title = "KSH1")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        theme(plot.title = element_text(hjust = 0.5)) -> ksh
      ggsave(file.path(plot_dir, paste0("ksh_abundance_size_", unique(out$size_grp), ".png")), plot = ksh, 
             width = 5, height = 3, units = "in")
      # kne (not kne4)  
      out %>%
        filter(district == "KNE",
               !(eval(as.symbol(by)) %in% c("KNE4"))) %>%
        ggplot()+
        geom_point(aes(x = year, y = abundance))+
        geom_line(aes(x = year, y = abundance, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = "Abundance (millions)")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne
      ggsave(file.path(plot_dir, paste0("kne_abundance_size_", unique(out$size_grp), ".png")), plot = kne, 
             width = 7, height = 6.5, units = "in")
      # wki   
      out %>%
        filter(district == "WKI") %>%
        ggplot()+
        geom_point(aes(x = year, y = abundance))+
        geom_line(aes(x = year, y = abundance, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = "Abundance (millions)", title = "WKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        theme(plot.title = element_text(hjust = 0.5)) -> wki
      ggsave(file.path(plot_dir, paste0("wki_abundance_size_", unique(out$size_grp), ".png")), plot = wki, width = 5, height = 3, units = "in")
      # eki   
      out %>%
        filter(district == "EKI") %>%
        ggplot()+
        geom_point(aes(x = year, y = abundance))+
        geom_line(aes(x = year, y = abundance, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = "Abundance (millions)", title = "EKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        theme(plot.title = element_text(hjust = 0.5)) -> eki
      ggsave(file.path(plot_dir, paste0("eki_abundance_size_", unique(out$size_grp), ".png")), plot = eki, width = 5, height = 3, units = "in")
      # yak  
      out %>%
        filter(district == "YAK") %>%
        ggplot()+
        geom_point(aes(x = year, y = abundance))+
        geom_line(aes(x = year, y = abundance, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = "Abundance (millions)")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak
      ggsave(file.path(plot_dir, paste0("yak_abundance_size_", unique(out$size_grp), ".png")), plot = yak, 
             width = 7, height = 6.5, units = "in")
      # plot all beds sampled in most recent survey
      yr <- max(out$year)
      out %>%
        filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by))))) %>%
        ggplot()+
        geom_point(aes(x = year, y = abundance, color = district))+
        geom_line(aes(x = year, y = abundance, color = district))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
        labs(x = NULL, y = "Abundance (millions)")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_color_brewer(palette = "Set2")+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_1
      ggsave(file.path(plot_dir, paste0("abundance_size_",yr, "_", unique(out$size_grp), ".png")), plot = yr_1, 
             width = 7, height = 8, units = "in")
    }
    
  }
  
  return(out)
    
}

# f_est_round_biomass() ----
# args: catch - cleaned catch data (output of f_load_clean_catch)
#       strata - cleaned strata data (output of f_load_strata)
#       spp - species race code, default = 74120, if not scallops make sure `write_csv` and `plot` are FALSE
#       by - stratifying variable, default = "bed_name", alternative = "bed_code"
#       units - default = "t" metric tonnes; also could be "lb" pounds
#       write_csv - T/F, default = T
#       plot - T/F, default = T
#       size_range - optional vector of size range, example: c(75, 100). Use NA for no min and/or max limit c(75, NA)
#       shad - cleaned strata data (output of f_load_clean_shad)
#       csv_dir - path to directory to store csv file
#       plot_dir - Path to directory to store plots
f_est_round_biomass <- function(catch, strata, spp = 74120, by = "bed_name", units = "t", write_csv = T, plot = T,
                            size_range, shad, csv_dir = NULL, plot_dir = NULL) {
  
  # create output directories
  if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # get bed area 
  strata %>% # strata loaded in custom functions
    ## compute bed area
    group_by(bed_code, bed_name, district) %>%
    summarise(area_nm2 = sum(sq_nmi_alb),
              n_stations = n()) %>% ungroup -> bed_area
  
  # rnd_biomass (by sample group) ----
  if(spp == 74120 & missing(size_range)) {
    
    catch %>%
      filter(rcode == spp) %>%
      left_join(bed_area, by = c("bed_code", "bed_name", "district")) %>%
      group_by_at(vars(c("year", "samp_grp", "district", by))) %>%
      summarise(cpue = mean(cpue_kg / 1000, na.rm = T),
                rnd_biomass = mean(cpue_kg / 1000, na.rm = T) * mean(area_nm2),
                se = sqrt(var(cpue_kg / 1000, na.rm = T) / n() * mean(area_nm2)^2),
                cv = se / rnd_biomass) %>% ungroup %>%
      # add lognormal confience intervals
      mutate(l95 = rnd_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
             u95 = rnd_biomass * exp(1.96 * sqrt(log(1 + cv^2)))) -> out
    # convert to lb
    if(units == "lb"){out <- mutate_at(out, 5:10, function(x){x / 0.000453592})}
    
    # write csv output
    if(write_csv == T) {write_csv(out, file.path(csv_dir, paste0("rnd_biomass_est_", units, ".csv")))}
    # plot output
    if(plot == T){
      # plots (separately by district, samp grp for cute sizing)
      ## ksh (only plots ksh1)
      out %>%
        filter(district == "KSH",
               !(eval(as.symbol(by)) %in% c("KSH2", "KSH3")),
               samp_grp == 1) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "KSH1")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> ksh1_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_ksh1_samp_grp1.png")), 
                       plot = ksh1_1, width = 5, height = 3, units = "in")
      out %>%
        filter(district == "KSH",
               !(eval(as.symbol(by)) %in% c("KSH2", "KSH3")),
               samp_grp == 2) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "KSH1")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> ksh1_2
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_ksh1_samp_grp2.png")), plot = ksh1_2, 
             width = 5, height = 3, units = "in")
      # kne (not kne4)  
      out %>%
        filter(district == "KNE",
               !(eval(as.symbol(by)) %in% c("KNE4")),
               samp_grp == 1) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_kne_samp_grp1.png")), plot = kne_1, 
             width = 7, height = 6.5, units = "in")
      out %>%
        filter(district == "KNE",
               !(eval(as.symbol(by)) %in% c("KNE4")),
               samp_grp == 2) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne_2
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_kne_samp_grp2.png")), plot = kne_2, 
             width = 7, height = 6.5, units = "in")
      # wki   
      out %>%
        filter(district == "WKI",
               samp_grp == 1) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "WKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> wki_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_wki_samp_grp1.png")), plot = wki_1, width = 5, height = 3, units = "in")
      out %>%
        filter(district == "WKI",
               samp_grp == 2) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "WKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> wki_2
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_wki_samp_grp2.png")), plot = wki_2, 
             width = 5, height = 3, units = "in")
      # eki   
      out %>%
        filter(district == "EKI",
               samp_grp == 1) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "EKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> eki_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_eki_samp_grp1.png")), plot = eki_1, width = 5, height = 3, units = "in")
      out %>%
        filter(district == "EKI",
               samp_grp == 2) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "EKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> eki_2
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_eki_samp_grp2.png")), plot = eki_2, 
             width = 5, height = 3, units = "in")
      # yak  
      out %>%
        filter(district == "YAK",
               samp_grp == 1) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_yak_samp_grp1.png")), plot = yak_1, 
             width = 7, height = 6.5, units = "in")
      out %>%
        filter(district == "YAK",
               samp_grp == 2) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak_2
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", units, "_yak_samp_grp2.png")), plot = yak_2, 
             width = 7, height = 6.5, units = "in")
      # plot all beds sampled in most recent surevy
      yr <- max(out$year)
      out %>%
        filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by)))),
               samp_grp == 1) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass, color = district))+
        geom_line(aes(x = year, y = rnd_biomass, color = district))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_brewer(palette = "Set2")+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", yr, "_", units, "_samp_grp1.png")), plot = yr_1, 
             width = 7, height = 8, units = "in")
      out %>%
        filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by)))),
               samp_grp == 2) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass, color = district))+
        geom_line(aes(x = year, y = rnd_biomass, color = district))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_brewer(palette = "Set2")+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_2
      ggsave(file.path(plot_dir, paste0("rnd_biomass_", yr, "_", units, "_samp_grp2.png")), plot = yr_2, 
             width = 7, height = 8, units = "in")
    }
    
  }
  
  # rnd_biomass (by specific size range) ----
  if(spp == 74120 & !missing(size_range)) {
    
    if(missing(shad)){stop("must include shad data")}
    size_range[is.na(size_range)] <- 0
    sizes <- c(0 + size_range[1], 1e6 - size_range[2]) 
    
    shad %>%
      filter(rcode == 74120,
             shell_height >= sizes[1], shell_height <= sizes[2]) %>%
      left_join(bed_area, by = c("bed_code", "bed_name", "district")) %>%
      group_by_at(vars(c("year", "samp_grp", "district", by, "tow", "area_nm2"))) %>%
      summarise(cpue_kg = sum(sample_factor) / mean(0.83*area_swept)) %>% ungroup %>%
      group_by_at(vars(c("year", "district", by))) %>%
      summarise(cpue = mean(cpue_kg / 1000, na.rm = T),
                rnd_biomass = mean(cpue_kg/ 1000, na.rm = T) * mean(area_nm2),
                se = sqrt(var(cpue_kg / 1000, na.rm = T) / n() * mean(area_nm2)^2),
                cv = se / rnd_biomass) %>% ungroup %>%
      # add lognormal confience intervals
      mutate(l95 = rnd_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
             u95 = rnd_biomass * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
      mutate(size_grp = paste0(ifelse(size_range[1] == 0, NA, size_range), "_", ifelse(size_range[2] == 0, NA, size_range))) %>%
      dplyr::select(1:3, 10, 4:9) -> out
    # convert to lb
    if(units == "lb"){out <- mutate_at(out, 5:10, function(x){x / 0.000453592})}
    
    # write csv output
    if(write_csv == 1) {write_csv(out, file.path(csv_dir, paste0("rnd_biomass_size_", units,"_", unique(out$size_grp),  "_", units, "_est.csv")))}
    # plot output
    if(plot == T) {
      # plots (separately by district, samp grp for cute sizing)
      ## ksh (only plots ksh1)
      out %>%
        filter(district == "KSH",
               !(eval(as.symbol(by)) %in% c("KSH2", "KSH3"))) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "KSH1")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> ksh
      ggsave(file.path(plot_dir, paste0("ksh_rnd_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = ksh, 
             width = 5, height = 3, units = "in")
      # kne (not kne4)  
      out %>%
        filter(district == "KNE",
               !(eval(as.symbol(by)) %in% c("KNE4"))) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne
      ggsave(file.path(plot_dir, paste0("kne_rnd_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = kne, 
             width = 7, height = 6.5, units = "in")
      # wki   
      out %>%
        filter(district == "WKI") %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "WKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> wki
      ggsave(file.path(plot_dir, paste0("wki_rnd_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = wki, width = 5, height = 3, units = "in")
      # eki   
      out %>%
        filter(district == "EKI") %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"), title = "EKI")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5)) -> eki
      ggsave(file.path(plot_dir, paste0("eki_rnd_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = eki, width = 5, height = 3, units = "in")
      # yak  
      out %>%
        filter(district == "YAK") %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass))+
        geom_line(aes(x = year, y = rnd_biomass, group = 1))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak
      ggsave(file.path(plot_dir, paste0("yak_rnd_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = yak, 
             width = 7, height = 6.5, units = "in")
      # plot all beds sampled in most recent survey
      yr <- max(out$year)
      out %>%
        filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by))))) %>%
        ggplot()+
        geom_point(aes(x = year, y = rnd_biomass, color = district))+
        geom_line(aes(x = year, y = rnd_biomass, color = district))+
        geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
        labs(x = NULL, y = paste0("Round Biomass (", units, ")"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_brewer(palette = "Set2")+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")+
        facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_1
      ggsave(file.path(plot_dir, paste0("rnd_biomass_size_",yr, "_", units,"_", unique(out$size_grp),  ".png")), plot = yr_1, 
             width = 7, height = 8, units = "in")
    }
    
  }
  
  return(out)
  
}


# f_est_meat_biomass() ----
# args: catch - cleaned catch data (output of f_load_clean_catch)
#       strata - cleaned strata data (output of f_load_strata)
#       shad - cleaned strata data (output of f_load_clean_shad)
#       shaw - cleaned strata data (output of f_load_clean_shaw)
#       by - stratifying variable, default = "bed_name", alternative = "bed_code"
#       units - default = "t" metric tonnes; also could be "lb" pounds
#       write_csv - T/F, default = T
#       plot - T/F, default = T
#       size_range - required vector of size range, example: c(75, 100). Use NA for no min and/or max limit c(75, NA)
#       csv_dir - path to directory to store csv file
#       plot_dir - Path to directory to store plots
f_est_meat_biomass <- function(catch, strata, shad, shaw, by = "bed_name", units = "t", write_csv = T, plot = T,
                               size_range = c(NA, NA), csv_dir = NULL, plot_dir = NULL) {
  
  # create output directories
  if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # get bed area 
  strata %>% # strata loaded in custom functions
    ## compute bed area
    group_by(bed_code, bed_name, district) %>%
    summarise(area_nm2 = sum(sq_nmi_alb),
              n_stations = n()) %>% ungroup -> bed_area
  
  # get sh:mw parameters
  if(missing(shaw)){stop("must include shad data")}
  shaw %>%
    group_by(year, district) %>% nest %>%
    mutate(par = purrr::map(data, function(data) {
     lm(log(meat_wt) ~ log(shell_height), data = data) -> lm
      coefficients(lm) %>% as.numeric() -> coef
      coef[1] <- exp(coef[1])# + (diag(vcov(lm))[1])/2)
      return(data.frame(alpha = coef[1], beta = coef[2]))
    })) %>% ungroup %>%
    transmute(year, district, par) %>% unnest(par) -> shmw_pars
  
  # compute mw biomass
  if(missing(shad)){stop("must include shad data")}
  size_range[is.na(size_range)] <- 0
  sizes <- c(0 + size_range[1], 1e6 - size_range[2]) 
  
  shad %>%
    filter(rcode == 74120,
           shell_height >= sizes[1], shell_height <= sizes[2]) %>%
    left_join(shmw_pars, by = c("year", "district")) %>%
    mutate(calc_wt_kg = alpha * shell_height^beta / 1000) %>%
    left_join(bed_area, by = c("bed_code", "bed_name", "district")) %>%
    group_by_at(vars(c("year", "samp_grp", "district", by, "tow", "area_nm2"))) %>%
    summarise(cpue_kg = sum(calc_wt_kg * sample_factor) / mean(0.83 * area_swept)) %>% ungroup %>%
    group_by_at(vars(c("year", "district", by))) %>%
    summarise(cpue = mean(cpue_kg / 1000, na.rm = T),
              meat_biomass = mean(cpue_kg/ 1000, na.rm = T) * mean(area_nm2),
              se = sqrt(var(cpue_kg / 1000, na.rm = T) / n() * mean(area_nm2)^2),
              cv = se / meat_biomass) %>% ungroup %>%
    # add lognormal confience intervals
    mutate(l95 = meat_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
           u95 = meat_biomass * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
    mutate(size_grp = paste0(ifelse(size_range[1] == 0, NA, size_range), "_", ifelse(size_range[2] == 0, NA, size_range))) %>%
    dplyr::select(1:3, 10, 4:9) -> out
  # convert to lb
  if(units == "lb"){out <- mutate_at(out, 5:10, function(x){x / 0.000453592})}
  # write csv output
  if(write_csv == 1) {write_csv(out, file.path(csv_dir, paste0("meat_biomass_size_", unique(out$size_grp),  "_", units, "_est.csv")))}
  # plot output
  if(plot == T) {
    # plots (separately by district, samp grp for cute sizing)
    ## ksh (only plots ksh1)
    out %>%
      filter(district == "KSH",
             !(eval(as.symbol(by)) %in% c("KSH2", "KSH3"))) %>%
      ggplot()+
      geom_point(aes(x = year, y = meat_biomass))+
      geom_line(aes(x = year, y = meat_biomass, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = paste0("Meat Biomass (", units, ")"), title = "KSH1")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma)+
      theme(plot.title = element_text(hjust = 0.5)) -> ksh
    ggsave(file.path(plot_dir, paste0("ksh_meat_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = ksh, 
           width = 5, height = 3, units = "in")
    # kne (not kne4)  
    out %>%
      filter(district == "KNE",
             !(eval(as.symbol(by)) %in% c("KNE4"))) %>%
      ggplot()+
      geom_point(aes(x = year, y = meat_biomass))+
      geom_line(aes(x = year, y = meat_biomass, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = paste0("Meat Biomass (", units, ")"))+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma)+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> kne
    ggsave(file.path(plot_dir, paste0("kne_meat_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = kne, 
           width = 7, height = 6.5, units = "in")
    # wki   
    out %>%
      filter(district == "WKI") %>%
      ggplot()+
      geom_point(aes(x = year, y = meat_biomass))+
      geom_line(aes(x = year, y = meat_biomass, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = paste0("Meat Biomass (", units, ")"), title = "WKI")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma)+
      theme(plot.title = element_text(hjust = 0.5)) -> wki
    ggsave(file.path(plot_dir, paste0("wki_meat_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = wki, width = 5, height = 3, units = "in")
    # eki   
    out %>%
      filter(district == "EKI") %>%
      ggplot()+
      geom_point(aes(x = year, y = meat_biomass))+
      geom_line(aes(x = year, y = meat_biomass, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = paste0("Meat Biomass (", units, ")"), title = "EKI")+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma)+
      theme(plot.title = element_text(hjust = 0.5)) -> eki
    ggsave(file.path(plot_dir, paste0("eki_meat_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = eki, width = 5, height = 3, units = "in")
    # yak  
    out %>%
      filter(district == "YAK") %>%
      ggplot()+
      geom_point(aes(x = year, y = meat_biomass))+
      geom_line(aes(x = year, y = meat_biomass, group = 1))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95), width = 0)+
      labs(x = NULL, y = paste0("Meat Biomass (", units, ")"))+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma)+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yak
    ggsave(file.path(plot_dir, paste0("yak_meat_biomass_size_", units,"_", unique(out$size_grp),  ".png")), plot = yak, 
           width = 7, height = 6.5, units = "in")
    # plot all beds sampled in most recent survey
    yr <- max(out$year)
    out %>%
      filter(eval(as.symbol(by)) %in% (out %>% filter(year == yr) %>% pull(eval(as.symbol(by))))) %>%
      ggplot()+
      geom_point(aes(x = year, y = meat_biomass, color = district))+
      geom_line(aes(x = year, y = meat_biomass, color = district))+
      geom_errorbar(aes(x = year, ymin = l95, ymax = u95, color = district), width = 0)+
      labs(x = NULL, y = paste0("Meat Biomass (", units, ")"))+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma)+
      scale_color_brewer(palette = "Set2")+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")+
      facet_wrap(~eval(as.symbol(by)), ncol = 2, dir = "v", ) -> yr_1
    ggsave(file.path(plot_dir, paste0("meat_biomass_size_",yr, "_", units,"_", unique(out$size_grp),  ".png")), plot = yr_1, 
           width = 7, height = 8, units = "in")
  }
  
  return(out)
  
}


# f_plot_shmw() ----
## plot shmw by district and year
# args: shaw - cleaned shaw data (output of f_load_clean_shaw)
#       all_districts - T/F create a plot for all districts, otherise function only plots districts surveyed in the last survey. Default = F.
#       years - Optional. Vector of survey years to plot. Example: c(2021, 2022, 2023) If specified, do not supply plot_dir, or plot file names will not make sense
#       plot_dir - Optional. Path to directory to store plots

f_plot_shmw <- function(shaw, all_districts = F, years = NULL, plot_dir = NULL) {
  
  # get sh:mw parameters
  if(missing(shaw)){stop("must include shad data")}
  shaw %>%
    group_by(year, district) %>% nest %>%
    mutate(par = purrr::map(data, function(data) {
      lm(log(meat_wt) ~ log(shell_height), data = data) -> lm
      coefficients(lm) %>% as.numeric() -> coef
      coef[1] <- exp(coef[1]) #+ diag(vcov(lm))[1]/2)
      return(data.frame(alpha = coef[1], beta = coef[2]))
    })) %>% ungroup %>%
    transmute(year, district, par) %>% unnest(par) -> shmw_pars
  
  # prepare data
  shaw %>%
    filter(!is.na(shell_height),
           !is.na(meat_wt)) %>%
    left_join(shmw_pars, by = join_by(year, district)) -> plot_dat
  
  if(!is.null(years)) {plot_dat %>% filter(year %in% years) -> plot_dat}
  
  # plot
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F)}
    
    # last survey
    plot_dat %>%
      filter(district %in% unique(plot_dat$district[plot_dat$year == max(plot_dat$year)])) %>%
      ggplot()+
      geom_point(aes(x = shell_height, y = meat_wt), color = "grey90")+
      geom_line(aes(x = shell_height, y = alpha*shell_height^beta, color = factor(year)))+
      scale_color_brewer(palette = "Set2")+
      facet_wrap(~district, ncol = 1)+
      labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL) -> p
    if(!is.null(plot_dir)) {
    ggsave(file.path(plot_dir, "shmw_last_survey_districts.png"), plot = p, 
           width = 5, height = 6.5, units = "in")
    }
    
    if(all_districts == T) {
      # all districts
      plot_dat %>%
        ggplot()+
        geom_point(aes(x = shell_height, y = meat_wt), color = "grey90")+
        geom_line(aes(x = shell_height, y = alpha*shell_height^beta, color = factor(year)))+
        scale_color_brewer(palette = "Set2")+
        facet_wrap(~district, ncol = 2)+
        labs(x = "Shell Height (mm)", y = "Meat Weight (g)", color = NULL) -> p
      if(!is.null(plot_dir)) {
      ggsave(file.path(plot_dir, "shmw_all_districts.png"), plot = p, 
             width = 8, height = 8, units = "in")
      }
    }
    
    return(p)
    
  }
  
# f_plot_sh_comp() ----

# args: shad - cleaned shad data (output of f_load_clean_shad)
#       by - column for bed, either "bed_code" or "bed_name". Default = "bed_name"
#       years - Optional. Vector of survey years to plot. Example: c(2021, 2022, 2023) If not specified, plots all years
#       bin_width - width of shell height bins in mm for plots, default = 5
#       plot_dir - Optional. Path to directory to store plots

f_plot_sh_comp <- function(shad, by = "bed_name", years = NULL, bin_width = 5, plot_dir) {
  # prep data ----
  shad %>%
    rename(col = eval(by)) %>%
    filter(!col %in% c("KSH2", "KSH3", "KNE4")) %>%
    mutate(sh_bin = floor(shell_height / bin_width) * bin_width) %>%
    group_by(year, sh_bin, district, col) %>%
    summarise(count = sum(sample_factor)) %>% ungroup -> plot_dat
  
  if(!is.null(years)){plot_dat <- filter(plot_dat, year %in% years)}

  # plots ----
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F)}
  ## ksh
  plot_dat %>% 
    filter(district == "KSH", !is.na(count)) %>% #print(n = 1000)
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T)+
    facet_wrap(~year, ncol = 1, dir = "v")+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL, title = "KSH")+
    theme(panel.spacing = unit(0, "lines"),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.background = element_blank()) -> ksh
  ggsave(file.path(plot_dir, "./sh_comp_ksh.png"), plot = ksh, height = 6, width = 4, units = "in")
  ## kne, by bed
  plot_dat %>% 
    filter(district == "KNE", !is.na(count)) %>% #print(n = 1000)
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    facet_grid(rows = vars(year), cols = vars(col))+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL)+
    theme(panel.spacing = unit(0, "lines"),
          #plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          #strip.text.x = element_blank(),
          panel.background = element_blank()) -> kne
  ggsave(file.path(plot_dir, "./sh_comp_kne_bed.png"), plot = kne, height = 6, width = 7, units = "in")
  ## kne, by district
  plot_dat %>% 
    filter(district == "KNE", !is.na(count)) %>% group_by(year, district, sh_bin) %>% summarise(count = sum(count)) %>%
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    geom_text_npc(aes(npcx = "right", npcy = 0.9, label = year), check_overlap = T)+
    facet_wrap(~year, ncol = 1, dir = "v")+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL, title = "KNE")+
    theme(panel.spacing = unit(0, "lines"),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.background = element_blank()) -> kne
  ggsave(file.path(plot_dir, "./sh_comp_kne.png"), plot = kne, height = 5, width = 4, units = "in")
  ## wki
  plot_dat %>% 
    filter(district == "WKI", !is.na(count)) %>% #print(n = 1000)
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T)+
    facet_wrap(~year, ncol = 1, dir = "v")+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL, title = "WKI")+
    theme(panel.spacing = unit(0, "lines"),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.background = element_blank()) -> wki
  ggsave(file.path(plot_dir, "./sh_comp_wki.png"), plot = wki, height = 6, width = 4, units = "in")
  ## eki
  plot_dat %>% 
    filter(district == "EKI", !is.na(count)) %>% #print(n = 1000)
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T)+
    facet_wrap(~year, ncol = 1, dir = "v")+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL, title = "EKI")+
    theme(panel.spacing = unit(0, "lines"),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.background = element_blank()) -> eki
  ggsave(file.path(plot_dir, "./sh_comp_eki.png"), plot = eki, height = 6, width = 4, units = "in")
  ## yak, by bed
  plot_dat %>% 
    filter(district == "YAK", !is.na(count)) %>% #print(n = 1000)
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    facet_grid(rows = vars(year), cols = vars(col))+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL)+
    theme(panel.spacing = unit(0, "lines"),
          #plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          #strip.text.x = element_blank(),
          panel.background = element_blank()) -> yak
  ggsave(file.path(plot_dir, "./sh_comp_yak_bed.png"), plot = yak, height = 6, width = 7, units = "in")
  ## yak, by district
  plot_dat %>% 
    filter(district == "YAK", !is.na(count)) %>% group_by(year, district, sh_bin) %>% summarise(count = sum(count)) %>%
    ggplot()+
    geom_bar(aes(x = sh_bin, y = count), stat = "identity", width = bin_width, fill = "grey70", color ="grey40")+
    geom_text_npc(aes(npcx = "left", npcy = 0.9, label = year), check_overlap = T)+
    facet_wrap(~year, ncol = 1, dir = "v")+
    scale_y_continuous(labels = scales::comma)+
    labs(x = "Shell Height (mm)", y = "Frequency", fill = NULL, title = "YAK")+
    theme(panel.spacing = unit(0, "lines"),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          panel.background = element_blank()) -> yak
  ggsave(file.path(plot_dir, "./sh_comp_yak.png"), plot = yak, height = 6, width = 5, units = "in")
  
}

# f_gonad_condition() ----
# args: shaw - cleaned shaw data (output of f_load_clean_shaw)
#       write_csv - T/F, default = T
#       plot - T/F, default = T
#       csv_dir - path to directory to store csv file
#       plot_dir - path to directory to store plots
f_gonad_condition <- function(shaw, write_csv = T, csv_dir = NULL, plot = T, plot_dir = NULL) {
  
  # percentage by year, and condition code
  shaw %>%
    filter(district != "KAM") %>%
    group_by(district) %>%
    nest() %>% #pull(data) %>% .[[4]] -> data
    mutate(gonad = purrr::map2(data, district, function(data, district){
      
      # table
      data %>%
        filter(!is.na(gonad)) %>% 
        count(year, gonad) %>%
        group_by(year) %>%
        mutate(total = sum(n)) %>% ungroup %>%
        mutate(perc = n / total * 100) %>%
        arrange(gonad) %>%
        transmute(year, gonad, perc) %>%
        pivot_wider(names_from = gonad, values_from = perc) %>%
        replace(is.na(.), 0) -> out
      names <- c("year", "immature", "empty", "init_recovery", "filling", "full", "cannot_determine")
      names(out) <- names[1:ncol(out)]

      if(plot == T) {
        # plot
        out %>%
          pivot_longer(2:ncol(.), names_to = "gonad", values_to = "perc") %>%
          mutate(gonad = str_to_title(gsub("_", " ", gonad)),
                 gonad = factor(gonad, levels = c("Immature", "Empty", "Init Recovery", "Filling", "Full", "Cannot Determine"))) %>%
          ggplot()+
          geom_bar(aes(x = factor(year), y = perc, fill = gonad), stat = "identity")+
          labs(x = NULL, y = "Percent Sampled", fill = NULL)+
          scale_fill_brewer(palette = "Set3") -> p
        ggsave(file.path(plot_dir, paste0(district, "_gonad_condition.png")), plot = p, height = 3, width = 5, units = "in")
      }
      
    return(out)
      
    })) %>%
    transmute(district, gonad) %>%
    unnest(gonad) %>%
    replace(is.na(.), 0) %>% ungroup -> out
  if(write_csv == T) {write_csv(out, file.path(csv_dir, "gonad_condition_percentage.csv"))}
 return(out) 
}

# f_meat_condition() ----
# args: shaw - cleaned shaw data (output of f_load_clean_shaw)
#       write_csv - T/F, default = T
#       plot - T/F, default = F
#       csv_dir - path to directory to store csv file
#       plot_dir - path to directory to store plots
f_meat_condition <- function(shaw, write_csv = T, csv_dir = NULL, plot = F, plot_dir = NULL){
  
  # percentage by year, and condition code
  shaw %>%
    filter(district != "KAM") %>%
    group_by(district) %>%
    nest() %>% #pull(data) %>% .[[1]] -> data
    mutate(meat_cond = purrr::map2(data, district, function(data, district){
      
      # table
      data %>%
        filter(!is.na(meat_condition)) %>% 
        count(year, meat_condition) %>%
        group_by(year) %>%
        mutate(total = sum(n)) %>% ungroup %>%
        mutate(perc = n / total * 100) %>%
        transmute(year, meat_condition, perc) %>%
        pivot_wider(names_from = meat_condition, values_from = perc) %>%
        replace(is.na(.), 0) -> out
      names <- c("year", "good", "weak")
      names(out) <- names[1:ncol(out)]
      
      if(plot == T) {
        
        # plot
        out %>%
          pivot_longer(2:ncol(.), names_to = "mc", values_to = "perc") %>%
          mutate(gonad = str_to_title(mc),
                 gonad = factor(gonad, levels = c("Good", "Weak"))) %>%
          ggplot()+
          geom_bar(aes(x = factor(year), y = perc, fill = mc), stat = "identity")+
          labs(x = NULL, y = "Percent Sampled", fill = NULL)+
          scale_fill_brewer(palette = "Set3") -> p
        ggsave(file.path(plot_dir, paste0(district, "_meat_condition.png")), plot = p, height = 3, width = 5, units = "in")
        
        
      }
      
      return(out)
      
    })) %>%
    transmute(district, meat_cond) %>%
    unnest(meat_cond) %>%
    replace(is.na(.), 0) %>% ungroup -> out
  if(write_csv == T) {write_csv(out, file.path(csv_dir, "meat_condition_percentage.csv"))}
  return(out) 
  
  
}

# f_shell_worm() ----
# args: shaw - cleaned shaw data (output of f_load_clean_shaw)
#       write_csv - T/F, default = T
#       plot - T/F, default = T
#       csv_dir - path to directory to store csv file
#       plot_dir - path to directory to store plots
f_shell_worm <- function(shaw, write_csv = T, csv_dir = NULL, plot = F, plot_dir = NULL) {
  
  # percentage by year, and condition code
  shaw %>%
    filter(district != "KAM") %>%
    group_by(district) %>%
    nest() %>% #pull(data) %>% .[[1]] -> data
    mutate(shell_worm = purrr::map2(data, district, function(data, district){
      
      # table
      data %>%
        filter(shell_worm %in% 10:13) %>% 
        count(year, shell_worm) %>%
        group_by(year) %>%
        mutate(total = sum(n)) %>% ungroup %>%
        mutate(perc = n / total * 100) %>%
        transmute(year, shell_worm, perc) %>%
        pivot_wider(names_from = shell_worm, values_from = perc) %>%
        replace(is.na(.), 0) -> out
      names <- c("year", "none", "mild", "moderate", "advanced")
      names(out) <- names[1:ncol(out)]

      
      if(plot == T) {
        
        # plot
        out %>%
          pivot_longer(2:ncol(.), names_to = "shell_worm", values_to = "perc") %>%
          mutate(shell_worm = str_to_title(gsub("_", " ", shell_worm)),
                 shell_worm = factor(shell_worm, levels = c("None", "Mild", "Moderate", "Advanced"))) %>%
          ggplot()+
          geom_bar(aes(x = factor(year), y = perc, fill = shell_worm), stat = "identity")+
          labs(x = NULL, y = "Percent Sampled", fill = NULL)+
          scale_fill_brewer(palette = "Set3") -> p
        ggsave(file.path(plot_dir, paste0(district, "_shell_worm.png")), plot = p, height = 3, width = 5, units = "in")
        
        
      }
      
      return(out)
      
    })) %>%
    transmute(district, shell_worm) %>%
    unnest(shell_worm) %>%
    replace(is.na(.), 0) %>% ungroup -> out
  if(write_csv == T) {write_csv(out, file.path(csv_dir, "shell_worm_percentage.csv"))}
}

# f_mud_blister() ----
# args: shaw - cleaned shaw data (output of f_load_clean_shaw)
#       write_csv - T/F, default = T
#       plot - T/F, default = T
#       csv_dir - path to directory to store csv file
#       plot_dir - path to directory to store plots
f_mud_blister <- function(shaw, write_csv = T, csv_dir = NULL, plot = F, plot_dir = NULL) {
  
  # percentage by year, and condition code
  shaw %>%
    filter(district != "KAM") %>%
    group_by(district) %>%
    nest() %>% #pull(data) %>% .[[1]] -> data
    mutate(mud_blister = purrr::map2(data, district, function(data, district){
      
      # table
      data %>%
        filter(mud_blister %in% 10:13) %>% 
        count(year, mud_blister) %>%
        group_by(year) %>%
        mutate(total = sum(n)) %>% ungroup %>%
        mutate(perc = n / total * 100) %>%
        transmute(year, mud_blister, perc) %>%
        pivot_wider(names_from = mud_blister, values_from = perc) %>%
        replace(is.na(.), 0) -> out
      names <- c("year", "none", "mild", "moderate", "advanced")
      names(out) <- names[1:ncol(out)]
      
      if(plot == T) {
        # plot
        out %>%
          pivot_longer(2:ncol(.), names_to = "mud_blister", values_to = "perc") %>%
          mutate(mud_blister = str_to_title(gsub("_", " ", mud_blister)),
                 mud_blister = factor(mud_blister, levels = c("None", "Mild", "Moderate", "Advanced"))) %>%
          ggplot()+
          geom_bar(aes(x = factor(year), y = perc, fill = mud_blister), stat = "identity")+
          labs(x = NULL, y = "Percent Sampled", fill = NULL)+
          scale_fill_brewer(palette = "Set3") -> p
        ggsave(file.path(plot_dir, paste0(district, "_mud_blister.png")), plot = p, height = 3, width = 5, units = "in")
      }
      
      return(out)
    })) %>%
    transmute(district, mud_blister) %>%
    unnest(mud_blister) %>%
    replace(is.na(.), 0) %>% ungroup -> out
  if(write_csv == T){write_csv(out, file.path(csv_dir, "mud_blister_percentage.csv"))}
}
