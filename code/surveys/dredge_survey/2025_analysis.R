# notes ----
# 2025 survey analysis
# tyler jackson

# load -----

source("./code/surveys/dredge_survey/dredge_survey_functions.R")

# data ----

tows <- f_load_clean_logbook()
catch <- f_load_clean_catch(tows = tows, spp = 74120)
shaw <- f_load_clean_shaw(tows = f_load_clean_logbook())
shad <- f_load_clean_shad(catch = catch)
strata <- f_load_strata()

# estimate abundance and biomass ----

## abundance
f_est_abundance(catch, strata, write_csv = T, plot = T,
                csv_dir = "./output/dredge_survey/2025", plot_dir = "./figures/dredge_survey/2025")

# plot by district
read_csv("./output/dredge_survey/2025/abundance_est.csv") %>%
  group_by(year, samp_grp, district) %>%
  summarise(abundance = sum(abundance),
            se = sqrt(sum(se^2)),
            cv = se / abundance,
            l95 = abundance * exp(-1.96 * sqrt(log(1 + cv^2))),
            u95 = abundance * exp(1.96 * sqrt(log(1 + cv^2)))) %T>%
  write_csv("./output/dredge_survey/2025/abundance_est_district.csv") %>% ungroup %>%
  nest_by(district, .keep = T) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
  mutate(plot = purrr::map(data, function(data) {
    data %>%
      filter(samp_grp == 1) %>%
      ggplot()+
      geom_line(aes(x = year, y = abundance / 1e6, group = 1))+
      geom_point(aes(x = year, y = abundance / 1e6))+
      geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = NULL, y = "Abundance (millions)") -> large
    ggsave(paste0("./figures/dredge_survey/2025/abundance_est_exploitable_", unique(data$district), ".png"),
           plot = large, width = 5, height = 3, units = "in")
    data %>%
      filter(samp_grp == 2) %>%
      ggplot()+
      geom_line(aes(x = year, y = abundance / 1e6, group = 2))+
      geom_point(aes(x = year, y = abundance / 1e6))+
      geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = NULL, y = "Abundance (millions)") -> small
    ggsave(paste0("./figures/dredge_survey/2025/abundance_est_small_", unique(data$district), ".png"),
           plot = small, width = 5, height = 3, units = "in")
      
  })) %>%
    dplyr::select(-plot, -district) %>% 
    unnest(data) %>%
    filter(samp_grp == 1) %>%
  ggplot()+
  geom_line(aes(x = year, y = abundance / 1e6, group = 1))+
  geom_point(aes(x = year, y = abundance / 1e6))+
  geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = NULL, y = "Abundance (millions)")+
  facet_wrap(~district, ncol = 2) -> x
ggsave("./figures/dredge_survey/2025/abundance_est_exploitable.png",
       plot = x, width = 7, height = 8, units = "in")

read_csv("./output/dredge_survey/2025/abundance_est.csv") %>%
  group_by(year, samp_grp, district) %>%
  summarise(abundance = sum(abundance),
            se = sqrt(sum(se^2)),
            cv = se / abundance,
            l95 = abundance * exp(-1.96 * sqrt(log(1 + cv^2))),
            u95 = abundance * exp(1.96 * sqrt(log(1 + cv^2))))  %>%
  filter(samp_grp == 2) %>%
  ggplot()+
  geom_line(aes(x = year, y = abundance / 1e6, group = 1))+
  geom_point(aes(x = year, y = abundance / 1e6))+
  geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = NULL, y = "Abundance (millions)")+
  facet_wrap(~district, ncol = 2) -> x
ggsave("./figures/dredge_survey/2025/abundance_est_small.png",
       plot = x, width = 7, height = 8, units = "in")


## round biomass
f_est_round_biomass(catch, strata, write_csv = T, plot = T, units = "t",
                    csv_dir = "./output/dredge_survey/2025", plot_dir = "./figures/dredge_survey/2025")

# plot by district
read_csv("./output/dredge_survey/2025/rnd_biomass_est_t.csv") %>%
  group_by(year, samp_grp, district) %>%
  summarise(rnd_biomass = sum(rnd_biomass),
            se = sqrt(sum(se^2)),
            cv = se / rnd_biomass,
            l95 = rnd_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
            u95 = rnd_biomass * exp(1.96 * sqrt(log(1 + cv^2)))) %T>%
  write_csv("./output/dredge_survey/2025/rnd_biomass_est_t_district.csv") %>% ungroup %>%
  nest_by(district, .keep = T) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
  mutate(plot = purrr::map(data, function(data) {
    data %>%
      filter(samp_grp == 1) %>%
      ggplot()+
      geom_line(aes(x = year, y = rnd_biomass / 1e6, group = 1))+
      geom_point(aes(x = year, y = rnd_biomass / 1e6))+
      geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = NULL, y = "Round Biomass (t)") -> large
    ggsave(paste0("./figures/dredge_survey/2025/rnd_biomass_est_t_exploitable_", unique(data$district), ".png"),
           plot = large, width = 5, height = 3, units = "in")
    data %>%
      filter(samp_grp == 2) %>%
      ggplot()+
      geom_line(aes(x = year, y = rnd_biomass / 1e6, group = 2))+
      geom_point(aes(x = year, y = rnd_biomass / 1e6))+
      geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
      scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(limits = c(0, NA))+
      labs(x = NULL, y = "Round Biomass (t)") -> small
    ggsave(paste0("./figures/dredge_survey/2025/rnd_biomass_est_t_small_", unique(data$district), ".png"),
           plot = small, width = 5, height = 3, units = "in")
    
  })) %>%
  dplyr::select(-plot, -district) %>% 
  unnest(data) %>%
  filter(samp_grp == 1) %>%
  ggplot()+
  geom_line(aes(x = year, y = rnd_biomass / 1e6, group = 1))+
  geom_point(aes(x = year, y = rnd_biomass / 1e6))+
  geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = NULL, y = "Round Biomass (t)")+
  facet_wrap(~district, ncol = 2) -> x
ggsave("./figures/dredge_survey/2025/rnd_biomass_est_t_exploitable.png",
       plot = x, width = 7, height = 8, units = "in")


read_csv("./output/dredge_survey/2025/rnd_biomass_est_t.csv") %>%
  group_by(year, samp_grp, district) %>%
  summarise(rnd_biomass = sum(rnd_biomass),
            se = sqrt(sum(se^2)),
            cv = se / rnd_biomass,
            l95 = rnd_biomass * exp(-1.96 * sqrt(log(1 + cv^2))),
            u95 = rnd_biomass * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
  filter(samp_grp == 2) %>%
  ggplot()+
  geom_line(aes(x = year, y = rnd_biomass / 1e6, group = 1))+
  geom_point(aes(x = year, y = rnd_biomass / 1e6))+
  geom_errorbar(aes(x = year, ymin = l95 / 1e6, ymax = u95 / 1e6), width = 0)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_y_continuous(limits = c(0, NA))+
  labs(x = NULL, y = "Round Biomass (t)")+
  facet_wrap(~district, ncol = 2) -> x
ggsave("./figures/dredge_survey/2025/rnd_biomass_est_t_small.png",
       plot = x, width = 7, height = 8, units = "in")


## meat biomass
f_est_meat_biomass(catch, strata, shad, shaw, write_csv = T, plot = T, units = "t", size_range = c(100, NA),
                   csv_dir = "./output/dredge_survey/2025", plot_dir = "./figures/dredge_survey/2025")

# shell height distribution ----

f_plot_sh_comp(shad, plot_dir = "./figures/dredge_survey/2025")


# shell height / meat weight plot ----

f_plot_shmw(shaw, all_districts = F, plot_dir = "./figures/dredge_survey/2025")

shaw %>%
  filter(district == "KSH") %>%
  ggplot()+
  #geom_point(aes(x = shell_height, y = meat_wt, color = factor(year)))+
  geom_point(aes(x = shell_height, y = whole_wt, color = factor(year)))

# gonad condition table ----

f_gonad_condition(shaw, csv_dir = "./output/dredge_survey/2025", plot_dir = "./figures/dredge_survey/2025")

# pathologies ----

# meat condition
f_meat_condition(shaw, csv_dir = "./output/dredge_survey/2025")

# shell worm
f_shell_worm(shaw, csv_dir = "./output/dredge_survey/2025")

# mud blister
f_mud_blister(shaw, csv_dir = "./output/dredge_survey/2025")







