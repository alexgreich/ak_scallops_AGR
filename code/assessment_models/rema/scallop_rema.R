# notes ----

# tier 4-ish assessment using rema
# tyler jackson
# 1/19/2024

# load ----

library(rema)

source("./code/observer/scallop_obs_functions.R")

# change a rema function for incorporating extra cv
tidy_extra_cv <- function (tidy_rema, save = FALSE, path = NULL, alpha_ci = 0.05) 
{
  #alpha_ci = 0.05
  if (nrow(tidy_rema$parameter_estimates %>% filter(parameter %in% 
                                                    c("extra_biomass_cv", "extra_cpue_cv"))) == 0) {
    stop("Additional observation error for the biomass and/or CPUE index is not estimated in this model.")
  }
  if (nrow(tidy_rema$parameter_estimates %>% filter(parameter %in% 
                                                    c("extra_biomass_cv"))) > 1) {
    stop("The new tidy_extra_cv() and plot_extra_cv() functions only work when the additional estimated CV is shared across all biomass or CPUE strata.\n\n         If you require this functionality, please file an issue at https://github.com/afsc-assessments/rema/issues")
  }
  # if (nrow(tidy_rema$parameter_estimates %>% filter(parameter %in% 
  #                                                   c("extra_cpue_cv"))) > 1) {
  #   stop("The new tidy_extra_cv() and plot_extra_cv() functions only work when the additional estimated CV is shared across all biomass or CPUE strata.\n\n         If you require this functionality, please file an issue at https://github.com/afsc-assessments/rema/issues")
  # }
  if (nrow(tidy_rema$parameter_estimates %>% filter(parameter %in% 
                                                    c("extra_biomass_cv"))) == 1) {
    tidy_rema$biomass_by_strata <- tidy_rema$biomass_by_strata %>% 
      dplyr::mutate(extra_cv = tidy_rema$parameter_estimates %>% 
                      filter(parameter == "extra_biomass_cv") %>% pull(estimate), 
                    tot_sd_log_obs = ifelse(obs > 0, sqrt(log(obs_cv^2 + 
                                                                extra_cv^2 + 1)), NA), tot_obs_lci = exp(log_obs - 
                                                                                                           qnorm(1 - alpha_ci/2) * tot_sd_log_obs), tot_obs_uci = exp(log_obs + 
                                                                                                                                                                        qnorm(1 - alpha_ci/2) * tot_sd_log_obs))
  }
  if (nrow(tidy_rema$parameter_estimates %>% filter(parameter %in% 
                                                    c("extra_cpue_cv"))) == 1) {
    tidy_rema$cpue_by_strata <- tidy_rema$cpue_by_strata %>% 
      dplyr::mutate(extra_cv = tidy_rema$parameter_estimates %>% 
                      filter(parameter == "extra_cpue_cv") %>% pull(estimate), 
                    tot_sd_log_obs = ifelse(obs > 0, sqrt(log(obs_cv^2 + 
                                                                extra_cv^2 + 1)), NA), tot_obs_lci = exp(log_obs - 
                                                                                                           qnorm(1 - alpha_ci/2) * tot_sd_log_obs), tot_obs_uci = exp(log_obs + 
                                                                                                                                                                        qnorm(1 - alpha_ci/2) * tot_sd_log_obs))
  }
  if (nrow(tidy_rema$parameter_estimates %>% filter(parameter %in% 
                                                    c("extra_cpue_cv"))) > 1) {
    tidy_rema$cpue_by_strata <-  tidy_rema$parameter_estimates %>% filter(parameter %in% 
                                                                            c("extra_cpue_cv")) %>%
      mutate(strata = unique(tidy_rema$cpue_by_strata$strata)) %>%
      transmute(strata, extra_cv = estimate) %>%
      left_join(tidy_rema$cpue_by_strata, .) %>%
      mutate(tot_sd_log_obs = ifelse(obs > 0, sqrt(log(obs_cv^2 + extra_cv^2 + 1)), NA), 
             tot_obs_lci = exp(log_obs - qnorm(1 - alpha_ci/2) * tot_sd_log_obs), 
             tot_obs_uci = exp(log_obs + qnorm(1 - alpha_ci/2) * tot_sd_log_obs))
  }
  return(tidy_rema)
}

# data ----

# standardized cpue index 2009 - present
index <- read_csv("./output/observer/2024/assessment_cpue_std_index.csv")

# dredge survey biomass
biomass_out <- read_csv("./output/dredge_survey/2023/2023_abundance_rnd_biomass_by_bed.csv")

# compute biomass time series by district ----

# trim to data of interest
biomass_out %>%
  filter(samp_grp == 1,
         !(bed_name %in% c("KSH2", "KSH3", "KNE4"))) %>%
  transmute(year, district, bed_name, biomass, cv = cv_biomass) %>% 
  # combine YAK and EKI
  # remove 2016 YAK, only EK1 in EKI was surveyed
  mutate(district = ifelse(district == "EKI", "YAK", district)) %>%
  filter(!(year == 2016 & district == "YAK")) %>%
  group_by(year, district, bed_name) %>%
  summarise(b = sum(biomass), 
            cv = sqrt(sum((cv * biomass)^2)) / b) %>% 
  rename(biomass = b) %>% ungroup -> b_tmp

# estimate biomass for kne
kne_lm <- glm(log(biomass) ~ factor(year) + bed_name, weights = 1/cv, data = b_tmp, subset = district == "KNE")
# fill in time series
b_tmp %>%
  filter(district == "KNE") %>%
  right_join(expand_grid(year = unique(.$year),
                                district = "KNE", 
                                bed_name = unique(.$bed_name))) %>%
  # add lm predictions
  mutate(fit = predict.glm(kne_lm, ., se = T)$fit,
         se = predict.glm(kne_lm, ., se = T)$se,
         biomass = ifelse(is.na(biomass), exp(fit + se^2/2), biomass),
         cv = ifelse(is.na(cv), se / fit, cv)) %>%
  # sum across beds
  group_by(year, district) %>%
  summarise(b = sum(biomass),
            cv = sum(biomass * cv) / b) -> kne_b
  
# estimate biomass for yak
yak_lm <- glm(log(biomass) ~ factor(year) + bed_name, weights = 1/cv, data = b_tmp, subset = district == "YAK")
# fill in time series
b_tmp %>%
  filter(district == "YAK") %>%
  right_join(expand_grid(year = unique(.$year),
                         district = "YAK", 
                         bed_name = unique(.$bed_name))) %>%
  # add lm predictions
  mutate(fit = predict.glm(yak_lm, ., se = T)$fit,
         se = predict.glm(yak_lm, ., se = T)$se,
         biomass = ifelse(is.na(biomass), exp(fit + se^2/2), biomass),
         cv = ifelse(is.na(cv), se / fit, cv)) %>%
  # sum across beds
  group_by(year, district) %>%
  summarise(b = sum(biomass),
            cv = sqrt(sum(biomass * cv)^2) / b) -> yak_b
  
# bind together biomass time series
b_tmp %>%
  group_by(year, district) %>%
  summarise(b = sum(biomass),
            cv = sum(biomass * cv) / b) %>% ungroup %>%
  # remove yak and kne, no kamishak
  filter(!(district %in% c("KNE", "YAK", "KAM"))) %>%
  # add in filled in yak and kne
  bind_rows(yak_b) %>%
  bind_rows(kne_b) %>%
  # prep rema data, biomass in tonnes
  transmute(year, strata = district, biomass = b * 0.000453592, cv) %>%
  arrange(strata, year) -> biomass
write_csv(biomass, "./output/models/rema/2024/survey_biomass_est.csv")
  
# index time series by district ----

# 2009 - present time series
index %>%
  transmute(year, strata = district, cpue = index, cv = se / index) %>%
  arrange(strata, year) %>%
  filter(!is.na(cpue)) -> cpue_2009_present


# rema model fit ----

# model 24.0 - single process error among strata
prepare_rema_input(model_name = "24.0",
                   start_year = 2009,
                   end_year = 2023,
                   biomass_dat = biomass, 
                   cpue_dat = cpue_2009_present,
                   multi_survey = 1,
                   sum_cpue_index = F, 
                   wt_cpue = 1,
                   extra_cpue_cv = list(assumption = 'extra_cv'),
                   PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1))) %>%
  fit_rema() -> rema_24.0

# clean output
out_24.0 <- tidy_extra_cv(tidy_rema(rema_24.0))

# model 24.1 - unique process error among strata
prepare_rema_input(model_name = "24.1",
                   start_year = 2009,
                   end_year = 2023,
                   biomass_dat = biomass, 
                   cpue_dat = cpue_2009_present,
                   multi_survey = 1,
                   sum_cpue_index = F, 
                   wt_biomass = 1,
                   wt_cpue = 1,
                   PE_options = list(penalty_options  = "normal_prior",
                                     penalty_values = c(c(0, 1), c(0, 1), c(-1.64, 0.38), c(0, 1))),
                   extra_cpue_cv = list(assumption = 'extra_cv') ) %>% 
  fit_rema() -> rema_24.1
out_24.1 <- tidy_extra_cv(tidy_rema(rema_24.1))

# model 24.2 - unique process error among strata
prepare_rema_input(model_name = "24.2",
                   start_year = 2009,
                   end_year = 2023,
                   biomass_dat = biomass, 
                   cpue_dat = cpue_2009_present,
                   multi_survey = 1,
                   sum_cpue_index = F, 
                   wt_biomass = 1,
                   wt_cpue = 0.5,
                   PE_options = list(penalty_options  = "normal_prior",
                                     penalty_values = c(c(0, 1), c(0, 1), c(-1.64, 0.38), c(0, 1))),
                   extra_cpue_cv = list(assumption = 'extra_cv') ) %>% 
  fit_rema() -> rema_24.2
out_24.2 <- tidy_extra_cv(tidy_rema(rema_24.2))

## estimate extra cv by district
prepare_rema_input(model_name = "24.3",
                   start_year = 2009,
                   end_year = 2023,
                   biomass_dat = biomass, 
                   cpue_dat = cpue_2009_present,
                   multi_survey = 1,
                   sum_cpue_index = F, 
                   wt_biomass = 1,
                   wt_cpue = 0.5,
                   PE_options = list(penalty_options  = "normal_prior",
                                     penalty_values = c(c(0, 1), c(0, 1), c(-1.64, 0.38), c(0, 1))),
                   extra_cpue_cv = list(assumption = 'extra_cv',
                                        pointer_extra_cpue_cv = c(1, 2, 3, 4) )) %>%
  fit_rema() -> rema_24.3

# clean output
out_24.3 <- tidy_extra_cv(tidy_rema(rema_24.3))

# rema model residual diagnostics ----

osa_24.0 <- get_osa_residuals(rema_24.0)
osa_24.1 <- get_osa_residuals(rema_24.1)
osa_24.2 <- get_osa_residuals(rema_24.2)

## cpue fits
osa_24.0$residuals$cpue %>%
  mutate(model = "Single PE") %>%
  bind_rows(osa_24.1$residuals$cpue %>%
              mutate(model = "PE by District")) %>%
  bind_rows(osa_24.2$residuals$cpue %>%
              mutate(model = "Extra cv by District")) -> cpue_res
## qq plot
ggplot(data = cpue_res, aes(sample = residual, color = model)) + 
  stat_qq() + 
  stat_qq_line(linetype = 2) + 
  facet_wrap(~strata) + 
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", color = NULL) -> qq_cpue
## res vs fitted
ggplot(data = cpue_res, aes(x = log_pred, y = residual, color = model)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) + 
  geom_point() +
  facet_wrap(~strata, scales = "free_x") + 
  labs(x = "Fitted (log-scale)", y = "Residual", color = NULL) -> resfit_cpue

## biomass fits
osa_24.0$residuals$biomass %>%
  mutate(model = "Single PE") %>%
  bind_rows(osa_24.1$residuals$biomass %>%
              mutate(model = "PE by District")) -> biomass_res
## qq plot
ggplot(data = biomass_res, aes(sample = residual, color = model)) + 
  stat_qq() + 
  stat_qq_line(linetype = 2) + 
  facet_wrap(~strata) + 
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", color = NULL) -> qq_biomass
## res vs fitted
ggplot(data = biomass_res, aes(x = log_pred, y = residual, color = model)) + 
  geom_hline(yintercept = 0, colour = "grey", size = 1) + 
  geom_point() +
  facet_wrap(~strata, scales = "free_x") + 
  labs(x = "Fitted (log-scale)", y = "Residual", color = NULL) -> resfit_biomass

# compare models ----

rema_comp <- compare_rema_models(list(rema_24.0, rema_24.1, rema_24.2, rema_24.3))

# save aic table
rema_comp$aic %>%
  write_csv("./output/models/rema/2024/lik_tab.csv")

# parameter table ----

# model 24.0
out_24.0$parameter_estimates[,c(1:4)] %>%
  mutate(par = c("$\\sigma_{PE}^2$", "$q_{\\text{KNE}}$", "$q_{\\text{KSH}}$", "$q_{\\text{WKI}}$", "$q_{\\text{YAK}}$", "$\\sigma_{\\tau}$"),
         est = ifelse(estimate < 0.001, scales::scientific(estimate, digits = 3), round(estimate, 3)),
         se = ifelse(std_err < 0.001, scales::scientific(std_err, digits = 3), round(std_err, 3))) %>%
  transmute(par, `24.0` = paste0(est, " (", se, ")")) -> par24.0

# model 24.1
out_24.1$parameter_estimates[,c(1:4)] %>%
  mutate(par = c("$\\sigma_{PE, \\ \\text{KNE}}^2$", "$\\sigma_{PE, \\ \\text{KSH}}^2$", "$\\sigma_{PE, \\ \\text{WKI}}^2$", "$\\sigma_{PE, \\ \\text{YAK}}^2$",
                 "$q_{\\text{KNE}}$", "$q_{\\text{KSH}}$", "$q_{\\text{WKI}}$", "$q_{\\text{YAK}}$", "$\\sigma_{\\tau}$"),
         est = ifelse(estimate < 0.001, scales::scientific(estimate, digits = 3), round(estimate, 3)),
         se = ifelse(std_err < 0.001, scales::scientific(std_err, digits = 3), round(std_err, 3))) %>%
  transmute(par, `24.1` = paste0(est, " (", se, ")")) -> par24.1
  
# model 24.2
out_24.2$parameter_estimates[,c(1:4)] %>%
  mutate(par = c("$\\sigma_{PE, \\ \\text{KNE}}^2$", "$\\sigma_{PE, \\ \\text{KSH}}^2$", "$\\sigma_{PE, \\ \\text{WKI}}^2$", "$\\sigma_{PE, \\ \\text{YAK}}^2$",
                 "$q_{\\text{KNE}}$", "$q_{\\text{KSH}}$", "$q_{\\text{WKI}}$", "$q_{\\text{YAK}}$", "$\\sigma_{\\tau}$"),
         est = ifelse(estimate < 0.001, scales::scientific(estimate, digits = 3), round(estimate, 3)),
         se = ifelse(std_err < 0.001, scales::scientific(std_err, digits = 3), round(std_err, 3))) %>%
  transmute(par, `24.2` = paste0(est, " (", se, ")")) -> par24.2

# model 24.3
out_24.3$parameter_estimates[,c(1:4)] %>%
  mutate(par = c("$\\sigma_{PE, \\ \\text{KNE}}^2$", "$\\sigma_{PE, \\ \\text{KSH}}^2$", "$\\sigma_{PE, \\ \\text{WKI}}^2$", "$\\sigma_{PE, \\ \\text{YAK}}^2$",
                 "$q_{\\text{KNE}}$", "$q_{\\text{KSH}}$", "$q_{\\text{WKI}}$", "$q_{\\text{YAK}}$", 
                 "$\\sigma_{\\tau, \\ \\text{KNE}}$", "$\\sigma_{\\tau, \\ \\text{KSH}}$", "$\\sigma_{\\tau, \\ \\text{WKI}}$", "$\\sigma_{\\tau, \\ \\text{YAK}}$"),
         est = ifelse(estimate < 0.001, scales::scientific(estimate, digits = 3), round(estimate, 3)),
         se = ifelse(std_err < 0.001, scales::scientific(std_err, digits = 3), round(std_err, 3))) %>%
  transmute(par, `24.3` = paste0(est, " (", se, ")")) -> par24.3

# join par tables and write
purrr::reduce(list(par24.0,par24.1,par24.2,par24.3), full_join, by = 'par') %>%
  dplyr::slice(1, 7:11, 2:5, 6, 11:14) %>%
  write_csv("./output/models/rema/2024/par_tab.csv")



# plot rema fits ----

# plot fit to biomass 
rbind(out_24.0$biomass_by_strata, out_24.1$biomass_by_strata, out_24.2$biomass_by_strata, out_24.3$biomass_by_strata) %>%
  ggplot()+
  geom_ribbon(data = function(x){filter(x, model_name == "24.2")}, aes(x = year, ymin = pred_lci, ymax = pred_uci), alpha = 0.2, fill = "grey70")+
  geom_line(aes(x = year, y = pred, color = model_name))+
  geom_point(aes(x = year, y = obs))+
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), width = 0)+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Round Biomass (t)", color = NULL)+
  facet_wrap(~strata, scales = "free") -> x
ggsave("./figures/assessment_models/2024/rema_biomass_fit.png", plot = x, width = 8, height = 5, units = "in")

# plot fit to cpue 
rbind(out_24.0$cpue_by_strata, out_24.1$cpue_by_strata, out_24.2$cpue_by_strata, out_24.3$cpue_by_strata) %>% 
  ggplot()+
  geom_ribbon(data = function(x){filter(x, model_name == "24.2")}, aes(x = year, ymin = pred_lci, ymax = pred_uci), alpha = 0.2, fill = "lightblue")+
  geom_point(aes(x = year, y = obs))+
  geom_errorbar(data = function(x){filter(x, model_name == "24.2")}, aes(x = year, ymin = tot_obs_lci, ymax = tot_obs_uci), width = 0, color = "grey70")+
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), width = 0)+
  geom_line(aes(x = year, y = pred, color = model_name))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "CPUE Index", color = NULL)+
  facet_wrap(~strata) -> x
ggsave("./figures/assessment_models/2024/rema_cpue_fit.png", plot = x, width = 8, height = 5, units = "in")

# total biomass trajectory
rbind(out_24.0$total_predicted_biomass, out_24.1$total_predicted_biomass, 
      out_24.2$total_predicted_biomass, out_24.3$total_predicted_biomass) %>%
  ggplot()+
  geom_ribbon(data = function(x){filter(x, model_name == "24.2")}, aes(x = year, ymin = pred_lci, ymax = pred_uci), alpha = 0.2, fill = "grey70")+
  geom_line(aes(x = year, y = pred, color = model_name))+
  scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_color_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Total Round Biomass (t)", color = NULL)+
  theme(legend.position = c(0, 1), 
        legend.justification = c(0,1)) -> x
ggsave("./figures/assessment_models/2024/rema_total_biomass.png", plot = x, width = 6, height = 3.5, units = "in")



# observer catch ----

## scallop catch data 2009/10 - present
do.call(bind_rows, lapply(list.files("data/observer/catch/", full.names = T), read_csv)) %>% 
  f_clean_catch() -> catch_tmp
catch_tmp %>%
  mutate(district = ifelse(district %in% c("WC", "C", "UB"), "M", district)) %>%
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  group_by(year, district) %>%
  summarise(retained_t = 0.000453592 * sum(round_weight, na.rm = T),
            retained_mw_t = 0.000453592 * sum(meat_weight, na.rm = T)) %>% ungroup -> ret_t

## scallop discard data 2009/10 - present
do.call(bind_rows, lapply(list.files("data/observer/bycatch/", full.names = T, pattern = ".csv"), read_csv)) %>%
  f_clean_bycatch( ., catch_tmp) %>%
  mutate(district = ifelse(district %in% c("WC", "C", "UB"), "M", district),
         year = as.numeric(substring(season, 1, 4))) %>%
  # get discards by day
  group_by(year, district, adfg, set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            disc_count = sum(disc_count, na.rm = T),
            disc_wt = sum(disc_wt, na.rm = T),
            broken_wt = sum(broken_wt, na.rm = T),
            rem_disc_wt = sum(rem_disc_wt, na.rm = T)) %>%
  # get discards by season
  group_by(year, district) %>%
  summarise(effort = sum(dredge_hrs, na.rm = T),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt, na.rm = T) / sum(sample_hrs, na.rm = T),
            discard_m_t = discard_rate_lb * effort * 0.000453592 * 0.2) %>% ungroup -> disc_t

# join catch data
left_join(ret_t, disc_t) %>%
  transmute(year, district, retained_t, retained_mw_t, discard_m_t) -> obs_catch

# reference points, surveyed area ----

# 2023/24 total catch
obs_catch %>%
  filter(year == 2023,
         district %in% c("KNE", "KSH", "WKI", "EKI", "YAK")) %>%
  summarise(total = sum(retained_t + discard_m_t)) %>% pull(total) -> total_catch

# M
M <- 0.13 

# tau_sf
tau_sf <- (yday(mdy("11/1/2023")) - yday(mdy("5/1/2023"))) / 365

# b_prj
out_24.2$total_predicted_biomass %>%
  filter(year == max(year)) %>% pull(pred) %>% as.numeric() %>%
  # do projection
  .[1] * exp(-M * tau_sf) - total_catch -> b_prj

# b_msy
b_msy <- mean(out_24.2$total_predicted_biomass$pred)

# b_bmsy
b_bmsy <- b_prj / b_msy

# f_ofl control rule
if(exists("b_bmsy")){
  if(b_bmsy <= 0.25){f_ofl <- 0}
  if(b_bmsy > 0.25 & b_bmsy <= 1){f_ofl <- (M*(b_bmsy-0.1))/(1-0.1)}
  if(b_bmsy > 1){f_ofl <- M}
}

# ofl 
ofl_survey <- 0.1 * b_prj * (1 - exp(-f_ofl))

# total catch ofl, non-surveyed area ----

# 2009 - 2023
## area H catch
read_csv("./data/observer/old_catch/h_fishery_table_1994_2019.csv") %>%
  transmute(year = season, district, retained_mw_t = ret_lbs_mt * 0.000453592) %>%
  filter(year %in% 2009:2023) -> area_h
## observed areas
obs_catch %>%
  # filter out surveyed area
  filter(!(district %in% c("KNE", "KSH", "WKI", "EKI", "YAK"))) %>%
  bind_rows(area_h) %>%
  filter(retained_mw_t > 0) %>%
  # get only mw estimates
  # compute ratio
  transmute(year, district, retained_mw_t, discard_m_mw_t = discard_m_t * 0.1,
            ratio = discard_m_mw_t / retained_mw_t) %>%
  # get avg ratio by year
  group_by(year) %>%
  mutate(rat_avg = mean(ratio, na.rm = T)) %>% ungroup %>%
  # fill in discards for area h
  mutate(discard_m_mw_t = ifelse(is.na(discard_m_mw_t), retained_mw_t * rat_avg, discard_m_mw_t)) %>%
  # compute total catch 
  mutate(total_mw_t = retained_mw_t + discard_m_mw_t) -> total_mw_district

# 1990 - 1998 excel sheets / ryan burt ft summary  
read_csv("./output/safe/2024/meat_weght_removals_time_series_1990_present.csv") %>%
  filter(landed_lb_mw > 0, year < 2009) %>%
  # make zero discards NA
  mutate(disc_lb_mw = ifelse(disc_lb_mw == 0, NA, disc_lb_mw)) %>%
  # fill in discards when absent 1996:2008
  group_by(year) %>%
  mutate(rat_avg_yr = mean(disc_lb_mw / landed_lb_mw, na.rm = T)) %>% ungroup %>%
  mutate(disc_lb_mw = ifelse(is.na(disc_lb_mw), landed_lb_mw * rat_avg_yr, disc_lb_mw)) %>%
  # fill in discards when absent 1990:1996
  group_by(district) %>%
  mutate(rat_avg_district = mean(disc_lb_mw / landed_lb_mw, na.rm = T)) %>% ungroup %>%
  mutate(disc_lb_mw = ifelse(is.na(disc_lb_mw), landed_lb_mw * rat_avg_district, disc_lb_mw)) %>%
  # convert to t
  transmute(year, district,
            retained_mw_t = landed_lb_mw * 0.000453592,
            discard_m_mw_t = disc_lb_mw * 0.000453592,
            total_mw_t = ifelse(!is.nan(discard_m_mw_t), retained_mw_t + discard_m_mw_t, retained_mw_t)) %>%
  filter(!(district %in% c("KNE", "KSH", "WKI", "EKI", "YAK", "E"))) -> total_mw_district_pre

# combine data 
bind_rows(total_mw_district_pre, total_mw_district %>% transmute(year, district, retained_mw_t, discard_m_mw_t, total_mw_t)) -> tot_catch
tot_catch %>%
  group_by(year) %>%
  summarise(retained_mw_t = sum(retained_mw_t, na.rm = T),
            discard_m_mw_t = sum(discard_m_mw_t, na.rm = T),
            total_mw_t = sum(total_mw_t, na.rm = T)) %>%
  write_csv("./output/models/rema/2024/non_survey_total_catch_ts.csv")
  
# plot of timeseries by district
tot_catch %>%
  mutate(district = factor(district, levels  = c("R", "O", "Q", "M", "H",
                                               "KSE", "KSEM", "KSW"))) %>%
  ggplot()+
  geom_bar(aes(x = factor(year), y = total_mw_t, fill = district), position = "stack", stat = "identity")+
  scale_fill_brewer(palette = "Set3")+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "Total Catch (t)", fill = NULL)+
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) -> x
ggsave("./figures/assessment_models/2024/tot_catch_mw_district.png", plot = x, width = 7, height = 3.5, units = "in")

## tot catch ofl 2009 - 2023
tot_catch %>% filter(year %in% 2009:2023) %>%
  group_by(year) %>%
  summarise(tot_t = sum(total_mw_t)) %>% ungroup %>%
  pull(tot_t) %>% mean -> ofl_ns_2009_2023  
## tot catch ofl 1990 - 1997  
tot_catch %>% filter(year %in% c(1990:1994, 1996:1997)) %>%
  group_by(year) %>%
  summarise(tot_t = sum(total_mw_t)) %>% ungroup %>%
  pull(tot_t) %>% mean -> ofl_ns_1990_1997
  
# reference point table ----

tibble(model = c("24.2", NA),
       b_hat = c(out_24.2$total_predicted_biomass %>%
                     filter(year == max(year)) %>% pull(pred) %>% as.numeric(), NA),
       b_prj = c(b_prj, NA),
       b_msy = c(b_msy, NA),
       b_bmsy = c(b_bmsy, NA),
       f_ofl = c(f_ofl, NA),
       ofl_s = c(ofl_survey, NA),
       ns_ref = c("1990-97", "2009-23"),
       ofl_ns = c(ofl_ns_1990_1997, ofl_ns_2009_2023)) %>%
  mutate(ofl_tot = ofl_survey[1] + ofl_ns) %>%
  write_csv("./output/models/rema/2024/reference_point_tab.csv")






