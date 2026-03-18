# notes ----

# observer data functions
# tyler jackson
# 1/17/2024

# load ----

library(tidyverse)
library(mgcv)
library(mgcViz)
library(DHARMa)
library(patchwork)

# ggplot theme ----

theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  #windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  #theme_light(base_size = base_size, base_family = base_family) 
  theme_light() +
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
tickr <- function(data, var, to) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}

theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1950:2100), yr, 3)

### custom color/fill pallete
cb_palette <- c("#009E73", "#0072B2", "#E69F00", "#56B4E9", 
                "#F0E442", "#D55E00", "#CC79A7")



# f_step_gam() ----

# args:
# null - null model
# full_scope - list giving the formula for the full model
# step - max number of steps, must just be sufficiently high
# r2_level - level of R2 change considered significant, default = 0.01
f_step_gam <- function(null, full_scope, steps = 1000, r2_level = 0.01, theta = NULL) {
  
  # functions within ----
  
  f_forward_selection <- function(null, full) {
    
    best.for <- null
    for (i in 1:steps){
      
      # extract best model stats
      best.for_AIC <- AIC(best.for, k = log(nrow(best.for$model)) + 1)
      best.for_r2 <- (best.for$null.deviance - best.for$deviance) / best.for$null.deviance
      
      # set up available terms
      best.for_terms <- gsub(" ", "", strsplit(deparse1(best.for$formula), split = "\\+|\\~")[[1]])
      avail_terms <- gsub(" ", "", strsplit(deparse1(full$formula), split = "\\+|\\~")[[1]])
      avail_terms <- avail_terms[!(avail_terms %in% best.for_terms)]
      
      if(length(avail_terms) == 0) {
        # complete
          message("Done with forward selection")
          print(fortmp)
          print(best.for)
          break
       
      }
      
      # fit test models
      for.mods = list()
      for.aic <- NULL
      for.r2 <- NULL
      sig <- NULL
      df <- NULL
      for(i in 1:length(avail_terms)){
        for.mods[[i]] <- update(best.for, paste0("~ . +", avail_terms[i]))
        for.aic[i] <- AIC(for.mods[[i]], k = log(nrow(for.mods[[i]]$model)) + 1)
        for.r2[i] <- (for.mods[[i]]$null.deviance - for.mods[[i]]$deviance) / for.mods[[i]]$null.deviance
        df[i] <- best.for$df.residual - for.mods[[i]]$df.residual
      }
      tibble(term = paste0("+ ", avail_terms),
             AIC = for.aic,
             delta_AIC = for.aic - best.for_AIC,
             r2 = for.r2,
             delta_r2 = for.r2 - best.for_r2,
             df) -> fortmp
      # filter for significant improvements
      sig <- fortmp %>% 
        filter(abs(delta_AIC) >= (2 * df),
               delta_r2 >= r2_level)
      
      # not complete
      if(nrow(sig) > 0) {
        # print progress
        message("best form:"); print(best.for$formula); message("trying:");print(fortmp)
        
        # keep the best by AIC
        filter(sig, delta_AIC == min(delta_AIC)) -> upd
        # update the best forward model
        best.for <- update(best.for, paste0("~ .", upd$term))
        steps <- steps - 1
      }
      # complete
      if(nrow(sig) == 0 | (best.for$formula == null$formula)) {
        message("Done with forward selection")
        print(fortmp)
        print(best.for)
        break
      }
    }
    
    return(list(best.for, fortmp))
    
  }
  f_backward_selection <- function(null, full) {
    
    best.back <- full      
    for (i in 1:steps){
      
      # extract best model stats
      best.back_AIC <- AIC(best.back, k = log(nrow(best.back$model)) + 1)
      best.back_r2 <- (best.back$null.deviance - best.back$deviance) / best.back$null.deviance
      
      # set up available terms
      avail_terms <- gsub(" ", "", strsplit(deparse1(best.back$formula), split = "\\+|\\~")[[1]])
      null_terms <- gsub(" ", "", strsplit(deparse1(null$formula), split = "\\+|\\~")[[1]])
      avail_terms <- avail_terms[!(avail_terms %in% null_terms)]
      
      # fit test models
      back.mods = list()
      back.aic <- NULL
      back.r2 <- NULL
      sig <- NULL
      df <- NULL
      for(i in 1:length(avail_terms)){
        back.mods[[i]] <- update(best.back, paste0("~ . -", avail_terms[i]))
        back.aic[i] <- AIC(back.mods[[i]], k = log(nrow(back.mods[[i]]$model)) + 1)
        back.r2[i] <- (back.mods[[i]]$null.deviance - back.mods[[i]]$deviance) / back.mods[[i]]$null.deviance
        df[i] <- back.mods[[i]]$df.residual - best.back$df.residual 
      }
      tibble(term = paste0("- ", avail_terms),
             AIC = back.aic,
             delta_AIC = back.aic - best.back_AIC,
             r2 = back.r2,
             delta_r2 = best.back_r2 - back.r2,
             df) -> backtmp
      
      # drop the term that is the least improvement
      backtmp %>% 
        filter((delta_r2 < r2_level) | (delta_AIC < 2 * df)) -> drop
      
      
      # not complete
      if(nrow(drop) > 0) {
        # print progress
        message("best form:"); print(best.back$formula); message("trying:");print(backtmp)
        
        # keep the best by AIC
        filter(drop, delta_AIC == min((delta_AIC))) -> upd
        # update the best forward model
        best.back <- update(best.back, paste0("~ .", upd$term))
        steps <- steps - 1
      }
      # complete
      if(nrow(drop) == 0 | (best.back$formula == null$formula)) {
        message("Done with backward selection")
        print(backtmp)
        print(best.back)
        break
      }
    }
    
    return(best.back)
    
  }
  
  # model selection and theta search ----
  
  for(k in 1:100) {
    
    # update full model
    full <- update(null, full_scope[[1]])
    # forward selection 
    for_mod <- f_forward_selection(null, full)
    for_best <- for_mod[[1]]
    fortmp <- for_mod[[2]]
    # backward selection 
    back_best <- f_backward_selection(null, full)
    
    # check that forward and backward models match ----
    if(paste(sort(attributes(for_best$terms)$term.labels), collapse = "+") != paste(sort(attributes(back_best$terms)$term.labels), collapse = "+")) {
      message("Forward and Backward do not match, choosing the best model based on CAIC")
      if(AIC(for_best, k = log(nrow(for_best$model))) < AIC(back_best, k = log(nrow(back_best$model)))) {best <- for_best} else{best <- back_best}
    }
    else{best = for_best}
    
    if(!is.null(theta)){
      # get optimal theta
      theta_opt <- MASS::theta.ml(best$model$tot_legal, fitted(best))[1]  
      
      # check theta
      if(abs(as.numeric(str_extract(null$family$family, "\\d+\\.*\\d*")) - theta_opt) > 0.01) {
        # refit null
        null <- update(null, family = negbin(theta = round(theta_opt, 2), link = "log")) 
      } 
      else{break}
    }
    else{break}
    
  }
  
  # output ----
  
  if(is.null(theta)) {return(list(best, fortmp))}
  else{return(list(model = best,
                   theta = theta_opt))}
  
  
}

# f_getCPUE_gam() ----
f_getCPUE_gam <- function(model, where, years) {
  
  ## Make sure that model is a summary and error-check vector lengths
  if(any(class(model) == "gam"))
    model <- summary(model)    # model is now a summary object
  n <- length(where) + 1       # number of CPUE indices to be extracted
  if(length(years) != n)
    stop("Expected ", n, " years, got ", length(years), ".")
  
  ## Extract model components
  relative <- as.numeric(c(0, model$p.coeff[where]))  # shave off names
  std_cpue <- exp(relative - mean(relative))
  
  V <- model$cov.scaled[where, where]
  
  ## Compute confidence limits
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1,] <- Q[-1,] + diag(rep(1, n - 1))
  V0 <- (Q %*% V) %*% t(Q)
  SE <- sqrt(diag(V0))
  Upper <- exp(log(std_cpue) + 2 * SE)
  Lower <- exp(log(std_cpue) - 2 * SE)
  
  out <- tibble(year = years, 
                index = std_cpue, 
                se = SE, 
                l95 = Lower,
                u95 = Upper)
  return(out)
  
}


# f_step_plot() ----
## create step plot for cpue index terms

## args: model - final cpue standardization model, class gam or bam
##       term_labs - optional, alternative term labels for plot

f_step_plot <- function(model, term_labs = NULL){
  
  ## get terms
  form <- gsub(" ", "", strsplit(deparse1(model$formula), split = "\\+|\\~")[[1]])
  resp <- form[1]
  # get the focal (index) term
  index_term <- form[2]
  # update terms
  terms <- form[c(-1,-2)]
  
  # fit the null model and get a standardized index
  null <- update(model, paste0("~.", paste0("- ", terms, collapse = " ")))
  loc <- grep(index_term, names(coef(null)))
  yrs <- sort(null$model %>% pull(index_term) %>% unique)
  
  if(class(model)[1] %in% c("bam", "gam")){
    f_getCPUE_gam(null, loc, yrs) %>%
      mutate(model = index_term) %>%
      dplyr::select(6, 1:5) -> null_ind
    
    # fit models and get a list of std indices
    mods <- list(null)
    ind <- list(null_ind)
    for(i in 2:(length(terms)+1)){
      mods[[i]] <- update(mods[[i-1]], paste0("~ . + ", terms[i-1]))
      loc <- grep(index_term, names(coef(mods[[i]])))
      yrs <- sort(mods[[i]]$model %>% pull(index_term) %>% unique)
      f_getCPUE_gam(mods[[i]], loc, yrs) %>%
        mutate(model = paste("+", terms[i-1])) %>%
        dplyr::select(6, 1:5) -> ind[[i]]
    } 
  }
  if(class(model)[1] %in% c("glm", "lm")){
    f_getCPUE(null, loc, yrs) %>%
      mutate(model = index_term) %>%
      dplyr::select(6, 1:5) -> null_ind
    
    # fit models and get a list of std indices
    mods <- list(null)
    ind <- list(null_ind)
    for(i in 2:(length(terms)+1)){
      mods[[i]] <- update(mods[[i-1]], paste0("~ . + ", terms[i-1]))
      loc <- grep(index_term, names(coef(mods[[i]])))
      yrs <- sort(mods[[i]]$model %>% pull(index_term) %>% unique)
      f_getCPUE(mods[[i]], loc, yrs) %>%
        mutate(model = paste("+", terms[i-1])) %>%
        dplyr::select(6, 1:5) -> ind[[i]]
    } 
  }
  
  # plot
  do.call("rbind", ind) %>%
    mutate(model = factor(model, levels = c(index_term, paste("+", terms)))) %>%
    nest_by(model, .keep = T) %>% ungroup -> tmp
  
  plot_dat <- list(tmp$data[[1]] %>% rename(model_background = model))
  for(i in 2:nrow(tmp)) {
    tmp$data[[i]] %>%
      bind_rows(do.call("rbind", tmp$data[1:(i-1)])) %>%
      rename(model_background = model) -> plot_dat[[i]]
  }
  
  if(is.null(term_labs)){term_labs <- tmp %>% pull(model) %>% unique}
  names(term_labs) <- tmp %>% pull(model) %>% unique
  
  tmp %>%
    mutate(plot_dat = plot_dat) %>%
    transmute(model, plot_dat) %>%
    unnest(plot_dat) %>%
    mutate(alpha = model_background == model) %>%
    ggplot()+
    geom_point(aes(x = year, y = index, alpha = alpha), show.legend = F)+
    geom_line(aes(x = year, y = index, group = model_background, alpha = alpha), show.legend = F)+
    geom_hline(yintercept = 1, linetype = 2)+
    scale_alpha_manual(values = c(0.1, 1))+
    facet_wrap(~model, ncol = 1, strip.position = "left", labeller = labeller(model = term_labs))+
    labs(x = NULL, y = NULL)+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.spacing = unit(0, "lines")) -> plot
  
  return(plot)
  
}
# data cleaning functions ----

# rename catch data to appropriate style
# args:
## x - catch data (as downloaded directly from wiki)
f_catch_rename <- function(x){
  names(x) <- c("fishery", "district", "adfg", "trip_id", "haul_id", 
                "haul", "gear_perf", "haul_sampled", "set_date", "bed_code", "set_lat",
                "set_lon", "statarea", "depth", "dredge_count", 
                "dredge_width", "dredge_down", "dredge_up", "duration", 
                "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                "rtnd_basket", "scallop_count", "round_weight", "meat_weight", "est_yield",
                "tot_rtnd_basket", "tot_day_meat_weight")
  x
}

# rename bycatch data to appropriate style
# args:
## x - daily bycatch data (as downloaded directly from wiki)
f_bycatch_rename <- function(x){
  names(x) <- c("fishery", "district", "adfg", "haul_id", "haul", "gear_perf", "set_date", "bed_code", "dredge_hrs", 
                "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                "dungeness_count", "king_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                "rem_disc_wt", "clapper_count")
  x
}

# rename crab_size data to appropriate style
# args:
## x - crab size data (as downloaded directly from wiki)
f_crab_size_rename <- function(x){
  names(x) <- c("fishery", "district", "race_code", "sex", "cw", "samp_frac")
  x
}

# rename shell_height data to appropriate style
# args:
## x - shell height data (as downloaded directly from wiki)
f_shell_height_rename <- function(x){
  names(x) <- c("fishery", "district", "haul_id", "adfg", "rtnd_disc", "sh", "shell_num")
  x
}

# add season to data (based on Fishery field)
# args:
## x - tibble of observer or logbook data
## fishery_col - name of column that denotes fishery. Default = "Fishery"
f_add_season <- function(x, fishery_col = "fishery"){
  x %>%
    pull(grep(fishery_col, names(.))) %>%
    str_sub(., 3, 4) %>%
    as.numeric() %>%
    tibble(season = .) %>%
    mutate(season = ifelse(season < 80, season + 2000, season + 1900),
           season = factor(paste0(season, "/", substring(season + 1, 3, 4)))) %>%
    bind_cols(x)
}

f_season_yr <- function(season){
  as.numeric(substring(season, 1, 4))
}

# revise district to align with current mgmt structure
# args: x - any tibble containing the field 'district' or 'District'
f_revise_district <- function(x){
  
  if(!("district" %in% names(x))){
    x %>%
      mutate(district = ifelse(bed_code %in% c("KSH4", "KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & ((set_lat < 57.7 & set_lon <= -154.35) | (is.na(set_lat))),
                               "KSW", district),
             district = ifelse(district %in% c("D16", "D", "YAK"),
                               "YAK", district))
  } else{
    x %>%
      mutate(district = ifelse(bed_code %in% c("KSH4", "KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & ((set_lat < 57.7 & set_lon <= -154.35) | (is.na(set_lat))),
                               "KSW", district),
             district = ifelse(district %in% c("D16", "D", "YAK"),
                               "YAK", district))
  }
}

# data mgmt for raw catch data prior to downstream analysis
# args:
## x - catch data (as downloaded directly from wiki)
f_clean_catch <- function(x) {
  x %>% 
    ## rename fields in current data (2009 - present)
    f_catch_rename() %>%
    ## drop phantom rows
    drop_na(haul_id) %>%
    ## add Season to data
    f_add_season() %>%
    ## classify Karluk bed as KSW district instead of KSH
    f_revise_district() %>% 
    ## coerce date to date class
    mutate(set_date = lubridate::mdy(set_date)) %>%
    ## remove tows with zero dredge hours (logbook mistake)
    filter(dredge_hrs != 0)  %>%
    ## coerce date to date class
    ## fix issue with missing basket weight in 2018/19
    mutate(round_weight = ifelse(season == "2018/19" & district == "O",
                                 54.1 * rtnd_basket, round_weight),
           bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code))
}

# data mgmt for raw bycatch data prior to downstream analysis
# args:
## x - bycatch data (as downloaded directly from wiki)  
## catch - cleaned catch data
f_clean_bycatch <- function(x, catch) { 
  x %>%
    ## rename fields in bycatch data (2009 - present)
    f_bycatch_rename() %>%
    ## add Season to data
    f_add_season() %>%
    ## coerce date to date class
    mutate(set_date = lubridate::mdy(set_date)) %>%
    ## remove district info
    dplyr::select(-1:-4) %>%
    ## get beds/district info from catch data
    ## ie no bycatch data that does not match with catch data !
    right_join(catch %>% dplyr::select(season, adfg, district, haul_id, set_lat, set_lon), 
               by = c("haul_id")) 
}

# data mgmt for raw crab size composition data prior to downstream analysis
# args:
## x - crab size data (as downloaded directly from wiki) 
f_clean_crab_size <- function(x) { 
  x %>%
    ## reason fields in crab size data (2009 - present)
    f_crab_size_rename() %>%
    ## drp the empty column
    dplyr::select(-7) %>%
    ## add Season to data
    f_add_season() %>%
    ## revise district
    ## unable to correct data for KSH / KSW
    mutate(district = ifelse(district %in% c("D", "YAK", "D16"), "YAK", district)) %>%
    ## add Species and Sex
    mutate(species = case_when(race_code == 68560 ~ "tanner_crab",
                               race_code == 68541 ~ "snow_crab"),
           species = factor(species, levels = c("tanner_crab", "snow_crab")),
           sex = case_when(sex == 1 ~ "Male",
                           sex == 2 ~ "Female",
                           sex == 3 ~ "Unknown"),
           sex = factor(sex, levels = c("Male", "Female", "Unknown")))
}

# data mgmt for raw crab size composition data prior to downstream analysis
# args:
## x - crab size data (as downloaded directly from wiki) 
## catch - cleaned catch data
f_clean_sh <- function(x, catch) { 
  x %>%
    ## rename fields in shell_height data (2009 - present)
    f_shell_height_rename() %>%
    ## add season to data
    f_add_season() %>%
    ## revise district as in catch data
    mutate(district = catch$district[match(.$haul_id, catch$haul_id)]) 
}

# data mgmt for raw meat weight data prior to downstream analysis
# args:
## x - meat weight data (as downloaded directly from wiki) 
## catch - cleaned catch data
f_clean_meat <-function(x){
  
  x %>%
    ## rename haul_id to join to catch
    rename_all(tolower) %>%
    mutate(district = ifelse(district %in% c("D", "D16"), "YAK", district)) %>%
    ## add season to meat weight data
    f_add_season(fishery_col = "fishery") %>%
    ## create haul id for 2020/21
    mutate(set_date = mdy(sample_date),
           haul_id =  paste0(fishery,
                                   sprintf("%06d", adfg),
                                   year(set_date),
                                   sprintf("%02d", month(set_date)),
                                   sprintf("%02d", day(set_date)),
                                   sprintf("%04d", haul))) %>%
    ## add retained - discard factor
    mutate(rtnd_disc = ifelse(shell_num < 11, "retained", "discarded")) -> meat
}
# f_fish_stats() ----
# quick summary of fishery statistics
# args:
## x - cleaned catch data
## district - abbrev. for any districts to summarise over
## ghl - logical include cmobine ghl. Default = F
## path - optional. Writes .csv file to path provided
f_fish_stats <- function(x, dist, add_ghl = F, path){
  # add Season if it is not found
  if(!("season" %in% names(x))) {x <- f_add_season(x)}
  # summarize catch data
  x %>%
    # filter to district
    filter(district %in% dist) %>%
    # summarise data 
    group_by(season) %>%
    summarise(mt_wt = round(sum(meat_weight, na.rm = T)),
              rnd_wt = round(sum(round_weight, na.rm = T)),
              rnd_num = round(sum(scallop_count, na.rm = T)),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              number_hauls = n(),
              mw_cpue = mt_wt / dredge_hrs,
              rw_cpue = rnd_wt / dredge_hrs) -> tmp
  # add ghl if necessary
  if(add_ghl == T){
    if(!exists("ghl")){stop("tibble named 'ghl' not found")}
    else{
      ghl %>%
        f_add_season() %>%
        filter(district %in% dist) %>%
        group_by(season) %>%
        summarise(ghl = sum(ghl, na.rm = T)) %>%
        left_join(., tmp, by = "season") %>%
        replace_na(list(mt_wt = 0, rnd_wt = 0, rnd_num = 0, dredge_hrs = 0, number_hauls = 0)) %>%
        filter(as.numeric(substring(season, 1, 4)) > 2008) -> tmp
    }
  }
  # write to a csv if necessary
  if(missing(path)) {tmp}
  else{
    write_csv(tmp, path)
    tmp
  }
}

# f_extent_catch () ----
## graphical extent of roundweight catch (mean distance between dredges in graphical units)
### args:
### x - logbook catch data
### quant - cut off quantile for contribution to catch. Default = 0.9.
f_extent_catch <- function(x, quant = 0.9){
  x %>%
    arrange(-round_weight) %>%
    mutate(cum_prop = cumsum(round_weight) / sum(round_weight, na.rm = T)) %>%
    filter(cum_prop <= quant) %>%
    dplyr::select(set_lon, set_lat) %>%
    dist() %>%
    mean()
}
