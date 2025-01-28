# notes -----

# scalobservR - functions for dealing with scallop observer data

## tyler jackson
## 1/14/2024

library(tidyverse)

# load_catch_by_haul() ----

# Load Scallop Observer Catch by Haul Data
#
# Load scallop observer catch by haul data and do data management routine.
# @param dir NULL. Path to directory with annual data pulls.
# @param dist NULL. Character string district abbreviation: KSH, YAK, KNE, WKI, EKI, O, UB, C, WC, Q.
# @param database_pull Default = FALSE. Do updated pull from database.
# @param clean Default = TRUE. Do data cleaning.
# @return Dataframe with observer catch by haul data
# @examples load_catch_by_haul(dir = ".", district = "KSH")
#
# @export
#
load_catch_by_haul <- function(dir, district = NULL, database_pull = F, clean = T) {
  
  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    obs <- do.call(bind_rows, lapply(list.files(dir, full.names = T), read_csv))
  }
  
  if(clean == T) {
    obs %>% 
      # rename
      rename_all(~c("fishery", "district", "adfg", "trip_id", "haul_id", 
                 "haul", "gear_perf", "haul_sampled", "set_date", "bed_code", "set_lat",
                 "set_lon", "statarea", "depth", "dredge_count", 
                 "dredge_width", "dredge_down", "dredge_up", "duration", 
                 "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                 "rtnd_basket", "scallop_count", "round_weight", "meat_weight", "est_yield",
                 "tot_rtnd_basket", "tot_day_meat_weight")) %>%
      ## drop phantom rows
      drop_na(haul_id) %>%
      ## add scal_year and season to data
      mutate(scal_year = ifelse(as.numeric(str_sub(fishery, 3, 4)) < 80, as.numeric(str_sub(fishery, 3, 4)) + 2000, as.numeric(str_sub(fishery, 3, 4)) + 1900),
             season = factor(paste0(scal_year, "/", substring(scal_year + 1, 3, 4)))) %>%
      ## classify Karluk bed as KSW district instead of KSH
      mutate(district = ifelse(bed_code %in% c("KSH4", "KSH5", "KSH6", "KSH7"), "KSW", district),
             district = ifelse(district == "KSH" & ((set_lat < 57.7 & set_lon <= -154.35) | (is.na(set_lat))),
                               "KSW", district),
             district = ifelse(district %in% c("D16", "D", "YAK"),
                               "YAK", district)) %>%
      ## coerce date to date class
      mutate(set_date = lubridate::mdy(set_date)) %>%
      ## remove tows with zero dredge hours (logbook mistake)
      filter(dredge_hrs != 0)  %>%
      ## fix issue with missing basket weight in 2018/19
      mutate(round_weight = ifelse(season == "2018/19" & district == "O",
                                   54.1 * rtnd_basket, round_weight),
             bed_code = ifelse(bed_code == "YAKB", "EK1", bed_code)) -> out
    
    if(!is.null(district)){out %>% filter(district %in% dist) -> out}
      
  }
  if(clean == F) {
    if(!is.null(district)){obs %>% filter(District %in% dist) -> out}
  }
 
 return(out)
  
}


# load_bycatch_by_haul() ----

# Load Scallop Observer Bycatch by Haul Data
#
# Load scallop observer bycatch by haul data and do data management routine.
# @param dir NULL. Path to directory with annual data pulls.
# @param catch NULL. Cleaned catch data.
# @param dist NULL. Character string district abbreviation: KSH, YAK, KNE, WKI, EKI, O, UB, C, WC, Q.
# @param database_pull Default = FALSE. Do updated pull from database.
# @param clean Default = TRUE. Do data cleaning.
# @return Dataframe with observer catch by haul data
# @examples load_catch_by_haul(dir = ".", district = "KSH")
#
# @export
#
load_bycatch_by_haul <- function(dir, catch, district = NULL, database_pull = F, clean = T) {
  
  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    obs <- do.call(bind_rows, lapply(list.files(dir,pattern = ".csv", full.names = T), read_csv))
  }
  
  if(clean == T) {
    obs %>% 
      # rename
      rename_all(~c("fishery", "district", "adfg", "haul_id", "haul", "gear_perf", "set_date", "bed_code", "dredge_hrs", 
                    "est_rnd_wt", "mt_wt", "sample_hrs", "bairdi_count", "opilio_count",
                    "dungeness_count", "king_count", "halibut_count", "disc_count", "disc_wt", "broken_wt",
                    "rem_disc_wt", "clapper_count")) %>%
      ## drop phantom rows
      drop_na(haul_id) %>%
      ## add scal_year and season to data
      mutate(scal_year = ifelse(as.numeric(str_sub(fishery, 3, 4)) < 80, as.numeric(str_sub(fishery, 3, 4)) + 2000, as.numeric(str_sub(fishery, 3, 4)) + 1900),
             season = factor(paste0(scal_year, "/", substring(scal_year + 1, 3, 4)))) %>% 
      ## classify Karluk bed as KSW district instead of KSH
      # mutate(district = ifelse(bed_code %in% c("KSH4", "KSH5", "KSH6", "KSH7"), "KSW", district),
      #        district = ifelse(district == "KSH" & ((set_lat < 57.7 & set_lon <= -154.35) | (is.na(set_lat))),
      #                          "KSW", district),
      #        district = ifelse(district %in% c("D16", "D", "YAK"),
      #                          "YAK", district)) %>%
      ## remove district info
      dplyr::select(-1:-3) %>%
      ## get beds/district info from catch data
      ## ie no bycatch data that does not match with catch data !
      right_join(catch %>% dplyr::select(adfg, district, haul_id, set_lat, set_lon), 
                 by = c("haul_id")) %>%
      ## coerce date to date class
      mutate(set_date = lubridate::mdy(set_date)) %>%
      ## remove tows with zero dredge hours (logbook mistake)
      filter(dredge_hrs != 0) -> out
    
    if(!is.null(district)){out %>% filter(district %in% dist) -> out}
    
  }
  if(clean == F) {
    if(!is.null(district)){obs %>% filter(District %in% dist) -> out}
  }
  
  return(out)
  
}



# load_sh_data() ----


# Load Scallop Observer Shell Height Composition Data
#
# Load scallop observer bycatch by haul data and do data management routine.
# @param dir NULL. Path to directory with annual data pulls.
# @param catch NULL. Cleaned catch data.
# @param dist NULL. Character string district abbreviation: KSH, YAK, KNE, WKI, EKI, O, UB, C, WC, Q.
# @param database_pull Default = FALSE. Do updated pull from database.
# @param clean Default = TRUE. Do data cleaning.
# @return Dataframe with observer catch by haul data
# @examples load_sh_data(dir = ".", district = "KSH")
#
# @export
#
load_sh_data <- function(dir, catch, dist = NULL, database_pull = F, clean = T) {
  
  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    obs <- do.call(bind_rows, lapply(list.files(dir, full.names = T), read_csv))
  }
  
  if(clean == T) {
  
    obs %>%
      ## rename fields in shell_height data (2009 - present)
      rename_all(~c("fishery", "district", "haul_id", "adfg", "rtnd_disc", "sh", "shell_num")) %>%
      ## add scal_year and season to data
      mutate(scal_year = ifelse(as.numeric(str_sub(fishery, 3, 4)) < 80, as.numeric(str_sub(fishery, 3, 4)) + 2000, as.numeric(str_sub(fishery, 3, 4)) + 1900),
             season = factor(paste0(scal_year, "/", substring(scal_year + 1, 3, 4)))) %>% 
      ## revise district as in catch data
      mutate(district = catch$district[match(.$haul_id, catch$haul_id)]) -> out
  
    if(!is.null(dist)){out %>% filter(district %in% dist) -> out}
    
  }
  if(clean == F) {
    if(!is.null(dist)){obs %>% filter(District %in% dist) -> out}
  }
  
  return(out)
  
}

# load_mw_data () ----

# Load Scallop Observer Meat Weight Data
#
# Load scallop observer bycatch by haul data and do data management routine.
# @param dir NULL. Path to directory with annual data pulls.
# @param catch NULL. Cleaned catch data.
# @param dist NULL. Character string district abbreviation: KSH, YAK, KNE, WKI, EKI, O, UB, C, WC, Q.
# @param database_pull Default = FALSE. Do updated pull from database.
# @param clean Default = TRUE. Do data cleaning.
# @return Dataframe with observer meat weight by haul data
# @examples load_mw_data(dir = ".", district = "KSH")
#
# @export
#
load_mw_data <- function(dir, catch, dist = NULL, database_pull = F, clean = T) {
  
  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    obs <- do.call(bind_rows, lapply(list.files(dir, full.names = T), read_csv))
  }
  
  if(clean == T) {
    
    obs %>%
      ## rename haul_id to join to catch
      rename_all(tolower) %>%
      ## add scal_year and season to data
      mutate(scal_year = ifelse(as.numeric(str_sub(fishery, 3, 4)) < 80, as.numeric(str_sub(fishery, 3, 4)) + 2000, as.numeric(str_sub(fishery, 3, 4)) + 1900),
             season = factor(paste0(scal_year, "/", substring(scal_year + 1, 3, 4)))) %>% 
      ## create haul id for 2020/21
      mutate(set_date = mdy(set_date),
             haul_id = ifelse(season == "2020/21", 
                              paste0(fishery,
                                     sprintf("%06d", adfg),
                                     year(set_date),
                                     sprintf("%02d", month(set_date)),
                                     sprintf("%02d", day(set_date)),
                                     sprintf("%04d", haul)), haul_id)) %>%
      ## join with catch data to get location (district, bed)
      left_join(catch %>%
                  dplyr::select(haul_id, district, bed_code, set_lat, set_lon),
                by = c("haul_id")) %>%
      ## add retained - discard factor
      mutate(rtnd_disc = ifelse(shell_num < 11, "retained", "discarded")) -> out
    
    if(!is.null(dist)){out %>% filter(district %in% dist) -> out}
    
  }
  if(clean == F) {
    if(!is.null(dist)){obs %>% filter(District %in% dist) -> out}
  }
  
  return(out)
  
}

# get_retained_summary() ---- 

# Compute retained catch summary statistics by season and district
#
# Compute retained meat weight, round weight, effort, and nominal CPUE
# @param data NULL. Cleaned catch by haul data. See load_catch_by_haul()
# @param by NULL. Grouping variable, character string other than scal_year and season (e.g. "district")
# @param units NULL. Unit of measurement, "t" or "lb". Default = "lb".
# @return Dataframe with summary statistics
# @examples get_retained_summary(data = catch, by = c("scal_year", "district"))
#
# @export
#
get_retained_summary <- function(data, by = NULL, units = "lb") {
  
  if(!(units %in% c("t", "lb"))){stop("Do not recognize the units, use t or lb")}
  units_conversion = ifelse(units == "lb", 1, 0.000453592)
  
  # make sure season and scal_year are not in by
  by <- by[!(by %in% c("season", "scal_year"))]
  
  data %>%
    group_by_at(c("season", "scal_year", by)) %>%
    summarise(ret_mw = round(sum(meat_weight, na.rm = T)) * units_conversion,
              ret_rw = round(sum(round_weight, na.rm = T)) * units_conversion,
              ret_n = round(sum(scallop_count, na.rm = T)),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              hauls = n(),
              mw_cpue = ret_mw / dredge_hrs,
              rw_cpue = ret_rw / dredge_hrs) %>% ungroup -> out
  
  return(out)
  
}

# get_discard_summary() -----

# Compute discarded catch summary statistics by season and district
#
# Compute retained catch summary statistics by season and district
#
# Compute discard and discard mortality from bycatch by haul data
# @param data NULL. Cleaned bycatch by haul data. See load_bycatch_by_haul()
# @param by NULL. Grouping variable, character string other than scal_year and season (e.g. "district")
# @param units NULL. Unit of measurement, "t" or "lb". Default = "lb".
# @return Dataframe with summary statistics
# @examples get_discard_summary(data = catch, by = c("scal_year", "district"))
#
# @export
#
get_discards <- function(data, by = NULL, units = "lb") {
  
  if(!(units %in% c("t", "lb"))){stop("Do not recognize the units, use t or lb")}
  units_conversion = ifelse(units == "lb", 1, 0.000453592)
  
  # make sure season and scal_year are not in by
  by <- by[!(by %in% c("season", "scal_year"))]
  
  # unstratified discard estimate
  data %>%
    filter(sample_hrs > 0) %>%
    group_by_at(c("season", "scal_year", by)) %>% #ungroup %>% #dplyr::slice(273) %>% as.data.frame() %>%
    transmute(sample_hrs,
              dredge_hrs,
              disc_rw = (disc_wt + broken_wt + rem_disc_wt) * units_conversion,
              disc_n = ifelse(disc_count > 0, (disc_count / (disc_wt + broken_wt)) * (disc_wt + broken_wt + rem_disc_wt), 0),
              disc_cpue_rw = disc_rw / sample_hrs,
              disc_cpue_n = disc_n / sample_hrs) %>%
    # get observer average discards
    summarise(disc_cpue_rw_bar = mean(disc_cpue_rw),
              disc_cpue_rw_var = var(disc_cpue_rw) / n(),
              disc_cpue_n_bar = mean(disc_cpue_n),
              disc_cpue_n_var = var(disc_cpue_n) / n(),
              sample_hrs = sum(sample_hrs), .groups = "drop") %>% 
    # join to dredge hrs
    left_join(data %>%
                group_by_at(c("season", "scal_year", by)) %>%
                summarise(dredge_hrs = sum(dredge_hrs), .groups = "drop"),
              by = c("season", "scal_year", by)) %>%
    # do expansion
    group_by_at(c("season", "scal_year", by)) %>%
    transmute(sample_hrs,
              discards_rw = disc_cpue_rw_bar * dredge_hrs,
              discards_rw_cv = sqrt(disc_cpue_rw_var *  dredge_hrs^2) / discards_rw ,
              discards_n = disc_cpue_n_bar * dredge_hrs,
              discards_n_cv = sqrt(disc_cpue_n_var *  dredge_hrs^2) / discards_n) %>%
    ungroup -> out
  
  return(out)
  
}





# get_sh_composition() ----

# Get Sampled Number at Size
#
# Compute number of scallops measured at size and total number measured
# @param data NULL. Cleaned shell height by haul data. See load_sh_data()
# @param by NULL. Grouping variable, character string other than scal_year and season (e.g. "district")
# @param by NULL. Grouping variable, character string other than scal_year and season (e.g. "district")


# @return Dataframe with summary statistics
# @examples get_retained_summary(data = catch, by = c("scal_year", "district"))
#
# @export
#
get_sh_composition <- function(data, type = NULL, by = NULL, catch = NULL, bycatch = NULL) {
  
  # make sure season and scal_year are not in by
  by <- by[!(by %in% c("season", "scal_year"))]
  
  if((is.null(type)|type == "retained") & is.null(catch)) {stop("Missing catch data")}
  if((is.null(type)|type == "discard") & is.null(bycatch)) {stop("Missing bycatch data")}
  
  # do summarise depending on type
  if(type == "retained") {
    shell_height %>%
      filter(rtnd_disc == "R") %>%
      left_join(catch %>% 
                  transmute(haul_id, wt = round_weight), by = join_by(haul_id)) %>%
      group_by_at(c("season", "scal_year", by)) %>%
      mutate(n_meas = n(),
             total_wt = sum(wt, na.rm = T)) %>%
      group_by_at(c("season", "scal_year", by, "sh")) %>%
      reframe(n = n(),
              n_meas = mean(n_meas),
              wt = sum(wt, na.rm = T),
              total_wt = mean(total_wt),
              p = wt / total_wt) -> out
  }
  if(type == "discard") {
    shell_height %>%
      filter(rtnd_disc == "D") %>%
      left_join(bycatch %>% transmute(haul_id, 
                                      wt = (disc_wt + broken_wt + rem_disc_wt) / sample_hrs * dredge_hrs), 
                by = join_by(haul_id)) %>%
      group_by_at(c("season", "scal_year", by)) %>%
      mutate(n_meas = n(),
             total_wt = sum(wt, na.rm = T)) %>%
      group_by_at(c("season", "scal_year", by, "sh")) %>%
      reframe(n = n(),
              n_meas = mean(n_meas),
              wt = sum(wt, na.rm = T),
              total_wt = mean(total_wt),
              p = wt / total_wt) -> out
  }
  if(is.null(type)) {
    
    shell_height %>% 
      filter(rtnd_disc == "R") %>%
      left_join(catch %>% transmute(haul_id, wt = round_weight), by = join_by(haul_id)) %>%
      bind_rows(shell_height %>%
                  filter(rtnd_disc == "D") %>%
                  left_join(bycatch %>% transmute(haul_id, 
                                                  wt = (disc_wt + broken_wt + rem_disc_wt) / sample_hrs * dredge_hrs), 
                            by = join_by(haul_id))) %>%
      group_by_at(c("season", "scal_year", by)) %>%
      mutate(n_meas = n(),
             total_wt = sum(wt, na.rm = T)) %>% ungroup %>%
      group_by_at(c("season", "scal_year", by, "sh")) %>%
      reframe(n = n(),
              n_meas = mean(n_meas),
              wt = sum(wt, na.rm = T),
              total_wt = mean(total_wt),
              p = wt / total_wt) -> out
    
  }
      
  return(out)
}














