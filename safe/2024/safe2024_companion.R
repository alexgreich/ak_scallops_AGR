# notes ----

## safe2024.Rmd companion script
## tyler jackson
## 1/22/2024

source("code/observer/scallop_obs_functions.R")

# mgmt quantities table ----

# table 
read_csv("./data/observer/metadata/ScallopGHLs_ByFisheryAndDistrict.csv") %>%
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", 
                    ifelse(district %in% c("D", "D16"), "YAK", 
                    ifelse(district %in% c("EKI", "WKI"), "E", district)))) %>% 
  f_add_season(., fishery_col = "fishery") %>%
  group_by(season, district) %>% summarise(ghl = sum(ghl)) %>% ungroup %>% #print(n = 1000)
  # join to catch and discard
  left_join(read_csv("./output/safe/2024/meat_weght_removals_time_series_1990_present.csv") %>%
               mutate(season = paste0(year, "/", substring(year+1, 3, 4)))) %>%
  # fill in missing year
  mutate(year = as.numeric(substring(season, 1, 4))) %>%
  # summarise
  group_by(season, year) %>%
  summarise(ghl = sum(ghl, na.rm = T),
            retained = sum(landed_lb_mw, na.rm = T),
            total = retained + sum(disc_lb_mw, na.rm = T)) %>% ungroup %>%
  mutate(ofl = case_when(year %in% 1996 ~ 1.100,
                         year %in% 1996:1998 ~ 1.800,
                         year %in% 1999:2010 ~ 1.240,
                         year > 2010 ~ 1.284),
         abc = 0.9 * ofl) -> mgmt_qt
# write csv
write_csv(mgmt_qt, "./output/safe/2024/mgmt_quantities_1990_present.csv")




# ecosystem component bycatch ----

## corrected season, district, and haul id data
lapply(list.files("./data/observer/catch", pattern = "CatchByHaul", full.names = T), read.csv) %>% 
  do.call("bind_rows", .) %>% as_tibble() %>%
  drop_na(Haul_ID) %>%
  f_catch_rename %>%
  ## add Season to data
  f_add_season %>%
  ## classify Karluk bed as KSW district instead of KSH
  f_revise_district %>% 
  ## coerce date to date class
  mutate(set_date = lubridate::mdy(set_date)) %>%
  ## remove tows with zero dredge hours (logbook mistake)
  filter(dredge_hrs != 0) %>%
  ## select columns to join
  dplyr::select(season, haul_id, district, dredge_hrs, haul_sampled) %>%
  rename_all(tolower) -> obs_haul_info

## fishery catch comp
lapply(list.files("./data/observer/catch_comp", pattern = "haulComp", full.names = T), read.csv) %>% 
  do.call("bind_rows", .) %>% as_tibble() %>%
  dplyr::select(Haul_ID, RACE_Code, Species_Name, `Weight.lbs.`) %>%
  rename_all(tolower) %>%
  right_join(obs_haul_info, by = "haul_id") %>%
  rename(wt_lb = `weight.lbs.`) -> fish_catch_comp


taxa <- c("Weathervane Scallop", "Rockfishes", "Gadids", "Pacific Halibut", "Flatfishes", "Misc. Roundfishes",
          "Skates", "Sharks", "Commercial Crabs", "Misc. Crabs", "Hermit Crabs", "Shrimp",
          "Misc. Bivalves", "Gastropods", "Sea Stars", "Brittle/Basket Stars", "Misc. Bentic Invert.", "Jellyfishes", 
          "Octopus/Squid", "Natural Debris", "Man-made Debris")
fill_color = c(RColorBrewer::brewer.pal(9, "Blues")[3:7],
               RColorBrewer::brewer.pal(9, "Greens")[3:4],
               RColorBrewer::brewer.pal(9, "Oranges")[3:6],
               RColorBrewer::brewer.pal(9, "Purples")[3:7],
               "peachpuff1", "cornflowerblue", "grey40", "grey60")


fish_catch_comp %>%
  mutate(taxon = case_when(race_code %in% 74120 ~ "Weathervane Scallop",
                           race_code %in% 30000:36000 ~ "Rockfishes",
                           race_code %in% 66000:67499 ~ "Shrimp",
                           race_code %in% c(74000:74119, 74121:75799,77011) ~ "Misc. Bivalves",
                           race_code %in% 71000:73999 ~ "Gastropods",
                           race_code %in% c(1:3,21:149, 710, 10400:21598, 21752:29999) ~ "Misc. Roundfishes",
                           race_code %in% 21705:21747 ~ "Gadids",
                           race_code %in% 10120 ~ "Pacific Halibut",
                           race_code %in% c(10001:10119, 10121:10295) ~ "Flatfishes",
                           race_code %in% 400:495 ~ "Skates",
                           race_code %in% c(80000:80159, 80160, 80161:82101) ~ "Sea Stars",
                           race_code %in% 83000:83701 ~ "Brittle/Basket Stars",
                           race_code %in% c(40011, 41100:44122, 82102:82999, 85000:99990, 50000:65210, 70100) ~ "Misc. Bentic Invert.",
                           race_code %in% c(68000:68012, 68050:68521, 68577:68781, 69250:69285, 69312:69320, 69325:69341,
                                            69520:69550, 68040, 68575) ~ "Misc. Crabs",
                           race_code %in% 69000:69200 ~ "Hermit Crabs",
                           race_code %in% c(68020, 68541:68570, 68580:68590, 69290:69310, 
                                            69321:69323, 69400:69401) ~ "Commercial Crabs",
                           race_code %in% 150:355 ~ "Sharks",
                           race_code %in% 0 ~ "Man-made Debris",
                           race_code %in% c(99990:99999) ~ "Natural Debris",
                           race_code %in% 78010:79513 ~ "Octopus/Squid",
                           race_code %in% 40500 ~ "Jellyfishes"),
         taxon = factor(taxon, levels = taxa)) -> byctach_w_taxon

# Bycacth by ecoregion
# scallops compared to other
byctach_w_taxon %>%
  mutate(taxon = ifelse(race_code != 74120, "Other", as.character(taxon))) %>%
  filter(!is.na(taxon)) %>%
  mutate(ecoregion = case_when(district == "Q" ~ "Area Q (EBS)",
                               district %in% c("O", "UB", "WC", "C", "KSH", "KNE", 
                                               "KSE", "KSW", "KSEM") ~ "Areas O, M, K (Western GOA)",
                               district %in% c("WKI", "EKI", "YAK") ~ "Areas E, D (Eastern GOA)"),
         ecoregion = factor(ecoregion, levels = c("Area Q (EBS)", "Areas O, M, K (Western GOA)", "Areas E, D (Eastern GOA)"))) %>%
  group_by(season, ecoregion) %>%
  mutate(bag_wt = sum(wt_lb, na.rm = T)) %>%
  group_by(season, ecoregion, taxon) %>%
  summarise(wt_lb = sum(wt_lb, na.rm = T),
            wt_prop = wt_lb / mean(bag_wt),
            bag_wt = mean(bag_wt)) %>%
  
  ggplot()+
  geom_bar(aes(x = season, y = wt_prop, fill = taxon), position = "stack", stat = "identity") +
  scale_fill_manual(values = c("grey40", "grey80")) +
  labs(x = NULL, y = "Proportion by Weight", fill = NULL) + 
  facet_wrap(~ecoregion, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x

byctach_w_taxon %>%
  mutate(ecoregion = case_when(district == "Q" ~ "Area Q (EBS)",
                               district %in% c("O", "UB", "WC", "C", "KSH", "KNE", 
                                               "KSE", "KSW", "KSEM") ~ "Areas O, M, K (Western GOA)",
                               district %in% c("WKI", "EKI", "YAK") ~ "Areas E, D (Eastern GOA)"),
         ecoregion = factor(ecoregion, levels = c("Area Q (EBS)", "Areas O, M, K (Western GOA)", "Areas E, D (Eastern GOA)"))) %>%
  filter(race_code != 74120) %>%
  group_by(season, ecoregion) %>%
  mutate(bag_wt = sum(wt_lb, na.rm = T)) %>%
  group_by(season, ecoregion, taxon) %>%
  summarise(wt_lb = sum(wt_lb, na.rm = T),
            wt_prop = wt_lb / mean(bag_wt),
            bag_wt = mean(bag_wt)) %>%
  
  ggplot()+
  geom_bar(aes(x = season, y = wt_prop, fill = taxon), position = "stack", stat = "identity") +
  scale_fill_manual(values = fill_color) +
  labs(x = NULL, y = "Proportion by Weight", fill = NULL) + 
  facet_wrap(~ecoregion, ncol = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) -> y

ggsave("./figures/safe/2024/fishery_bycatch_comp.png", plot = x + y,
       height = 6, width = 10, units = "in")





# catch data ----

## ghl
read_csv("./data/observer/metadata/ScallopGHLs_ByFisheryAndDistrict.csv") %>%
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district),
         district = ifelse(district %in% c("D16", "D"), "YAK", district),
         district = ifelse(district %in% c("KAM", "KAMN", "KAMS"), "H", district)) %>%
  f_add_season() %>% 
  transmute(year = as.numeric(substring(season, 1, 4)), district, ghl) %>%
  mutate(district = ifelse((year < 2000 & (district %in% c("E", "EKI", "WKI"))), "E", district)) %>%
  group_by(year, district) %>%
  summarise(ghl = sum(ghl)) %>% ungroup -> ghl

## scallop catch data 2009/10 - present
do.call(bind_rows, lapply(list.files("data/observer/catch/", full.names = T), read_csv)) %>%
  f_clean_catch() -> catch_tmp
catch_tmp %>%
  # combine area M
  mutate(district = ifelse(district %in% c("C", "UB", "WC"), "M", district),
         year = as.numeric(substring(season, 1, 4))) %>%
  group_by(year, district) %>%
  summarise(landed_lb_rnd = sum(round_weight, na.rm = T),
            landed_lb_mw = sum(meat_weight, na.rm = T),
            dredge_hrs = sum(dredge_hrs, na.rm = T)) %>% ungroup -> obs_ret

## scallop discard data 2009/10 - present
do.call(bind_rows, lapply(list.files("data/observer/bycatch/", full.names = T, pattern = ".csv"), read_csv)) %>%
  f_clean_bycatch( ., catch_tmp) -> tmp
# combine all area M
mutate(tmp, district = ifelse(district %in% c("UB", "C", "WC"), "M", district)) %>%
  # get discards by day
  group_by(season, district, adfg, set_date) %>%
  summarise(n_hauls = n(),
            sampled_hauls = sum(sample_hrs > 0),
            dredge_hrs = sum(dredge_hrs, na.rm = T),
            sample_hrs = sum(sample_hrs, na.rm = T),
            disc_count = sum(disc_count, na.rm = T),
            disc_wt = sum(disc_wt, na.rm = T),
            broken_wt = sum(broken_wt, na.rm = T),
            rem_disc_wt = sum(rem_disc_wt, na.rm = T)) %>%
  # get discards by season
  group_by(season, district) %>%
  summarise(effort = sum(dredge_hrs, na.rm = T),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt, na.rm = T) / sum(sample_hrs, na.rm = T),
            discard_lb_mw = discard_rate_lb * effort * 0.1) %>% ungroup %>%
  transmute(year = as.numeric(substring(season, 1, 4)),
            district, 
            # discard mortality 
            disc_mort_lb_mw = discard_lb_mw * 0.2) -> obs_disc

# join catch data post 2009
left_join(obs_ret, obs_disc) %>%
  right_join(ghl %>% filter(year >= 2009)) %>%
  right_join(expand_grid(year = unique(.$year), district = unique(.$district))) %>% 
  replace(is.na(.), 0) -> obs_2009_present

## old catch data from safe tables early 1990s - 2008
read_csv("./data/observer/old_catch/catch_summary_1992-2008.csv") %>%
  janitor::clean_names() %>%
  # combine YAK and D16, create year
  mutate(district = ifelse(district == "D16", "YAK", district),
         district = ifelse(district %in% c("KAM", "KAMN", "KAMS"), "H", district),
         year = as.numeric(substring(season, 1, 4)),
         district = ifelse((year < 2000 & district %in% c("E", "EKI", "WKI")), "E", district)) %>%
  group_by(year, district) %>%
  summarise(landed_lb_rnd = sum(rnd_wt, na.rm = T),
            landed_lb_mw = sum(mt_wt, na.rm = T),
            disc_mort_lb_mw = sum(mt_disc_m, na.rm = T),
            dredge_hrs = sum(dredge_hrs, na.rm = T)) %>% ungroup -> pre

## ryan burt fish ticket summary early 1990s - 2008
read_csv("./data/fish_ticket/rb_fish_ticket_summary.csv") %>%
  # get to longer format
  t() %>% as.data.frame %>%
  rownames_to_column() %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:13)%>%
  transmute(year = as.numeric(substring(District, 1, 4)),
            district = name, 
            landed_lb_mw_rb = as.numeric(value)) %>%
  # combine D and D16 
  mutate(district = ifelse(district %in% c("D16", "D"), "YAK", district)) %>%
  group_by(year, district) %>% summarise_all(sum) %>% ungroup %>%
  # remove area e after 1999
  filter(!(year > 1999 & district == "E")) -> rb_pre

## ryan burt fish ticket summary early, split out eki, wki 2000 - 2008
read_csv("./data/fish_ticket/area_e_2000_2008.csv") %>%
  filter(as.numeric(substring(season, 1, 4)) < 2009) %>%
  transmute(year = as.numeric(substring(season, 1, 4)), district, landed_lb_mwrb2 = landed_lb_mw) -> area_e
  
full_join(pre, rb_pre) %>% 
  left_join(area_e) %>%
  full_join(ghl %>% filter(year >= 1990, year <= 2008)) %>%
  transmute(year, district, ghl, 
            landed_lb_mw = ifelse(is.na(landed_lb_mw_rb), landed_lb_mwrb2, landed_lb_mw_rb),
            landed_lb_rnd = landed_lb_mw / 0.1,
            dredge_hrs = ifelse(ghl>0 & dredge_hrs==0, NA, dredge_hrs),
            disc_mort_lb_mw = ifelse(landed_lb_mw_rb>0 & disc_mort_lb_mw==0, NA, disc_mort_lb_mw)) %>%
  #right_join(expand_grid(year = unique(.$year), district = unique(.$district), ghl.$)) %>%
  replace_na(list(landed_lb_rnd = 0, landed_lb_mw = 0)) %>%
  mutate(dredge_hrs = ifelse(landed_lb_mw==0&is.na(dredge_hrs), 0, dredge_hrs)) -> ft_1990_2008
  
# save combined data
bind_rows(ft_1990_2008, obs_2009_present) %>%
  arrange(year) %>%
  write_csv("./output/safe/2024/fishery_stats_1990_present.csv")
  
# mgmt quantities table ----

read_csv("./output/safe/2024/fishery_stats_1990_present.csv") %>%
  mutate(season = paste0(year, "/", substring(year+1, 3, 4))) %>%
  # summarise
  group_by(season, year) %>%
  summarise(ghl = sum(ghl, na.rm = T),
            retained = sum(landed_lb_mw, na.rm = T),
            total = retained + sum(disc_mort_lb_mw, na.rm = T)) %>% ungroup %>%
  mutate(ofl = case_when(year %in% 1996 ~ 1.100,
                         year %in% 1996:1998 ~ 1.800,
                         year %in% 1999:2010 ~ 1.240,
                         year > 2010 ~ 1.284),
         abc = 0.9 * ofl) -> mgmt_qt
# write csv
write_csv(mgmt_qt, "./output/safe/2024/mgmt_quantities_1990_present.csv")


# statewide retained catch plot ----

read_csv("./output/safe/2024/mgmt_quantities_1990_present.csv") %>%
  mutate(ghl = ifelse(ghl == 0 | year < 1996, NA, ghl)) %>% #print(n = 1000)
  ggplot()+
  geom_bar(aes(x = factor(year), y = total, fill = "total"), width = 1, stat = "identity", color = 1)+
  geom_bar(aes(x = factor(year), y = retained, fill = "retained"), stat = "identity", color = 1, width = 1)+
  geom_line(aes(x = factor(year), y = ghl, group = 1), color = 2, linetype = 2)+
  geom_point(data = function(x){filter(x, year %in% 1996)}, aes(x = factor(year), y = ofl*1e6, group = 1), size = 6, shape = 95)+
  geom_line(data = function(x){filter(x, year %in% 1997:1998)}, aes(x = factor(year), y = ofl*1e6, group = 1))+
  geom_line(data = function(x){filter(x, year %in% 1999:2010)}, aes(x = factor(year), y = ofl*1e6, group = 1))+
  geom_line(data = function(x){filter(x, year %in% 2011:2023)}, aes(x = factor(year), y = ofl*1e6, group = 1))+
  scale_y_continuous(labels = scales::comma)+
  scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_fill_manual(values = c("grey70", "black"), labels = c("Retained", "Discard Mortality"))+
  labs(x = NULL, y = "Total Catch (Meat lb)", fill = NULL)+
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) -> x

ggsave("./figures/safe/2024/fishery_catch.png", plot = x,
       height = 3, width = 6, units = "in")


read_csv("./output/safe/2024/fishery_stats_1990_present.csv") %>%
  mutate(district = ifelse(district %in% c("EKI", "WKI"), "E", district)) %>%
  ggplot()+
  geom_bar(aes(x = year, y = landed_lb_mw, fill = district), width = 1, stat = "identity", position = "stack", color = 1)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  scale_fill_brewer(palette = "Set3")+
  geom_vline(xintercept = 1997.5, linetype = 2)+
  labs(x = NULL, y = "Total Catch (Meat lb)", fill = NULL) -> x

ggsave("./figures/safe/2024/fishery_catch_by_district.png", plot = x,
       height = 3, width = 6, units = "in")


read_csv("./output/safe/2024/fishery_stats_1990_present.csv") %>%
  filter(year %in% 1990:1997, year != 1995) %>%
  group_by(district) %>%
  summarise(landed_lb_mw = mean(landed_lb_mw, na.rm = T)) %>%
  ungroup %>%
  mutate(total = sum(landed_lb_mw),
         perc = landed_lb_mw / total * 100) %>%
  write_csv("./output/safe/2024/ref_period_avg_by_district.csv")
  
  

read_csv("./output/safe/2024/fishery_stats_1990_present.csv") %>%
  transmute(year, district, landed_lb_mw, dredge_hrs) %>%
  filter(complete.cases(.), dredge_hrs > 0) %>%
  group_by(year) %>%
  summarise(cpue = sum(landed_lb_mw) / sum(dredge_hrs)) %>%
  ggplot()+
  geom_line(aes(x = year, y = cpue))+
  geom_smooth(aes(x = year, y = cpue), method = "gam", se = F)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
  labs(x = NULL, y = "CPUE (lb / dredge hr)", fill = NULL) -> x

ggsave("./figures/safe/2024/fishery_cpue.png", plot = x,
       height = 3, width = 6, units = "in")
  


# bycatch ----
### crab bycath size data 2009/10 - Present  
catch_wiki <- do.call(bind_rows,
                      lapply(paste0("data/observer/catch/", list.files("data/observer/catch/")), read_csv))

bycatch_wiki <- do.call(bind_rows,
                        lapply(list.files("data/observer/bycatch", pattern = ".csv", full.names = T), read_csv))
## clean catch data
catch <- f_clean_catch(catch_wiki)
## clean bycatch data
bycatch <- f_clean_bycatch(bycatch_wiki, catch)

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
            retained_mt = sum(mt_wt, na.rm = T)) %>% ungroup %>%
  write_csv("./output/safe/2024/crab_bycatch_2009_present.csv")



