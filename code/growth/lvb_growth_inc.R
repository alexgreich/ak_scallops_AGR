# structure growth increment data for gmacs
## tyler jackson
## 12/26/2024

# load ----

library(tidyverse)
library(lme4)

# data ----

ages <- read_csv("./data/observer/age/age_data_observer_shells_1996_2015.csv") %>%
  filter(!is.na(annulus), !is.na(annulus_height)) %>%
  transmute(haul_id, shell_num, shell_height, tot_annuli, annulus, annulus_height) %>%
  right_join(read_csv("./data/observer/age/fish_log_data_1996_2015.csv") %>%
              transmute(fishery, district, haul_id), .)


# format data by district ----
 
ages %>% 
  group_by(haul_id, shell_num) %>% nest() %>% #pull(data) %>% .[[1]] -> data
  mutate(data = purrr::map(data, function(data) {
    data %>% 
      rename(ann_1 = annulus,
             sh_1 = annulus_height) %>%
      mutate(ann_2 = lead(ann_1),
             sh_2 = lead(sh_1)) %>%
      filter(sh_1 >= 30,
             !is.na(sh_2),
             (ann_2 - ann_1) == 1) %>%
      mutate(growth_inc = (sh_2 - sh_1) / (ann_2 - ann_1))
  })) %>%
  unnest(data) -> g_inc

g_inc %>% 
  filter(district == "KSH") %>%
  filter(tot_annuli > 5) %>%
  mutate(shell_id = paste0(shell_num, "_", haul_id)) -> tmp

# # define the growth increment function
# f_lvb_inc <- function(sh_1, m, b){
#   # define lvb parameters
#   K = -log(m)
#   Linf = b / (1 - m)
#   
#   sh_2 = exp(-K)*sh_1 + Linf*(1 - exp(-K))
#   return(sh_2)
# }
  
lmer(sh_2 ~ sh_1 + (sh_1|shell_id), data = tmp) -> mod

fixef(mod)
ranef(mod)

m <- as.numeric(fixef(mod)[2])
b <- as.numeric(fixef(mod)[1])
var_a <- as_tibble(VarCorr(mod))$vcov[2]
var_b <- as_tibble(VarCorr(mod))$vcov[1]
cov_ab <- as_tibble(VarCorr(mod))$vcov[3]

K = -log(m) + var_a / 2*m^2
Linf = (b / (1 - m)) + (1 / (1 - m)^2)*((b * var_a)/(1 - m) + cov_ab)

ages %>%
  distinct(district, haul_id, shell_num, shell_height, tot_annuli) %>%
  filter(district == "KSH") -> ages_tmp


tibble(t = 2:20,
       sh = Linf * (1 - exp(-K*t))) %>%
  ggplot()+
  geom_jitter(data = ages_tmp, aes(x = tot_annuli, y = shell_height))+
  geom_line(aes(x = t, y = sh), color = 2)








