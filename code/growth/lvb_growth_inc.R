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
      filter(sh_1 >= 41,
             !is.na(sh_2),
             (ann_2 - ann_1) == 1) %>%
      mutate(growth_inc = (sh_2 - sh_1) / (ann_2 - ann_1))
  })) %>%
  unnest(data) -> g_inc

g_inc %>% 
  filter(district == "KSH") %>%
  filter(tot_annuli > 5) %>%
  mutate(shell_id = paste0(shell_num, "_", haul_id)) -> tmp

# random  b ----

tmp %>%
  lmer(sh_2 ~ sh_1 + (1|shell_id), data = .) -> mod

m <- as.numeric(fixef(mod)[2])
b <- as.numeric(fixef(mod)[1])
var_a <- as_tibble(VarCorr(mod))$vcov[2]
var_a = 0
var_b <- as_tibble(VarCorr(mod))$vcov[1]
cov_ab <- as_tibble(VarCorr(mod))$vcov[3]
cov_ab = 0

K = -log(m) + var_a / 2*m^2
Linf = (b / (1 - m)) + (1 / (1 - m)^2)*((b * var_a)/(1 - m) + cov_ab)

varK = (log(m)^2 + ((1 - log(m)) * (var_a / m^2))) - ((log(m)^2 - (var_a * log(m)) / m^2))
seK = sqrt(varK)

varLinf = (1 / (1-m)^2)*((var_a*b^2/(1-m)^2) + var_b + ((2*b)/(1-m))*cov_ab)
seLinf = sqrt(varLinf)

(1 / (1-m)^2)*var_b



# approximate to log-normal
sqrt(log(1+(seLinf/Linf)^2))
sqrt(log(1+(seK/K)^2))
