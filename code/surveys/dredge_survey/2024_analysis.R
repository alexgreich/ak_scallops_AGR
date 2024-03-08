# notes ----
# 2024 survey analysis
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
                csv_dir = "./output/dredge_survey/2024", plot_dir = "./figures/dredge_survey/2024")
## round biomass
f_est_round_biomass(catch, strata, write_csv = T, plot = T, units = "t",
                    csv_dir = "./output/dredge_survey/2024", plot_dir = "./figures/dredge_survey/2024")

## meat biomass
f_est_meat_biomass(catch, strata, shad, shaw, write_csv = T, plot = T, units = "t", size_range = c(100, NA),
                   csv_dir = "./output/dredge_survey/2024", plot_dir = "./figures/dredge_survey/2024")

# shell height distribution ----

f_plot_sh_comp(shad, plot_dir = "./figures/dredge_survey/2024")


# shell height / meat weight plot ----

f_plot_shmw(shaw, all_districts = F, plot_dir = "./figures/dredge_survey/2024")


# gonad condition table ----

f_gonad_condition(shaw, csv_dir = "./output/dredge_survey/2024", plot_dir = "./figures/dredge_survey/2024")

# pathologies ----

# meat condition
f_meat_condition(shaw, csv_dir = "./output/dredge_survey/2024")

# shell worm
f_shell_worm(shaw, csv_dir = "./output/dredge_survey/2024")

# mud blister
f_mud_blister(shaw, csv_dir = "./output/dredge_survey/2024")







