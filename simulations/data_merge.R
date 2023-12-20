pacman::p_load(here, tidyverse)

# 10 items 10% missing ----

files_10i_10mp <- list.files(path = here("./data/sim_data_10i_10pm"), 
                    pattern = ".dat", recursive = T, full.names = T)

merged_data_10i_10mp <- read_delim(file = files_10i_10mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_10i_10mp <- merged_data_10i_10mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

# overall_desc_10i_10mp <- merged_data_10i_10mp |>
#   summarise(overall_scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
#             overall_scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

desc_10i_10mp <- merged_data_10i_10mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_desc_10i_10mp <- desc_10i_10mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_10i_10mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))
  
merged_data_10i_10mp <- merged_data_10i_10mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X10)), na.rm = T),
    mean = mean(c_across(c(X1:X10)), na.rm = T),
    n_complete = sum(!is.na(c_across(c(X1:X10)))),
    prorated = total/n_complete)

prorated_desc_10i_10mp <- merged_data_10i_10mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_prorated_desc_10i_10mp <- prorated_desc_10i_10mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_10i_10mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

# 10 items 25% missing ----


files_10i_25mp <- list.files(path = here("./data/sim_data_10i_25pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_10i_25mp <- read_delim(file = files_10i_25mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_10i_25mp <- merged_data_10i_25mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_10i_25mp <- merged_data_10i_25mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_desc_10i_25mp <- desc_10i_25mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_10i_25mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

merged_data_10i_25mp <- merged_data_10i_25mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X10)), na.rm = T),
         mean = mean(c_across(c(X1:X10)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X10)))),
         prorated = total/n_complete)

prorated_desc_10i_25mp <- merged_data_10i_25mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_prorated_desc_10i_25mp <- prorated_desc_10i_25mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_10i_25mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))