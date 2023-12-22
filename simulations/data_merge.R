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
here(0)
desc_10i_10mp <- merged_data_10i_10mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_desc_10i_10mp <- desc_10i_10mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_10i_10mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_10i_10mp, file = here("./analyses/descriptives_10i_10mp/overall_descriptives.csv"))
  
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

write_csv(overall_prorated_desc_10i_10mp, file = here("./analyses/descriptives_10i_10mp/prorated_descriptives.csv"))

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

write_csv(overall_desc_10i_25mp, file = here("./analyses/descriptives_10i_25mp/overall_descriptives.csv"))

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

write_csv(overall_prorated_desc_10i_25mp, file = here("./analyses/descriptives_10i_25mp/prorated_descriptives.csv"))

# 10 items 50% missing ----


files_10i_50mp <- list.files(path = here("./data/sim_data_10i_50pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_10i_50mp <- read_delim(file = files_10i_50mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_10i_50mp <- merged_data_10i_50mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_10i_50mp <- merged_data_10i_50mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_desc_10i_50mp <- desc_10i_50mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_10i_50mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_10i_50mp, file = here("./analyses/descriptives_10i_50mp/overall_descriptives.csv"))

merged_data_10i_50mp <- merged_data_10i_50mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X10)), na.rm = T),
         mean = mean(c_across(c(X1:X10)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X10)))),
         prorated = total/n_complete)

prorated_desc_10i_50mp <- merged_data_10i_50mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_prorated_desc_10i_50mp <- prorated_desc_10i_50mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_10i_50mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_10i_50mp, file = here("./analyses/descriptives_10i_50mp/prorated_descriptives.csv"))

# 10 items 75% missing ----


files_10i_75mp <- list.files(path = here("./data/sim_data_10i_75pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_10i_75mp <- read_delim(file = files_10i_75mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_10i_75mp <- merged_data_10i_75mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_10i_75mp <- merged_data_10i_75mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_desc_10i_75mp <- desc_10i_75mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_10i_75mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_10i_75mp, file = here("./analyses/descriptives_10i_75mp/overall_descriptives.csv"))

merged_data_10i_75mp <- merged_data_10i_75mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X10)), na.rm = T),
         mean = mean(c_across(c(X1:X10)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X10)))),
         prorated = total/n_complete)

prorated_desc_10i_75mp <- merged_data_10i_75mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_prorated_desc_10i_75mp <- prorated_desc_10i_75mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_10i_75mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_10i_75mp, file = here("./analyses/descriptives_10i_75mp/prorated_descriptives.csv"))

# 10 items 90% missing ----

files_10i_90mp <- list.files(path = here("./data/sim_data_10i_90pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_10i_90mp <- read_delim(file = files_10i_90mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_10i_90mp <- merged_data_10i_90mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_10i_90mp <- merged_data_10i_90mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_desc_10i_90mp <- desc_10i_90mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_10i_90mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_10i_90mp, file = here("./analyses/descriptives_10i_90mp/overall_descriptives.csv"))

merged_data_10i_90mp <- merged_data_10i_90mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X10)), na.rm = T),
         mean = mean(c_across(c(X1:X10)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X10)))),
         prorated = total/n_complete)

prorated_desc_10i_90mp <- merged_data_10i_90mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X10)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X10)), na.rm = T)) 

overall_prorated_desc_10i_90mp <- prorated_desc_10i_90mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_10i_90mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_10i_90mp, file = here("./analyses/descriptives_10i_90mp/prorated_descriptives.csv"))

# 15 items 10% missing ----

files_15i_10mp <- list.files(path = here("./data/sim_data_15i_10pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_15i_10mp <- read_delim(file = files_15i_10mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_15i_10mp <- merged_data_15i_10mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_15i_10mp <- merged_data_15i_10mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_desc_15i_10mp <- desc_15i_10mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_15i_10mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_15i_10mp, file = here("./analyses/descriptives_15i_10mp/overall_descriptives.csv"))

merged_data_15i_10mp <- merged_data_15i_10mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X15)), na.rm = T),
         mean = mean(c_across(c(X1:X15)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X15)))),
         prorated = total/n_complete)

prorated_desc_15i_10mp <- merged_data_15i_10mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_prorated_desc_15i_10mp <- prorated_desc_15i_10mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_15i_10mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_15i_10mp, file = here("./analyses/descriptives_15i_10mp/prorated_descriptives.csv"))

# 15 items 25% missing ----

files_15i_25mp <- list.files(path = here("./data/sim_data_15i_25pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_15i_25mp <- read_delim(file = files_15i_25mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_15i_25mp <- merged_data_15i_25mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_15i_25mp <- merged_data_15i_25mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_desc_15i_25mp <- desc_15i_25mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_15i_25mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_15i_25mp, file = here("./analyses/descriptives_15i_25mp/overall_descriptives.csv"))

merged_data_15i_25mp <- merged_data_15i_25mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X15)), na.rm = T),
         mean = mean(c_across(c(X1:X15)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X15)))),
         prorated = total/n_complete)

prorated_desc_15i_25mp <- merged_data_15i_25mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_prorated_desc_15i_25mp <- prorated_desc_15i_25mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_15i_25mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_15i_25mp, file = here("./analyses/descriptives_15i_25mp/prorated_descriptives.csv"))

# 15 items 50% missing ----

files_15i_50mp <- list.files(path = here("./data/sim_data_15i_50pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_15i_50mp <- read_delim(file = files_15i_50mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_15i_50mp <- merged_data_15i_50mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_15i_50mp <- merged_data_15i_50mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_desc_15i_50mp <- desc_15i_50mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_15i_50mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_15i_50mp, file = here("./analyses/descriptives_15i_50mp/overall_descriptives.csv"))

merged_data_15i_50mp <- merged_data_15i_50mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X15)), na.rm = T),
         mean = mean(c_across(c(X1:X15)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X15)))),
         prorated = total/n_complete)

prorated_desc_15i_50mp <- merged_data_15i_50mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_prorated_desc_15i_50mp <- prorated_desc_15i_50mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_15i_50mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_15i_50mp, file = here("./analyses/descriptives_15i_50mp/prorated_descriptives.csv"))

# 15 items 75% missing ----

files_15i_75mp <- list.files(path = here("./data/sim_data_15i_75pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_15i_75mp <- read_delim(file = files_15i_75mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_15i_75mp <- merged_data_15i_75mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_15i_75mp <- merged_data_15i_75mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_desc_15i_75mp <- desc_15i_75mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_15i_75mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_15i_75mp, file = here("./analyses/descriptives_15i_75mp/overall_descriptives.csv"))

merged_data_15i_75mp <- merged_data_15i_75mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X15)), na.rm = T),
         mean = mean(c_across(c(X1:X15)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X15)))),
         prorated = total/n_complete)

prorated_desc_15i_75mp <- merged_data_15i_75mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_prorated_desc_15i_75mp <- prorated_desc_15i_75mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_15i_75mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_15i_75mp, file = here("./analyses/descriptives_15i_75mp/prorated_descriptives.csv"))

# 15 items 90% missing ----

files_15i_90mp <- list.files(path = here("./data/sim_data_15i_90pm"), 
                             pattern = ".dat", recursive = T, full.names = T)

merged_data_15i_90mp <- read_delim(file = files_15i_90mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_15i_90mp <- merged_data_15i_90mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset)

desc_15i_90mp <- merged_data_15i_90mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_desc_15i_90mp <- desc_15i_90mp |>
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(desc_15i_90mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_desc_15i_90mp, file = here("./analyses/descriptives_15i_90mp/overall_descriptives.csv"))

merged_data_15i_90mp <- merged_data_15i_90mp |>
  rowwise() |>
  mutate(total = sum(c_across(c(X1:X15)), na.rm = T),
         mean = mean(c_across(c(X1:X15)), na.rm = T),
         n_complete = sum(!is.na(c_across(c(X1:X15)))),
         prorated = total/n_complete)

prorated_desc_15i_90mp <- merged_data_15i_90mp |>
  group_by(dataset) |>
  summarise(scale_mean = mean(c_across(c(X1:X15)), na.rm = T),
            scale_sd = sd(c_across(c(X1:X15)), na.rm = T)) 

overall_prorated_desc_15i_90mp <- prorated_desc_15i_90mp |> 
  summarise(mean_avg = mean(scale_mean),
            se_avg = (mean_avg / sqrt(nrow(merged_data_15i_90mp))),
            ci_lower = (mean_avg - (1.96*se_avg)),
            ci_upper = (mean_avg + (1.96*se_avg)))

write_csv(overall_prorated_desc_15i_90mp, file = here("./analyses/descriptives_15i_90mp/prorated_descriptives.csv"))