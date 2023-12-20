pacman::p_load(here, tidyverse)

files_10i_10mp <- list.files(path = here("./data/sim_data_10i_10pm"), 
                    pattern = ".dat", recursive = T, full.names = T)

merged_data_10i_10mp <- read_delim(file = files_10i_10mp, col_names = F, delim = " ", na = "-99") |>
  bind_rows()

merged_data_10i_10mp <- merged_data_10i_10mp |>
  mutate(id = rep(c(1:1000), times = 1000), # make case id (1:1000 per dataset)
         dataset = rep(c(1:1000), each = 1000)) |> # make dataset id 
  relocate(dataset, .before = 1) |>
  relocate(id, .after = dataset) |>
  rowwise() |>
  mutate(mean = mean(c_across(c(X1:X10)), na.rm = T))

