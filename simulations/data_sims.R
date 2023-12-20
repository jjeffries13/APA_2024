pacman::p_load(here, tidyverse, simsem, lavaan, mice, skimr)

# 10 item scale ----

## 10 item population model ----

pop_model <- "
f1 =~ NA*i1 + 0.7*i1 + 0.7*i2 + 0.7*i3 + 0.7*i4 + 0.7*i5 + 
0.7*i6 + 0.7*i7 + 0.7*i8 + 0.7*i9 + 0.7*i10

f1 ~~ 1*f1
i1 ~~ 0.51*i1
i2 ~~ 0.51*i2
i3 ~~ 0.51*i3
i4 ~~ 0.51*i4
i5 ~~ 0.51*i5
i6 ~~ 0.51*i6
i7 ~~ 0.51*i7
i8 ~~ 0.51*i8
i9 ~~ 0.51*i9
i10 ~~ 0.51*i10
"

## 10 item sample model ----

analysis_model <- "
f1 =~ NA*i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10

f1 ~~ 1*f1

i1 ~ 1*1
i2 ~ 1*1
i3 ~ 1*1
i4 ~ 1*1
i5 ~ 1*1
i6 ~ 1*1
i7 ~ 1*1
i8 ~ 1*1
i9 ~ 1*1
i10 ~ 1*1
"

sim_output_10pm <- sim(nRep = 1000, model = analysis_model, n = 1000,
                       generate = pop_model, lavaanfun = "cfa",
                       seed = 13131, multicore = T, pmMCAR = 0.10)

summary(sim_output_10pm)

# set.seed(13131) 
# 
# sim_data_10i <- generate(model = analysis_model, n = 1000, maxDraw = 1000, empirical = T)
# 
# skim(sim_data_10i)

## 10% missingness, 1000 .dat datasets(sim_data_10i_10pm) ----

# exportData(nRep = 1000, model = analysis_model, n = 1000, program = "Mplus", 
#            fileStem = "sim_10i_10pm", missCode = -99, seed = 13131, multicore = T, pmMCAR = 0.10)

# sim_data_10i_10pm <- sim_data_10i |>
#   ampute(prop = 0.10, mech = "MCAR")
# 
# data_cond1 <- sim_data_10i_10pm$amp
# 
# skim(data_cond1)

## 25% missingness, 1000 .dat datasets (sim_data_10i_25pm) ----

# exportData(nRep = 1000, model = analysis_model, n = 1000, program = "Mplus",
#            fileStem = "sim_10i_25pm", missCode = -99, seed = 13131, multicore = T, pmMCAR = 0.25)

# sim_data_10i_25pm <- sim_data_10i |>
#   ampute(prop = 0.25, mech = "MCAR")
# 
# data_cond2 <- sim_data_10i_25pm$amp
# 
# skim(data_cond2)

## 50% missingness, 1000 .dat datasets (sim_data_10i_50pm) ----

# exportData(nRep = 1000, model = analysis_model, n = 1000, program = "Mplus",
#            fileStem = "sim_10i_50pm", missCode = -99, seed = 13131, multicore = T, pmMCAR = 0.50)

# sim_data_10i_50pm <- sim_data_10i |>
#   ampute(prop = 0.5, mech = "MCAR")
# 
# data_cond3 <- sim_data_10i_50pm$amp
# 
# skim(data_cond3)

## 75% missingness, 1000 .dat datasets (sim_data_10i_75pm) ----

# exportData(nRep = 1000, model = analysis_model, n = 1000, program = "Mplus",
#            fileStem = "sim_10i_75pm", missCode = -99, seed = 13131, multicore = T, pmMCAR = 0.75)

# sim_data_10i_75pm <- sim_data_10i |>
#   ampute(prop = 0.75, mech = "MCAR")
# 
# data_cond4 <- sim_data_10i_75pm$amp
# 
# skim(data_cond4)

## 90% missingness, 1000 .dat datasets (sim_data_10i_90pm) ----

# exportData(nRep = 1000, model = analysis_model, n = 1000, program = "Mplus",
#            fileStem = "sim_10i_90pm", missCode = -99, seed = 13131, multicore = T, pmMCAR = 0.90)

# sim_data_10i_90pm <- sim_data_10i |>
#   ampute(prop = 0.90, mech = "MCAR")
# 
# data_cond5 <- sim_data_10i_90pm$amp
# 
# skim(data_cond5)

# 15 item scale ----

## 15 item population model ----

pop_model2 <- "
f1 =~ NA*i1 + 0.7*i1 + 0.7*i2 + 0.7*i3 + 0.7*i4 + 0.7*i5 + 
0.7*i6 + 0.7*i7 + 0.7*i8 + 0.7*i9 + 0.7*i10 + 0.7*i11 + 0.7*i12 +
0.7*i13 + 0.7*i14 + 0.7*i15

f1 ~~ 1*f1
i1 ~~ 0.51*i1
i2 ~~ 0.51*i2
i3 ~~ 0.51*i3
i4 ~~ 0.51*i4
i5 ~~ 0.51*i5
i6 ~~ 0.51*i6
i7 ~~ 0.51*i7
i8 ~~ 0.51*i8
i9 ~~ 0.51*i9
i10 ~~ 0.51*i10
i11 ~~ 0.51*i11
i12 ~~ 0.51*i12
i13 ~~ 0.51*i13
i14 ~~ 0.51*i14
i15 ~~ 0.51*i15
"

## 15 item sample model ----

analysis_model2 <- "
f1 =~ NA*i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + 
i11 + i12 + i13 + i14 + i15

f1 ~~ 1*f1

i1 ~ 1*1
i2 ~ 1*1
i3 ~ 1*1
i4 ~ 1*1
i5 ~ 1*1
i6 ~ 1*1
i7 ~ 1*1
i8 ~ 1*1
i9 ~ 1*1
i10 ~ 1*1
i11 ~ 1*1
i12 ~ 1*1
i13 ~ 1*1
i14 ~ 1*1
i15 ~ 1*1
"

## 10% missing ----

exportData(nRep = 1000, model = analysis_model2, n = 1000, program = "Mplus",
           fileStem = "sim_15i_10pm", missCode = -99, seed = 13131, multicore = T, 
           numProc = NULL, pmMCAR = 0.10)

## 25% missing ----

exportData(nRep = 1000, model = analysis_model2, n = 1000, program = "Mplus",
           fileStem = "sim_15i_25pm", missCode = -99, seed = 13131, multicore = T, 
           pmMCAR = 0.25)

## 50% missing ----

exportData(nRep = 1000, model = analysis_model2, n = 1000, program = "Mplus",
           fileStem = "sim_15i_50pm", missCode = -99, seed = 13131, multicore = T, 
           pmMCAR = 0.50)


## 75% missing ----

exportData(nRep = 1000, model = analysis_model2, n = 1000, program = "Mplus",
           fileStem = "sim_15i_75pm", missCode = -99, seed = 13131, multicore = T, 
           pmMCAR = 0.75)

## 90% missing ----

exportData(nRep = 1000, model = analysis_model2, n = 1000, program = "Mplus",
           fileStem = "sim_15i_90pm", missCode = -99, seed = 13131, multicore = T, 
           pmMCAR = 0.90)

# Archive ----
# summary(sim_output_10pm)
# summaryFit(sim_output_10pm)
# summaryParam(sim_output_10pm)
# plotCutoff(sim_output_10pm, 0.05)
#
# sim_output_25pm <- sim(nRep = 1000, model = analysis_model, n = 1000, 
#                        generate = pop_model, lavaanfun = "cfa", 
#                        seed = 13131, multicore = T, pmMCAR = 0.25)
# 
# # summary(sim_output_25pm)
# # summaryFit(sim_output_25pm)
# # summaryParam(sim_output_25pm)
# # plotCutoff(sim_output_25pm, 0.05)
# 
# sim_output_50pm <- sim(nRep = 1000, model = analysis_model, n = 1000, 
#                        generate = pop_model, lavaanfun = "cfa", 
#                        seed = 13131, multicore = T, pmMCAR = 0.5)
# 
# # summary(sim_output_50pm)
# # summaryFit(sim_output_50pm)
# # summaryParam(sim_output_50pm)
# # plotCutoff(sim_output_50pm, 0.05)
# 
# sim_output_75pm <- sim(nRep = 1000, model = analysis_model, n = 1000, 
#                        generate = pop_model, lavaanfun = "cfa", 
#                        seed = 13131, multicore = T, pmMCAR = 0.75)
# 
# # summary(sim_output_75pm)
# # summaryFit(sim_output_75pm)
# # summaryParam(sim_output_75pm)
# # plotCutoff(sim_output_75pm, 0.05)
# 
# sim_output_90pm <- sim(nRep = 1000, model = analysis_model, n = 1000, 
#                        generate = pop_model, lavaanfun = "cfa", 
#                        seed = 13131, multicore = T, pmMCAR = 0.90)
# 
# # summary(sim_output_90pm)
# # summaryFit(sim_output_90pm)
# # summaryParam(sim_output_90pm)
# # plotCutoff(sim_output_90pm, 0.05)