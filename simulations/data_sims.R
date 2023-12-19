pacman::p_load(here, tidyverse, simsem, lavaan)

# lavaan syntax version of {simsem} ----

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

analysis_model <- "
f1 =~ NA*i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10

f1 ~~ 1*f1
"

sim_output_10pm <- sim(1000, analysis_model, n = 1000, generate = pop_model, 
                       lavaanfun = "cfa", seed = 13131, multicore = T, pmMCAR = 0.10)

summary(sim_output_10pm)
summaryFit(sim_output_10pm)
summaryParam(sim_output_10pm)
plotCutoff(sim_output_10pm, 0.05)

sim_output_25pm <- sim(1000, analysis_model, n = 1000, generate = pop_model, 
                       lavaanfun = "cfa", seed = 13131, multicore = T, pmMCAR = 0.25)

summary(sim_output_25pm)
summaryFit(sim_output_25pm)
summaryParam(sim_output_25pm)
plotCutoff(sim_output_25pm, 0.05)

sim_output_50pm <- sim(1000, analysis_model, n = 1000, generate = pop_model, 
                       lavaanfun = "cfa", seed = 13131, multicore = T, pmMCAR = 0.5)

summary(sim_output_50pm)
summaryFit(sim_output_50pm)
summaryParam(sim_output_50pm)
plotCutoff(sim_output_50pm, 0.05)

sim_output_75pm <- sim(1000, analysis_model, n = 1000, generate = pop_model, 
                       lavaanfun = "cfa", seed = 13131, multicore = T, pmMCAR = 0.75)

summary(sim_output_75pm)
summaryFit(sim_output_75pm)
summaryParam(sim_output_75pm)
plotCutoff(sim_output_75pm, 0.05)

sim_output_90pm <- sim(1000, analysis_model, n = 1000, generate = pop_model, 
                       lavaanfun = "cfa", seed = 13131, multicore = T, pmMCAR = 0.90)

summary(sim_output_90pm)
summaryFit(sim_output_90pm)
summaryParam(sim_output_90pm)
plotCutoff(sim_output_90pm, 0.05)