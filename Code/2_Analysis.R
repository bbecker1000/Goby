source("1_DataCleaning.R")

library(lme4)
library(sjPlot)

hist(goby_master$Sum_TW)
hist(goby_master$Sum_TW/(goby_master$volume))

#big test model

m1 <- glmer(Sum_TW ~ Zone + Year + Sum_SB + Sum_SC + offset(log(volume+0.001)) + (1|Zone), 
            family = negative.binomial(1),
           # data = subset(goby_master, Season == "Fall")
            data = goby_master)

summary(m1)
plot_model(m1, type = "eff")



