source("1_DataCleaning.R")

library(lme4)
library(sjPlot)

hist(goby_master$Sum_TW)
hist(goby_master$Sum_TW/(goby_master$volume))
goby_master
#Need to get in rain and wind data !


hist(goby_master$Water_temp_1)
hist(goby_master$Sum_SC)
hist(goby_master$Sum_SB)
plot(goby_master$Zone)


#big test model

m1 <- glmer(Sum_TW ~ Zone + 
              Dom_substrate +
              scale(Year) + 
              Season + 
              scale(Sum_SB) + 
              scale(Sum_SC) + 
              Water_temp_1 + 
              (1|Zone) + (1|Season),
            data = goby_master,
            family = negative.binomial(1),  #poisson
            offset=log(volume))

summary(m1)
plot_model(m1, type = "eff")
plot(m1) # need to identify a large outlier, also only 113 complete cases
performance::r2(m1)

