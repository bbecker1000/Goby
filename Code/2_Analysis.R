source("1_DataCleaning.R")

library(lme4)
library(sjPlot)

hist(goby_master$Sum_TW)
hist(goby_master$Sum_TW/(goby_master$volume))

#Need to get in rain and wind data !

#big test model

m1 <- glmer(Sum_TW ~ Zone + scale(Year) + 
              Season + scale(Sum_SB) + scale(Sum_SC) + 
              Water_temp_1 + (1|Zone) + (1|Season),
            data = goby_master,
            family = negative.binomial(1),
            offset=log(volume))

           
          
           
summary(m1)
plot_model(m1, type = "eff")



