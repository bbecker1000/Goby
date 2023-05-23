source("1_DataCleaning.R")

library(lme4)
library(sjPlot)

hist(goby_master$Sum_TW)

hist(goby_master$Sum_TW/(goby_master$volume))
goby_master
#Need to get in rain and wind data !

#Temporarily non-goby fish NA to zero #ask Darren
goby_master$Sum_SC[is.na(goby_master$Sum_SC)] <- 0
goby_master$Sum_SB[is.na(goby_master$Sum_SB)] <- 0

##

hist(goby_master$Water_temp_1)
hist(goby_master$Sum_SC)
hist(goby_master$Sum_SB)
plot(goby_master$Zone)


#big test model

m1 <- glmer(Sum_TW ~ Zone + 
              Dom_substrate +  (pool Muck)
              scale(Year) + 
              Season +   # DF should probably remove winter data  perhaps just look at a seasonal dynamics standpoint and flushed out in winter
              scale(Sum_SB) + 
              scale(Sum_SC) + 
              Water_temp_1 + # should we reduce the WQ variables?  keep Temp and DO for now.
              (1|Zone) + (1|Season),
            data = goby_master,
            family = negative.binomial(1),  #poisson
            offset=log(volume))

summary(m1)
plot_model(m1, type = "eff")
plot(m1) # need to identify a large outlier, also only 113 complete cases
performance::r2(m1)

