source("1_DataCleaning.R")

library(lme4)
library(sjPlot)
library(marginaleffects)

goby_master <- goby_master_2

hist(goby_master$Sum_TW)

hist(goby_master$Sum_TW/(goby_master$volume))
goby_master
#Need to get in rain and wind data !

#Temporarily non-goby fish NA to zero #ask Darren
#no longer needed
##goby_master$Sum_SC[is.na(goby_master$Sum_SC)] <- 0
# goby_master$Sum_SB[is.na(goby_master$Sum_SB)] <- 0

##
hist(goby_master$Sum_TW)
hist(goby_master$Water_temp_1)
hist(goby_master$Sum_SC)
hist(goby_master$Sum_SB)
plot(goby_master$Zone)


#big test model

m1 <- glmer(Sum_TW ~  
              Dom_substrate +  # (pool Muck)
              scale(Year) +
              scale(Sum_SB) + 
              scale(Sum_SC) + 
              Water_temp_1 + 
              min_DO +
              Zone +# should we reduce the WQ variables?  keep Temp and DO for now.
              (1|Zone),
            data = goby_master,
            #family = poisson,
            family = negative.binomial(1),  #poisson
            offset=log(volume))

summary(m1)
p <- plot_model(m1, type = "eff")
plot_grid(p)
#plot_predictions(m1, condition = c("Year", "Zone"))
plot(m1) # need to identify a large outlier, also only 316 complete cases
performance::r2(m1)
