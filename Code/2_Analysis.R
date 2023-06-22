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

## check why have NAs
goby_master$Since_Breach[is.na(goby_master$Since_Breach)] <- 0

goby_master$Dom_substrate <- ifelse(goby_master$Dom_substrate == "corophium_tubes", 
                                    "muck", goby_master$Dom_substrate)


View(goby_master)
##
hist(goby_master$Sum_TW)
(goby_master$Dom_substrate)
hist(goby_master$Water_temp_1)
hist(goby_master$Sum_SC)
hist(goby_master$Sum_SB)
plot(goby_master$Zone)

hist(goby_master$Since_Breach)  



#big test model

m1 <- glmer(Sum_TW ~  
              Dom_substrate +  
              SAV + # (pool Muck)
              scale(Year) +
              scale(Sum_SB) + 
              scale(Sum_SC) + 
              Rain_Sum +
              temp_mean + 
              min_DO +
              Zone +
              Since_Breach + # should we reduce the WQ variables?  keep Temp and DO for now.
              (1|Zone),
            data = goby_master,
            #family = poisson,
            family = negative.binomial(1),  #poisson
            offset=log(volume))

summary(m1)
p.eff <- plot_model(m1, type = "eff")  #eff
plot_grid(p.eff)
p.resid <- plot_model(m1, type = "resid")
p.resid
plot(m1) # need to identify a large outlier, also only 316 complete cases
performance::r2(m1)

## causal on WQ and breach

m1.no_breach <- glmer(Sum_TW ~  
              Dom_substrate +  # (pool Muck)
              scale(Year) +
              scale(Sum_SB) + 
              scale(Sum_SC) + 
                Rain_Sum +
                temp_mean + 
              min_DO +
              #Since_Breach + 
              Zone +
              # should we reduce the WQ variables?  keep Temp and DO for now.
              (1|Zone),
            data = goby_master,
            #family = poisson,
            family = negative.binomial(1),  #poisson
            offset=log(volume))

m1.no_temp_1 <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        Rain_Sum +
                        #temp_mean + 
                        #min_DO +
                        Since_Breach + 
                        Zone +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                        (1|Zone),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))

summary(m1.no_breach)  #temp = -0.11 (sig)
summary(m1.no_temp_1)  #breach = -1.95,  
summary(m1)            #temp = -0.05 (ns), breach = -1.8
## so once we know breach, there is little additional info gained from knowing temp.
## so breach constant, but temp weaker (and non-significant) with breach included
## conclude that breach is the causal variable
## do same for 
  # substrate --> sculpin 
  #breach --> stickleback
  #breach --> SAV
  # others...

p.eff <- plot_model(m1.no_temp_1, type = "eff")  #eff
plot_grid(p.eff)
p.resid <- plot_model(m1.no_temp_1, type = "resid")
p.resid
plot(m1.no_temp_1)



m1.int <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) * Zone +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        Rain_Sum +
                        #temp_mean + 
                        #min_DO +
                        Since_Breach + 
                        
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                        (1|Zone),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))
summary(m1.int)
