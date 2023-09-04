source("1_DataCleaning.R")


library(lme4)
library(sjPlot)
library(marginaleffects)

goby_master <- goby_master_3

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

# deal with soume outlier issues on SB and SC

# goby_master$Sum_SB <- ifelse(goby_master$Sum_SB > 1000 & !is.na(goby_master$Sum_SB), 
#                              1000, 
#                              goby_master$Sum_SB)
# hist(goby_master$Sum_SB)
# goby_master$Sum_SB
# goby_master$Sum_SC <- ifelse(goby_master$Sum_SC > 150, 
#                              150 ,
#                              goby_master$Sum_SC)
# hist(goby_master$Sum_SC)
# 
# goby_master$Sum_TW <- ifelse(goby_master$Sum_TW > 1000 & !is.na(goby_master$Sum_TW), 
#                              1000, 
#                              goby_master$Sum_TW)


(goby_master)
##
hist(goby_master$Sum_TW)
barplot(goby_master$Dom_substrate)
hist(goby_master$Water_temp_1)
hist(goby_master$Sum_SC)
hist(goby_master$Sum_SB)
plot(goby_master$Zone)
hist(goby_master$Rain_Sum)
hist(goby_master$Since_Breach)  
hist(log(goby_master$volume))

#check is sampling effort consistent over time.
ggplot(goby_master, aes(Year, volume)) +
  geom_point() +
  geom_smooth()

ggplot(goby_master, aes(Year, Area)) +
  geom_point() +
  geom_smooth()

ggplot(goby_master, aes(Year, Ave_depth)) +
  geom_point() +
  geom_smooth()


## all authors decided that Area makes more sense than Volume since most (>99%) gobies are not larvae (<12mm) and are neritic.

## get nb theta estimate for use in negative binomial models
m.nb <- glmer.nb(Sum_TW ~ 
                   #Dom_substrate +  
                   SAV + # (pool Muck)
                   scale(Year) +
                   scale(Sum_SB) + 
                   scale(Sum_SC) + 
                   scale(Rain_Sum) +
                   scale(temp_mean) + 
                   scale(min_DO) +
                   #Zone +
                   Since_Breach + 
                   u_mean + # should we reduce the WQ variables?  keep Temp and DO for now.
                   (1|Zone)+ (1|Dom_substrate),
                   data = goby_master,
                 #family = poisson,
                 #family = negative.binomial(1),  #poisson
                 offset=log(Area), verbose=TRUE)
m.nb
## The neg.binomial theta parameter:
THETA <- getME(m.nb, "glmer.nb.theta")
LL <- logLik(m.nb)

#theta = 0.61



#big test model

m1 <- glmer(Sum_TW ~  
              # Dom_substrate +  
              scale(SAV) + # (pool Muck)
              scale(Year) +
              scale(Sum_SB) + 
              scale(Sum_SC) + 
              scale(Rain_Sum) +
              scale(temp_mean) + 
              scale(min_DO) +
              #Zone +
              Since_Breach + 
              scale(u_mean) + # should we reduce the WQ variables?  keep Temp and DO for now.
              (1|Zone) + (1|Dom_substrate),
            data = goby_master,
            #family = poisson,
            family = negative.binomial(THETA),  #poisson
            #offset=log(volume)
            offset=log(Area)
            )




summary(m1)
p.eff <- plot_model(m1, type = "eff")  #eff
plot_grid(p.eff)
p.resid <- plot_model(m1, type = "resid")
p.resid
plot(m1) # need to identify a large outlier, also only 316 complete cases
performance::r2(m1)

## 1.0 causal on WQ and breach

m1.no_breach <- glmer(Sum_TW ~  
              #Dom_substrate +  # (pool Muck)
              scale(Year) +
              scale(Sum_SB) + 
              scale(Sum_SC) + 
                scale(Rain_Sum) +
                scale(temp_mean) + 
              scale(min_DO) +
              #Since_Breach + 
              #Zone +
                scale(u_mean) +
                # should we reduce the WQ variables?  keep Temp and DO for now.
              (1|Zone)+ (1|Dom_substrate),
            data = goby_master,
            #family = poisson,
            family = negative.binomial(THETA),  #poisson
            offset=log(volume))

m1.no_temp_1 <- glmer(Sum_TW ~  
                       # Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #temp_mean + 
                        #min_DO +
                        Since_Breach + 
                        #Zone +
                        scale(u_mean) +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                        (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(THETA),  #poisson
                      offset=log(volume))

summary(m1.no_breach)  #temp = -0.17 (P = 0.05)
summary(m1.no_temp_1)  #breach = -1.5 (P < 0.01)  
summary(m1)            #temp = -0.08 (ns), breach = -1.4
## so once we know breach, there is little additional info gained from knowing temp.
## so breach constant, but temp weaker (and non-significant) with breach included
## conclude that breach is the causal variable
## do same for 
  # Breach --> DO
  # substrate --> sculpin 
  # breach --> stickleback
  # breach --> SAV
  # SAV --> Sculpin
  # Rain --> Microsporidia
  # Rain --> WQ --> SAV --> Stickleback

p.eff <- plot_model(m1.no_temp_1, type = "eff")  #eff
plot_grid(p.eff)
p.resid <- plot_model(m1.no_temp_1, type = "resid")
p.resid
plot(m1.no_temp_1)


## Breach vs DO

m1.breach_DO <- glmer(Sum_TW ~  
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(temp_mean) + 
                        scale(min_DO) +
                        Since_Breach + 
                        scale(u_mean) +
                        (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      family = negative.binomial(THETA),  
                      offset=log(Area))

m1.DO <- glmer(Sum_TW ~  
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(temp_mean) + 
                        scale(min_DO) +
                        #Since_Breach + 
                        scale(u_mean) +
                        (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      family = negative.binomial(THETA),  
                      offset=log(Area))

m1.Breach <- glmer(Sum_TW ~  
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        scale(u_mean) +
                        (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      family = negative.binomial(THETA),  
                      offset=log(Area))

summary(m1.breach_DO)
# DO = 0.06, p = .41
# breach = -1.4, p < 0.01
summary(m1.Breach)
# breach = -1.36, p < 0.01
summary(m1.DO)
# DO = 0.14, p = .07

# so Breach controls DO

## Breach vs DO

m1.breach_SB <- glmer(Sum_TW ~  
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        scale(u_mean) +
                        (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      family = negative.binomial(THETA),  
                      offset=log(volume))







## 1.1 CAUSAL on breach --> stickleback

m1.SB.breach <- glmer(Sum_TW ~  
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        scale(u_mean) +
                        (1|Zone) + (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(THETA),  #poisson
                      offset=log(volume))

m1.SB.no_breach <- glmer(Sum_TW ~  
                        scale(Year) +
                        scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                         (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(THETA),  #poisson
                      offset=log(volume))

m1.no_SB.breach <- glmer(Sum_TW ~  
                           scale(Year) +
                           #scale(Sum_SB) + 
                           scale(Sum_SC) + 
                           scale(Rain_Sum) +
                           #scale(temp_mean) + 
                           #scale(min_DO) +
                           Since_Breach + 
                           (1|Zone)+ (1|Dom_substrate),
                         data = goby_master,
                         family = negative.binomial(1), 
                         offset=log(volume))





summary(m1.SB.breach)  #SB = ns , Breach = -1.96
summary(m1.SB.no_breach)  #SB = ns,  
summary(m1.no_SB.breach)  #Breach = -1.97

# so SB not important either way

## 1.2 CAUSAL on breach --> SAV

m1.SAV.breach <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        #scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        scale(SAV) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        Zone +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                         (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))

m1.SAV.no_breach <- glmer(Sum_TW ~  
                           Dom_substrate +  # (pool Muck)
                           scale(Year) +
                           #scale(Sum_SB) + 
                           scale(Sum_SC) + 
                           scale(Rain_Sum) +
                            scale(SAV) +
                           #scale(temp_mean) + 
                           #scale(min_DO) +
                           #Since_Breach + 
                           Zone +
                           # should we reduce the WQ variables?  keep Temp and DO for now.
                            (1|Zone)+ (1|Dom_substrate),
                         data = goby_master,
                         #family = poisson,
                         family = negative.binomial(1),  #poisson
                         offset=log(volume))

m1.no_SAV.breach <- glmer(Sum_TW ~  
                           Dom_substrate +  # (pool Muck)
                           scale(Year) +
                           #scale(Sum_SB) + 
                           scale(Sum_SC) + 
                           scale(Rain_Sum) +
                            #scale(SAV) +
                           #scale(temp_mean) + 
                           #scale(min_DO) +
                           Since_Breach + 
                           Zone +
                           # should we reduce the WQ variables?  keep Temp and DO for now.
                            (1|Zone)+ (1|Dom_substrate),
                         data = goby_master,
                         #family = poisson,
                         family = negative.binomial(1),  #poisson
                         offset=log(volume))





summary(m1.SAV.breach)  #SAV = .12 , Breach = -1.87
summary(m1.SAV.no_breach)  #SAV = .20,  
summary(m1.no_SAV.breach)  #Breach = -1.97

# so SAV keeps importance not important either way

## 1.3 CAUSAL on SAV --> Sculpin

m1.SAV.SC <- glmer(Sum_TW ~  
                         Dom_substrate +  # (pool Muck)
                         scale(Year) +
                         #scale(Sum_SB) + 
                         scale(Sum_SC) + 
                         scale(Rain_Sum) +
                         scale(SAV) +
                         #scale(temp_mean) + 
                         #scale(min_DO) +
                         Since_Breach + 
                         Zone +
                         # should we reduce the WQ variables?  keep Temp and DO for now.
                     (1|Zone)+ (1|Dom_substrate),
                       data = goby_master,
                       #family = poisson,
                       family = negative.binomial(1),  #poisson
                       offset=log(volume))


m1.SAV.no_SC <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        #scale(Sum_SB) + 
                        # scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        scale(SAV) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        Zone +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                        (1|Zone) + (1|Dom_substrate)(1|Zone),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))


m1.no_SAV.SC <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        #scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        #scale(SAV) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        Zone +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                        (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))

summary(m1.SAV.SC)  #SAV = .12 , SC = -0.31
summary(m1.SAV.no_SC)  #SAV = .12,  
summary(m1.no_SAV.SC)  #SC = -0.31

# SAV no effect on SC so keep SC !

## 1.4 CAUSAL on Rain --> microsporidia

m1.Rain.micro <- glmer(Sum_TW ~  
                     Dom_substrate +  # (pool Muck)
                     scale(Year) +
                     #scale(Sum_SB) + 
                     scale(Sum_SC) + 
                     scale(Rain_Sum) +
                     scale(SAV) +
                     scale(micro_sum) +
                     #scale(temp_mean) + 
                     #scale(min_DO) +
                     Since_Breach + 
                     Zone +
                     # should we reduce the WQ variables?  keep Temp and DO for now.
                       (1|Zone)+ (1|Dom_substrate),
                   data = goby_master,
                   #family = poisson,
                   family = negative.binomial(1),  #poisson
                   offset=log(volume))


m1.Rain.no_micro <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        #scale(Sum_SB) + 
                        # scale(Sum_SC) + 
                        scale(Rain_Sum) +
                        scale(SAV) +
                        #scale(micro_sum) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        Zone +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                          (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))


m1.no_Rain.micro <- glmer(Sum_TW ~  
                        Dom_substrate +  # (pool Muck)
                        scale(Year) +
                        #scale(Sum_SB) + 
                        scale(Sum_SC) + 
                        #scale(Rain_Sum) +
                        scale(micro_sum) +
                        #scale(SAV) +
                        #scale(temp_mean) + 
                        #scale(min_DO) +
                        Since_Breach + 
                        Zone +
                        # should we reduce the WQ variables?  keep Temp and DO for now.
                          (1|Zone)+ (1|Dom_substrate),
                      data = goby_master,
                      #family = poisson,
                      family = negative.binomial(1),  #poisson
                      offset=log(volume))

summary(m1.Rain.micro)  #Rain = ns , micro = 0.24
summary(m1.Rain.no_micro)  #rain = ns,  
summary(m1.no_Rain.micro)  #micro = 0.26

# rain no impact on Goby or micro.  KEEP Micro, drop rain.

p.eff <- plot_model(m1.no_Rain.micro, type = "eff")  #eff
plot_grid(p.eff)
p.resid <- plot_model(m1.no_Rain.micro, type = "resid")
p.resid
plot(m1.no_Rain.micro)





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
