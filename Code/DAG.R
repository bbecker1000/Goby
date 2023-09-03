##DAGs for TW Goby

library(dagitty)

## Dependent Var
# TW Goby density (area or volume)
# TW Goby size distribution?

## Independent covariates
# Year
# Season (only a few early dates so probably skip)
# Water temp
# Salinity
# DO (perc_sat)
# pH
# Stickleback density
# Sculpin Density
# microsporidia frequency
# substrate
# phytoplankton cover?  May need to get into dataset from raw data
# SAV species?
# SAV Cover?
# EAV Species?
# EAV Cover?
# Zone ???  WQ and substrate act as controls, esp. low DO
# Breach <- Need to code  #DF suggests using date since last Breach...can provide dates
# Wind/Weather <- Need to get

## Random Effects
# Zone/Station_ID

# build the DAG

set.seed(123)

DAG_GOBY <- dagitty("dag{ 
  Year -> GOBY ;
  WQ -> GOBY ;
  Stickleback -> GOBY ;
  Sculpin -> GOBY ;
  Rain -> Microsporidia -> GOBY ;
  Substrate -> GOBY ;
  SAV -> GOBY ;
  Breach -> WQ -> GOBY ;
  Rain -> WQ -> GOBY ;
  Wind -> WQ -> GOBY ;
  Zone -> WQ -> GOBY ;
  Zone -> Wind ;
  WQ -> SAV -> GOBY ;
  WQ -> Sculpin -> GOBY ;
  WQ -> Stickleback -> GOBY ;
  WQ -> SAV -> Stickleback ;
  WQ -> SAV -> Sculpin ;
  Substrate -> Sculpin -> GOBY;
  
  
  Year [exposure] ;
  WQ [exposure] ;
  Zone [exposure] ;
  Stickleback [exposure] ;
  Sculpin [exposure] ;
  Microsporidia [exposure] ;
  Substrate [exposure] ;
  SAV [exposure] ;
  Breach [exposure] ;
  Rain [exposure] ;
  Wind [exposure] ;
}")
  
## add in zone?

plot(DAG_GOBY)
impliedConditionalIndependencies(DAG_GOBY)


# to pretty up the plot 
# not done yet
coordinates(DAG_GOBY) <- list(x=c(Year=0,WQ=3,Stickleback=3,Sculpin=6,
                                  Rain=0,Microsporidia=0, Substrate=3, 
                                  SAV=4.5, Breach=3, Wind=6, Zone= 6, GOBY=0),
                              y=c(Year=3,WQ=-3,Stickleback=-1,Sculpin=1,
                                  Rain=-5,Microsporidia=-3, Substrate=3, 
                                  SAV=0, Breach=-5, Wind=-3, Zone=-5, GOBY=0))
plot(DAG_GOBY)



