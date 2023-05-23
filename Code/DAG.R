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
  WQ -> SAV -> GOBY ;
  WQ -> Sculpin -> GOBY ;
  WQ -> Stickleback -> GOBY ;
  WQ -> SAV -> Stickleback ;
  WQ -> SAV -> Sculpin ;
  Substrate -> Sculpin -> GOBY;
  
  
  Year [exposure] ;
  WQ [exposure] ;
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
coordinates(DAG_GOBY) <- list(x=c(BreedingYear=3,AreaType=1,Visitor=2,PEFA=5,
                                  WinterPrecip=5,ShortDrought=5, LongDrought =5, 
                                  Psi = 3, VegU= 4, PreyU = 4),
                              y=c(BreedingYear=4,AreaType=4,Visitor=4,PEFA=4,
                                  WinterPrecip=3,ShortDrought=2, LongDrought =1, 
                                  Psi = 2, VegU=2, PreyU = 1))




