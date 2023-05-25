### data cleaning

# load packages
library(tidyverse)
library(data.table)
library(lubridate)

## Import three data tables
water_qual_event <- read_csv("Data/water_qual_event.csv", 
                             col_types = cols(Date = col_date(format = "%m/%d/%Y")))
View(water_qual_event)
fish_dat <- read_csv("Data/fish_dat.csv", 
                     col_types = cols(Year = col_double())) # import notes problems on rows 12485 and 13444, but I don't see anything wrong.
#note that mort and Injury are all NA


water_qual <- read_csv("Data/water_qual.csv", 
                       col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                        ID = col_character(), Start_time = col_character()))

hist(fish_dat$Length) #outlier length on a stickleback (370mm!)  Check with Darren


## To Merge all the files with the fish data, we need to use Year-Season-Station_ID for all three files
# Water_qual_event
water_qual_event$Year <- year(water_qual_event$Date)
water_qual_event$Month <- month(water_qual_event$Date)
hist(water_qual_event$Month)
water_qual_event$Season <- ifelse(water_qual_event$Month < 6, "Winter", "Fall")  # Darren OK'd
water_qual_event$Unique_ID2 <- paste0(water_qual_event$Year,"_", 
                                      water_qual_event$Season, "_", 
                                      water_qual_event$Station_ID)
duplicated(water_qual_event$Unique_ID2) #yay, no duplicates #406 rows

#Water_qual
water_qual$Year <- year(water_qual$Date)
water_qual$Month <- month(water_qual$Date)
water_qual$Season <- ifelse(water_qual$Month < 6, "Winter", "Fall")
water_qual$Unique_ID2 <- paste0(water_qual$Year,"_", 
                                      water_qual$Season, "_", 
                                      water_qual$Station_ID)
duplicated(water_qual$Unique_ID2) #duplicates expected will fix with pivot wider


#fish_dat - seaon and year done
fish_dat$Unique_ID2 <- paste0(fish_dat$Year,"_", 
                                fish_dat$Season, "_", 
                                fish_dat$Station_ID)
duplicated(fish_dat$Unique_ID2) #mostly duplicate will fix with summarize

#lets make water_qual wide so each even has unique row.  
#currently surface and bottom samples from same even in consecutive rows

## add a unique "Group_ID" to each duplicated row to enable pivot wide
## rowid is from data.table package
water_qual_temp <- water_qual %>% 
  mutate(Group_ID = data.table::rowid(Unique_ID2)) #changed to Unique_ID2

#View(water_qual_temp)
hist(water_qual_temp$Group_ID)

# looks like some data are more than just the 2 values...perhaps these can be deleted?  look at raw data to see why 3-6 samples at some events.
# only 14 instances of > 2 environmental samples...I think we can delete these replicates and just keep the core 2 samples for each parameter.

water_qual_temp <- water_qual_temp %>%
  filter(Group_ID < 3)

hist(water_qual_temp$Group_ID)

# pivot wide
water_qual_wide <- water_qual_temp %>%
  pivot_wider(id_cols = c("Unique_ID2", "Date"), # added Date to keep in file
              names_from = "Group_ID",
              values_from = c("Water_temp", "Spec_cond", "Der_spec_cond", "DO", "Perc_sat", "pH")) #airtemp only 1 measurement
View(water_qual_wide) #has 397 rows, water_qual_event has 406 rows.  so some events missing WQ data?

# change some character to numeric
water_qual_wide$Water_temp_1 <- as.numeric(water_qual_wide$Water_temp_1)
water_qual_wide$Water_temp_2 <- as.numeric(water_qual_wide$Water_temp_2)
water_qual_wide$Spec_cond_1  <- as.numeric(water_qual_wide$Spec_cond_1)
water_qual_wide$Spec_cond_2  <- as.numeric(water_qual_wide$Spec_cond_2)
water_qual_wide$DO_1  <- as.numeric(water_qual_wide$DO_1)
water_qual_wide$DO_2  <- as.numeric(water_qual_wide$DO_2)
water_qual_wide$Perc_sat_1  <- as.numeric(water_qual_wide$Perc_sat_1)
water_qual_wide$Perc_sat_2  <- as.numeric(water_qual_wide$Perc_sat_2)

#join WQ_event and WQ
WQ_event_WQ <- left_join(water_qual_event, water_qual_wide, by = "Unique_ID2")
View(WQ_event_WQ) 

# fix some Species naming errors
unique(fish_dat$Species) 
fish_dat$Species <- 
  ifelse(fish_dat$Species == "PSC", "SC",
    ifelse(fish_dat$Species == "SSC", "SC", 
        ifelse(fish_dat$Species == "SCU", "SC", fish_dat$Species)))
unique(fish_dat$Species) #looks good

# get sum fish per day
fish_dat_sum <- 
  fish_dat %>%                                       
  group_by(Unique_ID2, Species) %>%                         
  summarise_at(vars(Numbers),             
               list(spec_sum = sum))

# sum fish into columns by species
fish_dat_sum <- 
  fish_dat_sum %>% 
  pivot_wider(id_cols = "Unique_ID2",
              names_from = "Species",
              values_from = "spec_sum",
              names_prefix = "Sum_")
fish_dat_sum
fish_dat_sum <- fish_dat_sum %>% select(-Sum_NONE) #remove "none" column


#calculate min, max, mean for species length per unique ID  (consider adding 95% CI) !!
fish_stats <- fish_dat %>%
  group_by(Unique_ID2) %>%                         
  summarise_at(vars(Length),             
               list(min_length = min,
                    max_length = max,
                    mean_length = mean))

#calculate sum of mortality per unique ID


# Note that mortality and injury data are all NA when imported.  TTB please check original data. 
fish_mort <- fish_dat %>%                                       
  group_by(Unique_ID2, Species) %>%                         
  summarise_at(vars(Mort),             
               list(mort_sum = sum))

fish_mortality <- 
  fish_mort %>% 
  pivot_wider(id_cols = "Unique_ID2",
              names_from = "Species",
              values_from = "mort_sum",
              names_prefix = "Mort_")
fish_mortality <- fish_mortality %>% select(-Mort_NONE)


#calculate sum of injury per unique ID
fish_injury <- fish_dat %>%                                       
  group_by(Unique_ID2, Species) %>%                         
  summarise_at(vars(Injury),             
               list(inj_sum = sum))
summary(fish_dat)
#fish_injury <- fish_injury %>% select(-NONE)

#calculate sum of microsporidian per unique ID
###change N/Y to 0/1 first
fish_dat_temp <- fish_dat %>%
  mutate(Microsporidian = ifelse(Microsporidian == "N",0,1))

fish_dat <- fish_dat_temp %>% mutate(Microsporidian = ifelse(is.na(Microsporidian), 0, Microsporidian))

microsporidium <- fish_dat %>% group_by(Unique_ID2) %>%                         
  summarise_at(vars(Microsporidian),             
               list(micro_sum = sum))
microsporidium

### let's join all our five datasets
#water_qual_event 
#water_qual_wide,
#fish_dat_sum
#fish_stats
#microsporidium
#fish_mortality <- all NAs, so check original data
#fish_injury    <- all NAs, so check original data

goby_master <- list(water_qual_event, water_qual_wide, fish_dat_sum, fish_stats, microsporidium) %>%   #will add fish_mort
  reduce(left_join, by = "Unique_ID2")
str(goby_master)
View(goby_master)
#Volume is just mean depth by area and volumer has one na.  so replace all and make numeric
goby_master$volume <- goby_master$Ave_depth * goby_master$Area
goby_master$Volumer <- as.numeric(goby_master$Volumer)

plot(goby_master$volume, as.numeric(goby_master$Volumer))  
# question for Darren, why does volume and Volumer usually but not always match?


goby_master$Volume_Match <- goby_master$Volumer == goby_master$volume

#date fields match?
goby_master$Match <- goby_master$Date.x == goby_master$Date.y
# 5 dates don't match and are 1-2 days different, must have been data collected on two days due to logistics.

goby_master <- goby_master %>%
  rename("Zone" = "Zone 2")

goby_master$Zone <- as.factor(goby_master$Zone)


#1 case where volume = 0 was in shallow mud.  assign a really small area
hist(goby_master$volume)
goby_master$volume <- ifelse(goby_master$volume == 0, 0.2, goby_master$volume)

#look at zones to combine.  refer to previous report.
plot(goby_master$Zone)

# remove the Winter data?
table(goby_master$Season)

str(goby_master)
goby_master$Water_temp_1 <- as.numeric(goby_master$Water_temp_1)
hist(goby_master$Water_temp_1)



## TO DO
# pool zones?
# include precip data
# breach data?

# drop winter data
goby_master[goby_master$Season != "Winter", ]  

# drop zones 
goby_master[goby_master$Zone != "NE" & goby_master$Zone != "SE" & goby_master$Zone != "SW", ]


#####OK goby_master ready for analysis -----------------------------------------












