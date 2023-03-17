## data cleaning

library(tidyverse)
library(tidyr)
library(data.table)
library(dplyr)
library(readr)
water_qual_event <- read_csv("Data/water_qual_event.csv")
fish_dat <- read_csv("Data/fish_dat.csv")  
# import notes problems on rows 12485 and 13444, but I don't see anything
# View(fish_dat)
water_qual <- read_csv("Data/water_qual.csv")
View(water_qual)

View(water_qual_event)
#lets make water_qual wide so each even has unique row.  
#currently surface and bottom samples from same even in consecutive rows

## add a unique "Group_ID" to each duplicated row to enable pivot wide
water_qual_temp <- water_qual %>% 
  mutate(Group_ID = rowid(Unique_ID))

#View(water_qual_temp)
hist(water_qual_temp$Group_ID)

# looks like some data are more than just the 2 values...perhaps these can be deleted?  look at raw data to see why 3-6 samples at some events.
# only 14 instances of > 2 environmental samples...I think we can delete these replicates and just keep the core 2 samples for each parameter.

water_qual_temp <- water_qual_temp %>%
  filter(Group_ID < 3)

hist(water_qual_temp$Group_ID)

# pivot wide
water_qual_wide <- water_qual_temp %>%
  pivot_wider(id_cols = "Unique_ID",
              names_from = "Group_ID",
              values_from = c("Water_temp", "Spec_cond", "Der_spec_cond", "DO", "Perc_sat", "pH")) #airtemp only 1 measurement
View(water_qual_wide)  


