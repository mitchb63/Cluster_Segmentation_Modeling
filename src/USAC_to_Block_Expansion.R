setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
library(data.table)

# Read in the desired tables
usac <- read_csv('data/output/by_tract/join/usac_join.csv')
glimpse(usac)

zip_exp <- read_csv('data/output/by_block/Zip_expansion.csv')
glimpse(zip_exp)

# Join the usac data to the zip-tract expansion table
usac_tract <- zip_exp %>%
  left_join(usac, c('ZIP' = 'zip')) %>%
  mutate(Racers = Total_racers * zip_pop_proportion,
         Racers_Male = Male_Racers * zip_pop_proportion,
         Racers_Female = Female_Racers * zip_pop_proportion,
         Racers_Junior = Junior_Racers * zip_pop_proportion,
         Racers_Elite = Elite_Racers * zip_pop_proportion,
         Racers_Master = Master_Racers * zip_pop_proportion,
         Racers_Pro_Road = Pro_Road_Racers * zip_pop_proportion,
         Racers_Pro_MTB = Pro_MTB_Racers * zip_pop_proportion,
         Racers_Pro_Track = Pro_Track_Racers * zip_pop_proportion,
         Racers_Pro_Cross = Pro_Cross_Racers * zip_pop_proportion,
         Racers_Mid_road = Mid_Road_Racers * zip_pop_proportion,
         Racers_Mid_MTB = Mid_MTB_Racers * zip_pop_proportion,
         Racers_Mid_Track = Mid_Track_Racers * zip_pop_proportion,
         Racers_Mid_Cross = Mid_Cross_Racers * zip_pop_proportion)
glimpse(usac_tract)

# Select the desired columns
cols <- c(1,2,20:33)
usac_tract_sel <- usac_tract[,cols]
glimpse(usac_tract_sel)

# Write the expanded Table
fwrite(usac_tract_sel, 'data/output/by_tract/USAC_by_tract.csv')

# Import the tract expansion table
tract_exp <- read_csv('data/output/by_block/Tract_expansion.csv')
glimpse(tract_exp)

# Join the usac data to the tract-block expansion table
usac_block <- tract_exp %>%
  left_join(usac_tract_sel, by = 'G_ID') %>%
  mutate(Racers = Racers * block_pop_proportion,
         Male_Racers = Racers_Male * block_pop_proportion,
         Female_Racers = Racers_Female * block_pop_proportion,
         Junior_Racers = Racers_Junior * block_pop_proportion,
         Elite_Racers = Racers_Elite * block_pop_proportion,
         Master_Racers = Racers_Master * block_pop_proportion,
         Pro_Road_Racers = Racers_Pro_Road * block_pop_proportion,
         Pro_MTB_Racers = Racers_Pro_MTB * block_pop_proportion,
         Pro_Track_Racers = Racers_Pro_Track * block_pop_proportion,
         Pro_Cross_Racers = Racers_Pro_Cross * block_pop_proportion,
         Mid_Road_Racers = Racers_Mid_road * block_pop_proportion,
         Mid_MTB_Racers = Racers_Mid_MTB * block_pop_proportion,
         Mid_Track_Racers = Racers_Mid_Track * block_pop_proportion,
         Mid_Cross_Racers = Racers_Mid_Cross * block_pop_proportion)
glimpse(usac_block)
summary(usac_block)

# Select the desired columns
cols <- c(1,9:35)
usac_block_sel <- usac_block[,cols]
glimpse(usac_block_sel)

# Write the expanded Table
fwrite(usac_block_sel, 'data/output/by_block/USAC_by_block.csv')

