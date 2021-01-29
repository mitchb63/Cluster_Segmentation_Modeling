setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
memory.limit(48915)

df <- read_csv('data/output/by_block/Complete_Block_Dataset.csv')
glimpse(df)
summary(df)

# Remove census block with no population anbd with unreasonably high populations
df_filtered <- df %>%
  filter(Total_Population != 0 & Total_Population <= 3500)

# Remove duplicate USAC variables
cols <- c(1:141,155)
df_filtered <- df_filtered[,cols]

rm(df)

glimpse(df_filtered)
summary(df_filtered)

# Remove the median income outliers
high_inc <- which(df_filtered$Median_household_income_in_2019_dollars < -5000000)
length(high_inc)
df_filtered$Median_household_income_in_2019_dollars[high_inc] <- NA
summary(df_filtered$Median_household_income_in_2019_dollars)

# Replace NAs in the USAC data with 0's
usac_zero <- which(is.na(df_filtered$Racers_Mid_Cross))
length(usac_zero)
df_filtered$Racers_Mid_Cross[usac_zero] <- 0
summary(df_filtered$Racers_Mid_Cross)

# Examine and plot individual variables

ggplot(df_filtered, aes(x=Total_Households)) + geom_histogram(bins = 100)
table(df_filtered$Total_Population)

extract <- which(df_filtered$Total_Employment_Pop > 3500)
df_high <- df_filtered[extract,]

View(df_high)

library(data.table)
fwrite(df_filtered,'data/output/by_block/Block_dataset_partial_cleaning.csv')
# df_filtered <- fread('data/output/by_block/Block_dataset_partial_cleaning.csv')


#############################################################################
# Combine a few columns into new variables

df_filtered <- df_filtered %>%
  mutate(Male_15_to_19 = Male_15_to_17 + Male_18_and_19,
         Male_20_to_24 = Male_20 + Male_21 + Male_22_to_24,
         Male_60_to_64 = Male_60_and_61 + Male_62_to_64,
         Male_65_to_69 = Male_65_and_66 + Male_67_to_69,
         Female_15_to_19 = Female_15_to_17 + Female_18_and_19,
         Female_20_to_24 = Female_20 + Female_21 + Female_22_to_24,
         Female_60_to_64 = Female_60_and_61 + Female_62_to_64,
         Female_65_to_69 = Female_65_and_66 + Female_67_to_69)

glimpse(df_filtered)

df_filtered <- df_filtered %>%
  mutate(Male_no_diploma = Male_Nursery_to_4th_grade + Male_5th_and_6th_grade + Male_7th_and_8th_grade + Male_9th_grade + Male_10th_grade + Male_11th_grade + Male_12th_grade_no_diploma,
         Male_Some_college = Male_Some_college_less_than_1_year + Male_Some_college_1_or_more_years_no_degree,
         Female_no_diploma = Female_Nursery_to_4th_grade + Female_5th_and_6th_grade + Female_7th_and_8th_grade + Female_9th_grade + Female_10th_grade + Female_11th_grade + Female_12th_grade_no_diploma,
         Female_Some_college = Female_Some_college_less_than_1_year + Female_Some_college_1_or_more_years_no_degree)

fwrite(df_filtered,'data/output/by_block/Block_dataset_partial_cleaningb.csv')
# df_filtered <- read_csv('data/output/by_block/Block_dataset_partial_cleaningb.csv')

cols <- c(1:24,143,144,30:36,145,146,41:48,147,148,54:60,149,150,65:68,79,151,87,152,90:94,96,153,104,154,107:111,69:76,112,113,115:120,121:127,128:141,142)
df_ordered <- df_filtered[,cols]
glimpse(df_ordered)

df_ordered <- df_ordered %>%
  rename(Median_Household_Income = Median_household_income_in_2019_dollars,
         Total_in_labor_force = In_labor_force,
         Total_in_civilian_labor_force = In_labor_force_Civilian_labor_force,
         Total_civilian_labor_force_employeed = In_labor_force_Civilian_labor_force_Employed,
         Total_civilian_labor_force_unemployeed = In_labor_force_Civilian_labor_force_Unemployed,
         Total_in_Armed_Forces = In_labor_force_Armed_Forces,
         Commute_Public_Transportation = Public_transportation_excluding_taxicab,
         Commute_Taxicab = Taxicab,
         Commute_Motorcycle = Motorcycle,
         Commute_Bicycle = Bicycle,
         Commute_Walked = Walked,
         Commute_Other = Other_means,
         Commute_worked_from_home = Worked_from_home,
         Bicycle_Racers = Racers,
         Bicycle_Racers_Male = Racers_Male,
         Bicycle_Racers_Female = Racers_Female,
         Bicycle_Racers_Junior = Racers_Junior,
         Bicycle_Racers_Elite = Racers_Elite,
         Bicycle_Racers_Master = Racers_Master,
         Bicycle_Racers_Pro_Road = Racers_Pro_Road,
         Bicycle_Racers_Pro_MTB = Racers_Pro_MTB,
         Bicycle_Racers_Pro_track = Racers_Pro_Track,
         Bicycle_Racers_Pro_Cross = Racers_Pro_Cross,
         Bicycle_Racers_Mid_Road = Racers_Mid_road,
         Bicycle_Racers_Mid_MTB = Racers_Mid_MTB,
         Bicycle_Racers_Mid_Track = Racers_Mid_Track,
         Bicycle_Racers_Mid_Cross = Racers_Mid_Cross,
         Minimum_Winter_Outdoor_Temperature = Min_Temp)

library(data.table)
fwrite(df_ordered,'data/output/by_block/Complete_Block_Dataset_CLEAN.csv')

rm(df_filtered)

#make this example reproducible 
set.seed(1)

var_names <- colnames(df_ordered)

scale_names <- var_names[10:114]
#scale var1 and var2 to have mean = 0 and standard deviation = 1
df_scaled <- df_ordered %>% mutate_at(scale_names, ~(scale(.) %>% as.vector))
rm(df_ordered)
summary(df_scaled)

fwrite(df_scaled,'data/output/by_block/Complete_Block_Dataset_CLEAN_SCALED.csv')
