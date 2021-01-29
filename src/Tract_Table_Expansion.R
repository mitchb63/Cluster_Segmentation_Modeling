setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
library(data.table)

# Read in the desired table.  
tract_table <- read_csv('data/output/by_tract/join/Table_B28011.csv')
glimpse(tract_table)
summary(tract_table)
tract_table$GEO_ID <- str_sub(tract_table$GEO_ID, 10)

# Import the tract expansion table.  This can be accomplished by population proportion or by housing proportion
tract_exp <- read_csv('data/output/by_block/Tract_expansion_by_housing.csv')
glimpse(tract_exp)
tract_exp$G_ID <- str_sub(tract_exp$G_ID, 10)

head(tract_exp$G_ID)
head(tract_table$GEO_ID)

# Join the tract Table data to the tract-block expansion table
block_table <- tract_exp %>%
  left_join(tract_table, c('G_ID' = 'GEO_ID')) %>%
  mutate(V1 = Households_with_no_Internet_access  * block_housing_proportion) #,
         V2 = In_labor_force * block_pop_proportion,
         V3 = In_labor_force_Civilian_labor_force * block_pop_proportion,
         V4 = In_labor_force_Civilian_labor_force_Employed * block_pop_proportion,
         V5 = In_labor_force_Civilian_labor_force_Unemployed * block_pop_proportion,
         V6 = In_labor_force_Armed_Forces * block_pop_proportion,
         V7 = Not_in_labor_force * block_pop_proportion) #,
         V8 = Male_10th_grade * block_pop_proportion,
         V9 = Male_11th_grade * block_pop_proportion,
         V10 = Male_12th_grade_no_diploma * block_pop_proportion,
         V11 = Male_High_school_graduate_includes_equivalency * block_pop_proportion,
         V12 = Male_Some_college_less_than_1_year * block_pop_proportion,
         V13 = Male_Some_college_1_or_more_years_no_degree * block_pop_proportion,
         V14 = Male_Associates_degree * block_pop_proportion,
         V15 = Male_Bachelors_degree * block_pop_proportion,
         V16 = Male_Masters_degree * block_pop_proportion,
         V17 = Male_Professional_school_degree * block_pop_proportion,
         V18 = Male_Doctorate_degree * block_pop_proportion,
         V19 = Female_Edu_Pop * block_pop_proportion,
         V20 = Female_No_schooling_completed * block_pop_proportion,
         V21 = Female_Nursery_to_4th_grade * block_pop_proportion,
         V22 = Female_5th_and_6th_grade * block_pop_proportion,
         V23 = Female_7th_and_8th_grade * block_pop_proportion,
         V24 = Female_9th_grade * block_pop_proportion,
         V25 = Female_10th_grade * block_pop_proportion,
         V26 = Female_11th_grade * block_pop_proportion,
         V27 = Female_12th_grade_no_diploma * block_pop_proportion,
         V28 = Female_High_school_graduate_includes_equivalency * block_pop_proportion,
         V29 = Female_Some_college_less_than_1_year * block_pop_proportion,
         V30 = Female_Some_college_1_or_more_years_no_degree * block_pop_proportion,
         V32 = Female_Associates_degree * block_pop_proportion,
         V33 = Female_Bachelors_degree * block_pop_proportion,
         V34 = Female_Masters_degree * block_pop_proportion,
         V35 = Female_Professional_school_degree * block_pop_proportion,
         V36 = Female_Doctorate_degree * block_pop_proportion)

glimpse(block_table)
summary(block_table)

# Select and rename the desired columns
cols <- c(1,7)
block_table_sel <- block_table[,cols]
glimpse(block_table_sel)

block_table_sel <- block_table_sel %>%
  rename(Households_with_no_Internet_access = V1) #,
         In_labor_force = V2,
         In_labor_force_Civilian_labor_force = V3,
         In_labor_force_Civilian_labor_force_Employed = V4,
         In_labor_force_Civilian_labor_force_Unemployed = V5,
         In_labor_force_Armed_Forces = V6,
         Not_in_labor_force = V7) #,
         Male_10th_grade = V8,
         Male_11th_grade = V9,
         Male_12th_grade_no_diploma = V10,
         Male_High_school_graduate_includes_equivalency = V11,
         Male_Some_college_less_than_1_year = V12,
         Male_Some_college_1_or_more_years_no_degree = V13,
         Male_Associates_degree = V14,
         Male_Bachelors_degree = V15,
         Male_Masters_degree = V16,
         Male_Professional_school_degree = V17,
         Male_Doctorate_degree = V18,
         Female_Edu_Pop = V19,
         Female_No_schooling_completed = V20,
         Female_Nursery_to_4th_grade = V21,
         Female_5th_and_6th_grade = V22,
         Female_7th_and_8th_grade = V23,
         Female_9th_grade = V24,
         Female_10th_grade = V25,
         Female_11th_grade = V26,
         Female_12th_grade_no_diploma = V27,
         Female_High_school_graduate_includes_equivalency = V28,
         Female_Some_college_less_than_1_year = V29,
         Female_Some_college_1_or_more_years_no_degree = V30,
         Female_Associates_degree = V32,
         Female_Bachelors_degree = V33,
         Female_Masters_degree = V34,
         Female_Professional_school_degree = V35,
         Female_Doctorate_degree = V36)

glimpse(block_table_sel)

# Write the expanded Table
fwrite(block_table_sel, 'data/output/by_block/Table_B28011_by_block.csv')
