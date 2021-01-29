setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
library(data.table)
library(censusapi)

# Add key to .Renviron
Sys.setenv(CENSUS_KEY='b3a5636791fcac096b1fd609d4ba24e1bd7c10ae')
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#################################################################
# Enter the table number and year for the desired table below.
df_table <- 'P12_2010e'
table_name <- 'P12' # 'B19052', 'C08301', 'B29002', 'B99231')
survey <- 'sf1'
census_name_path <- 'dec/sf1'
year <- 2010

table_output_new_name <- str_c('data/output/by_block/Table_', table_name, 'e_by_block_w_zip.csv')

#################################################################

# Read in the desired table
df <- read_csv(str_c('data/output/by_block/',df_table, '.csv'))
glimpse(df)

# Check for duplicate rows and duplicate GEO_ID values
working_table <- df %>% distinct()
uniq_GEO_ID <- unique(working_table$GEO_ID)
length(uniq_GEO_ID)
rm(df)
rm(uniq_GEO_ID)

# Pull the sf1 variable Metadata from the api
dec_vars <- listCensusMetadata(
  name = census_name_path,
  vintage = year,
  type = "variables")

# Create a subset of the acs variable name table with just the table of interest
table_var_names <- dec_vars %>%
  filter(group == table_name)
table_var_names <- table_var_names[,1:2]
View(table_var_names)

#################################################################
# Select the desired columns
cols <- c(6,8:55)
working_table <- working_table[,cols]
glimpse(working_table)

#################################################################
# Pivot the Table so you can merge the actual variable names
working_table <- pivot_longer(working_table,
                              cols = 2:ncol(working_table),
                              names_to = "Variable",
                              values_to = "Value")
head(working_table)

# Add the actual variable names by joining table_var_names to working_table
working_table <- working_table %>%
  left_join(table_var_names, c('Variable' = 'name')) 
head(working_table)

###########################################################################
# Again, select the desired columns in the desired order
cols2 <- c(1,4,3)
working_table <- working_table[,cols2]
head(working_table)

# Get rid of rows that are exact duplicates
working_table <- working_table %>% distinct()

# Pivot the table back so that each variable has its own column
working_table <- working_table %>%
  pivot_wider(names_from = label, values_from = Value)
head(working_table)

# Get rid of rows that are exact duplicates
working_table <- working_table %>% distinct()

#####################################################################################################
# Only need to add this section for the first Table that all others will be joined to
# Import the zip code crosswalk table for the desired year
# these files can be downloaded from https://www.huduser.gov/portal/datasets/usps_crosswalk.html
#zip_list <- read_csv(str_c('data/zip_list_', year, '.csv'))
#zip_list$G_ID <- as.character(zip_list$G_ID)
#zip_list$ZIP <- as.character(zip_list$ZIP)
#zip_list <- zip_list[!duplicated(zip_list$G_ID),]
# glimpse(zip_list)

# Import the large crosswalk table with ZCTA5 
# this file can be downloaded from https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt
#cross_list <- read_csv(str_c('data/big_crosswalk.csv'))
#cross_list$GEOID <- as.character(cross_list$GEOID)
#cross_list <- cross_list[!duplicated(cross_list$GEOID),]
#cols <- c(2,6)
#cross_list <- cross_list[,cols]
#glimpse(cross_list)

# Create a new column that modifies the GEO_ID column to match the format of G_ID in the zip_list
#head(zip_list)
#head(cross_list$GEOID)
#head(working_table$GEO_ID)
#working_table$G_ID <- str_sub(working_table$GEO_ID, 10, -5)
#working_table$G_ID2 <- str_sub(working_table$GEO_ID, -5)
#head(working_table$G_ID)

# Add the zip codes by joining zip_list to working_table
#working_table <- working_table %>%
#  left_join(zip_list, by = 'G_ID')

# Add the ZCTA5 codes by joining cross_list to working_table
#working_table <- working_table %>%
#  left_join(cross_list, c('G_ID' = 'GEOID'))

# Remove the G_ID column since you don't need it anymore
#working_table <- select(working_table, -G_ID)
glimpse(working_table)
summary(working_table)

working_table <- working_table %>%
  rename(Total_Male_Pop = `Total!!Male`,
         Male_Under_5 = `Total!!Male!!Under 5 years`,
         Male_5_to_9 = `Total!!Male!!5 to 9 years`,
         Male_10_to_14 = `Total!!Male!!10 to 14 years`,
         Male_15_to_17 = `Total!!Male!!15 to 17 years`,
         Male_18_and_19 = `Total!!Male!!18 and 19 years`,
         Male_20 = `Total!!Male!!20 years`,
         Male_21 = `Total!!Male!!21 years`,
         Male_22_to_24 = `Total!!Male!!22 to 24 years`,
         Male_25_to_29 = `Total!!Male!!25 to 29 years`,
         Male_30_to_34 = `Total!!Male!!30 to 34 years`,
         Male_35_to_39 = `Total!!Male!!35 to 39 years`,
         Male_40_to_44 = `Total!!Male!!40 to 44 years`,
         Male_45_to_49 = `Total!!Male!!45 to 49 years`,
         Male_50_to_54 = `Total!!Male!!50 to 54 years`,
         Male_55_to_59 = `Total!!Male!!55 to 59 years`,
         Male_60_and_61 = `Total!!Male!!60 and 61 years`,
         Male_62_to_64 = `Total!!Male!!62 to 64 years`,
         Male_65_and_66 = `Total!!Male!!65 and 66 years`,
         Male_67_to_69 = `Total!!Male!!67 to 69 years`,
         Male_70_to_74 = `Total!!Male!!70 to 74 years`,
         Male_75_to_79 = `Total!!Male!!75 to 79 years`,
         Male_80_to_84 = `Total!!Male!!80 to 84 years`,
         Male_85_and_over = `Total!!Male!!85 years and over`,
         Total_Female_Pop = `Total!!Female`,
         Female_Under_5 = `Total!!Female!!Under 5 years`,
         Female_5_to_9 = `Total!!Female!!5 to 9 years`,
         Female_10_to_14 = `Total!!Female!!10 to 14 years`,
         Female_15_to_17 = `Total!!Female!!15 to 17 years`,
         Female_18_and_19 = `Total!!Female!!18 and 19 years`,
         Female_20 = `Total!!Female!!20 years`,
         Female_21 = `Total!!Female!!21 years`,
         Female_22_to_24 = `Total!!Female!!22 to 24 years`,
         Female_25_to_29 = `Total!!Female!!25 to 29 years`,
         Female_30_to_34 = `Total!!Female!!30 to 34 years`,
         Female_35_to_39 = `Total!!Female!!35 to 39 years`,
         Female_40_to_44 = `Total!!Female!!40 to 44 years`,
         Female_45_to_49 = `Total!!Female!!45 to 49 years`,
         Female_50_to_54 = `Total!!Female!!50 to 54 years`,
         Female_55_to_59 = `Total!!Female!!55 to 59 years`,
         Female_60_and_61 = `Total!!Female!!60 and 61 years`,
         Female_62_to_64 = `Total!!Female!!62 to 64 years`,
         Female_65_and_66 = `Total!!Female!!65 and 66 years`,
         Female_67_to_69 = `Total!!Female!!67 to 69 years`,
         Female_70_to_74 = `Total!!Female!!70 to 74 years`,
         Female_75_to_79 = `Total!!Female!!75 to 79 years`,
         Female_80_to_84 = `Total!!Female!!80 to 84 years`,
         Female_85_and_over = `Total!!Female!!85 years and over`)

working_table <- working_table %>% distinct()

fwrite(working_table, table_output_new_name)
