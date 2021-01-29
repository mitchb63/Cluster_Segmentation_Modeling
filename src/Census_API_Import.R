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

apis <- listCensusApis()
View(apis)

#######################################################################
############################################################################

# Retrieve tract-level data within a specific state and county using a nested 'regionin' argument.  
# NOTE:  You cannot use a wildcard to retrieve all the States in one call.  A loop must be created

# Enter the table number and year for the desired table below.
table_name <- 'B15001' # 'B19052', 'C08301', 'B29002', 'B99231')
survey <- 'acs5'
census_name_path <- 'acs/acs5'
year <- 2019

geo_types <- listCensusMetadata(
  name = "acs/acs5", 
  type = "geography")

table_output_new_name <- str_c('output/by_tract/', survey, '_Table_', table_name, '_', year, '_by_tract_w_zip.csv')

######################################################################################
######################################################################################

# Pull the acs variable Metadata from the api
acs_vars <- listCensusMetadata(
  name = census_name_path,
  vintage = year,
  type = "variables")

# Import the zip code crosswalk table for the desired year
# these files can be downloaded from https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_list <- read_csv(str_c('data/zip_list_', year, '.csv'))
zip_list$G_ID <- as.character(zip_list$G_ID)
zip_list$ZIP <- as.character(zip_list$ZIP)

glimpse(zip_list)

# This loop downloads the data tables one State at a time (51 total w/ DC) and updates you as to what it just did
working_table <- NULL

for (i in 1:length(fips)){                           
  table <- table_name
  state <- fips[i]
  year <- year
  vars <- c(str_c("group(", table, ")"))
  regionin <- str_c("state:", state, "+county:*")
  
  df_temp <- getCensus(name = census_name_path,
                       vintage = year,
                       vars =  vars,                   
                       region = "tract:*",
                       regionin = regionin)
  
  working_table <- rbind(working_table, df_temp)
  print(str_c("Just completed downloading Table ", table_name, " for State #", i, "..."))
}

#######################################################################

head(working_table)

# Create a subset of the acs variable name table with just the table of interest
table_var_names <- acs_vars %>%
  filter(group == table_name)
View(table_var_names)
table_var_names <- table_var_names[,1:2]
View(table_var_names)

# Select the desired columns in the desired order
glimpse(working_table)
cols <- c(1:2,31,32,7,11,15,19,23,27)
working_table <- working_table[,cols]
glimpse(working_table)

# Pivot the Table so you can merge the actual variable names
working_table <- pivot_longer(working_table,
                              cols = 5:ncol(working_table),
                              names_to = "Variable",
                              values_to = "Value")
head(working_table)

# Add the actual variable names by joining table_var_names to working_table
working_table <- working_table %>%
  left_join(table_var_names, c('Variable' = 'name')) 
head(working_table)

# Again, select the desired columns in the desired order
cols2 <- c(1:4,7,6)
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

# Create a new column that modifies the GEO_ID column to match the format of G_ID in the zip_list
head(zip_list)
head(working_table$GEO_ID)
working_table$G_ID <- str_sub(working_table$GEO_ID, 10)
head(working_table$G_ID)

# Add the zip codes by joining zip_list to working_table
working_table <- working_table %>%
  left_join(zip_list, by = 'G_ID')
# Remove the G_ID column since you don't need it anymore
working_table <- select(working_table, -G_ID)
head(working_table)

# Write the file to your folder
fwrite(working_table, table_output_new_name)
