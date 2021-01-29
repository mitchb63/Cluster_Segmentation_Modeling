setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
library(data.table)

memory.limit(48915)

df <- read_csv('data/output/by_block/Table_P2_CLEAN.csv')
df2 <- read_csv('data/output/by_block/Table_P3_by_block_w_zip.csv')

glimpse(df)
glimpse(df2)

df_join <- df %>%
  left_join(df2, by = 'GEO_ID')

rm(df)
rm(df2)

df3 <- read_csv('data/output/by_block/Table_P4_by_block_w_zip.csv')

glimpse(df3)

df_join2 <- df_join %>%
  left_join(df3, by = 'GEO_ID')

rm(df_join)
rm(df3)

df4 <- read_csv('data/output/by_block/Table_P12_by_block_w_zip.csv')

glimpse(df4)

df_join3 <- df_join2 %>%
  left_join(df4, by = 'GEO_ID')
glimpse(df_join3)

rm(df_join2)
rm(df4)

df5 <- read_csv('data/output/by_block/Table_H13_by_block_w_zip.csv')

glimpse(df5)

df_join4 <- df_join3 %>%
  left_join(df5, by = 'GEO_ID')
glimpse(df_join4)

rm(df_join3)
rm(df5)

df6 <- read_csv('data/output/by_block/Table_B15002_by_block.csv')

glimpse(df6)

df_join5 <- df_join4 %>%
  left_join(df6, by = 'GEO_ID')
glimpse(df_join5)

rm(df_join4)
rm(df6)

fwrite(df_join5, 'data/output/by_block/df_join5.csv')
########################################################################

df_join5 <- read_csv('data/output/by_block/df_join5.csv')
glimpse(df_join5)
summary(df_join5)

df7 <- fread('data/output/by_block/Table_B19013_by_block.csv')

glimpse(df7)
# Get rid of rows that are exact duplicates
df7 <- df7 %>% distinct()

df_join6 <- df_join5 %>%
  left_join(df7, by = 'GEO_ID')
glimpse(df_join6)

rm(df_join5)
rm(df7)

df8 <- fread('data/output/by_block/Table_B28011_by_block.csv')
# Get rid of rows that are exact duplicates
df8 <- df8 %>% distinct()
glimpse(df8)

df_join7 <- df_join6 %>%
  left_join(df8, by = 'GEO_ID')

# Delete areas with no zip or ZCTA5 identifier
df_join7 <- df_join7 %>%
  filter(!is.na(ZIP) & !is.na(ZCTA5))
glimpse(df_join7)

rm(df_join6)
rm(df8)

df9 <- fread('data/output/by_block/Table_B23025_by_block.csv')
# Get rid of rows that are exact duplicates
df9 <- df9 %>% distinct()
glimpse(df9)

df_join8 <- df_join7 %>%
  left_join(df9, by = 'GEO_ID')

# Delete areas with no zip or ZCTA5 identifier
df_join8 <- df_join8 %>%
  filter(!is.na(ZIP) & !is.na(ZCTA5))
glimpse(df_join8)

rm(df_join7)
rm(df9)

df10 <- fread('data/output/by_block/Table_B08301_by_block.csv')
# Get rid of rows that are exact duplicates
df10 <- df10 %>% distinct()
glimpse(df10)

df_join9 <- df_join8 %>%
  left_join(df10, by = 'GEO_ID')

# Delete areas with no zip or ZCTA5 identifier
df_join9 <- df_join9 %>%
  filter(!is.na(ZIP) & !is.na(ZCTA5))
glimpse(df_join9)

rm(df_join8)
rm(df10)

df11 <- fread('data/output/by_block/USAC_by_block.csv')
# Get rid of rows that are exact duplicates
df11 <- df11 %>% distinct()
glimpse(df11)
summary(df11)

df_join10 <- df_join9 %>%
  left_join(df11, by = 'GEO_ID')

# Delete areas with no zip or ZCTA5 identifier
df_join10 <- df_join10 %>%
  filter(!is.na(ZIP) & !is.na(ZCTA5))
glimpse(df_join10)

rm(df_join9)
rm(df11)

fwrite(df_join10, 'data/output/by_block/df_join10.csv')


####################################################
# Need to add fips to main Table

df_join10 <- fread('data/output/by_block/df_join10.csv')
df_join10$G_ID <- str_sub(df_join10$GEO_ID, 10, -5)
glimpse(df_join10)
df_fips <- read_csv('data/output/by_tract/join/Complete_Tract_Dataset.csv')
cols <- c(4,8)
df_fips <- df_fips[,cols]
glimpse(df_fips)
df_fips$GEO_ID <- str_sub(df_fips$GEO_ID, 10)
df_join10 <- df_join10 %>%
  left_join(df_fips, c('G_ID' = 'GEO_ID'))
df_join10 <- select(df_join10, -G_ID)
rm(df_fips)

####################################################

df12 <- fread('data/output/by_block/min_temp_join.csv')
# Get rid of rows that are exact duplicates
df12 <- df12 %>% distinct()
glimpse(df12)
summary(df12)

df_join11 <- df_join10 %>%
  left_join(df12, by = 'fips')

# Delete areas with no zip or ZCTA5 identifier
df_join11 <- df_join11 %>%
  filter(!is.na(ZIP) & !is.na(ZCTA5))
glimpse(df_join11)

rm(df_join10)
rm(df12)

# Select and reorder columns
cols <- c(1:8,154,9:153,155)
df_final <- df_join11[,c(1:8,154,9:153,155)]
glimpse(df_final)
summary(df_final)

fwrite(df_final, 'data/output/by_block/Complete_Block_Dataset.csv')

