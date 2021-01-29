setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
library(data.table)

# Read in the desired table
df <- read.csv(str_c('data/output/by_block/Table_P2_by_block_w_zip.csv'))

glimpse(df)
summary(df)

# Delete areas with no zip or ZCTA5 identifier
df2 <- df %>%
  filter(!is.na(ZIP) & !is.na(ZCTA5))
glimpse(df2)
summary(df2)

fwrite(df2, 'data/output/by_block/Table_P2_CLEAN.csv')

##################################################################
# 
df2$G_ID <- str_sub(df2$GEO_ID, 1, -5)

# Group the block level population by tract using G_ID
df_sum_by_tract <- df2 %>%
  group_by(G_ID) %>%
  summarize(tract_pop = sum(Total_Population))
glimpse(df_sum_by_tract)
head(df_sum_by_tract)

# Join the sum back to the block level Table using G_ID
df_pop_dist <- df2 %>%
  left_join(df_sum_by_tract, by = 'G_ID')
glimpse(df_pop_dist)

cols <- c(6:9,12,13)
df_pop_dist <- df_pop_dist[,cols]
glimpse(df_pop_dist)  
df_pop_dist <- df_pop_dist %>%
  mutate(block_pop_proportion = Total_Population/tract_pop)

tract_sanity <- df_pop_dist %>%
  group_by(G_ID) %>%
  summarize(total = sum(block_pop_proportion))

fwrite(df_pop_dist, 'data/output/by_block/Tract_expansion_by_pop.csv')

#########################################################################
# Create tract level grouping with ZIP
# Group the block level population by tract using G_ID
df_sum_by_tract_zip <- df2 %>%
  group_by(ZIP, G_ID) %>%
  summarize(tract_pop = sum(Total_Population))
glimpse(df_sum_by_tract_zip)
head(df_sum_by_tract_zip)

df_sum_by_zip <- df_sum_by_tract_zip %>%
  group_by(ZIP) %>%
  summarize(zip_pop = sum(tract_pop))
glimpse(df_sum_by_zip)
head(df_sum_by_zip)

# Join the sum back to the tract level Table using ZIP
df_pop_dist_zip <- df_sum_by_tract_zip %>%
  left_join(df_sum_by_zip, by = 'ZIP')
glimpse(df_pop_dist_zip)


df_pop_dist_zip2 <- df_pop_dist_zip %>%
  mutate(zip_pop_proportion = tract_pop/zip_pop)

glimpse(df_pop_dist_zip2)

zip_sanity <- df_pop_dist_zip2 %>%
  group_by(ZIP) %>%
  summarize(total = sum(zip_pop_proportion))

fwrite(df_pop_dist_zip2, 'data/output/by_block/Zip_expansion.csv')


