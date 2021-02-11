
library(tidyverse)

memory.limit(48915)

df <- read_csv('data/output/by_block/Block_Dataset_IMPUTED.csv')
glimpse(df)

cols <- c(6,10:114)
df_trim <- df[,cols]
glimpse(df_trim)

df_k <- df_trim %>% remove_rownames %>% column_to_rownames(var="GEO_ID")

glimpse(df_k)
rm(df_trim)

df_k <- df_k %>%
  mutate(Male_under_10 = Male_Under_5 + Male_5_to_9,
         Male_10_to_19 = Male_10_to_14 + Male_15_to_19,
         Male_20_to_34 = Male_20_to_24 + Male_25_to_29 + Male_30_to_34,
         Male_35_to_64 = Male_35_to_39 + Male_40_to_44 + Male_45_to_49 + Male_50_to_54 + Male_55_to_59 + Male_60_to_64,
         Male_65_and_over = Male_65_to_69 + Male_70_to_74 + Male_75_to_79 + Male_80_to_84 + Male_85_and_over,
         Female_under_10 = Female_Under_5 + Female_5_to_9,
         Female_10_to_19 = Female_10_to_14 + Female_15_to_19,
         Female_20_to_34 = Female_20_to_24 + Female_25_to_29 + Female_30_to_34,
         Female_35_to_64 = Female_35_to_39 + Female_40_to_44 + Female_45_to_49 + Female_50_to_54 + Female_55_to_59 +  Female_60_to_64,
         Female_65_and_over = Female_65_to_69 + Female_70_to_74 + Female_75_to_79 + Female_80_to_84 + Female_85_and_over,
         Male_No_diploma = Male_no_diploma + Male_No_schooling_completed,
         Male_Some_College = Male_Some_college + Male_Associates_degree,
         Male_Graduate_degree = Male_Masters_degree + Male_Professional_school_degree + Male_Doctorate_degree,
         Female_No_diploma = Female_no_diploma + Female_No_schooling_completed,
         Female_Some_College = Female_Some_college + Female_Associates_degree,
         Female_Graduate_degree = Female_Masters_degree + Female_Professional_school_degree + Female_Doctorate_degree)

glimpse(df_k)
cols <- c(1:8,11,12,106:110,31,111:115,116,52,117,55,118,119,61,120,64,121,68:77,80,81,84:105)
df_k <- df_k[,cols]
glimpse(df_k)

#scale var1 and var2 to have mean = 0 and standard deviation = 1
df_k_scaled <- df_k %>% mutate_at(c('Male_under_10', 'Male_10_to_19', 'Male_20_to_34', 'Male_35_to_64', 'Male_65_and_over',
                                    'Female_under_10', 'Female_10_to_19', 'Female_20_to_34', 'Female_35_to_64', 'Female_65_and_over',
                                    'Male_No_diploma', 'Male_Some_College', 'Male_Graduate_degree',
                                    'Female_No_diploma', 'Female_Some_College', 'Female_Graduate_degree'), ~(scale(.) %>% as.vector))

apply(is.na(df_k_scaled), 2, which)
summary(df_k_scaled$Total_civilian_labor_force_employeed)  
df_k_scaled$Total_civilian_labor_force_employeed[is.na(df_k_scaled$Total_civilian_labor_force_employeed)] <- mean(df_k_scaled$Total_civilian_labor_force_employeed, 
                                                                                                                  na.rm=TRUE)

glimpse(df_k_scaled)
library(data.table)
fwrite(df_k_scaled, 'data/output/by_block/Block_Dataset_for_k_means.csv')

rm(df)

# Compute optimal Number of clusters using Elbow Method
set.seed(45)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df_k_scaled, k, nstart = 5 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:35

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

write.csv(wss_values, 'data/output/by_block/wss_values_k_35.csv')

k_model_16 <- kmeans(df_k_scaled, centers = 16, nstart = 50)
library(factoextra)
fviz_cluster(k_model_16, data = df_k_scaled)

write.csv(k_model_16$cluster, 'data/output/by_block/cluster_vector_k_16.csv')
