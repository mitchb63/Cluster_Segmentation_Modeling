
library(tidyverse)
library(mice)

memory.limit(48915)

df <- read_csv('data/output/by_block/Complete_Block_Dataset_CLEAN.csv')
glimpse(df)

cl_vector <- read_csv('data/output/by_block/cluster_vector_k_16.csv')
glimpse(cl_vector)
cl_vector <- cl_vector[,2] %>%
  rename(cluster = x)

df_clusters <- df %>%
  cbind(cl_vector)

glimpse(df_clusters)
rm(df)

df_clusters2 <- df_clusters %>%
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

glimpse(df_clusters2)
cols <- c(1:9,115,10:21,116:120,40,121:125,126,61,127,64,128,129,70,130,73,131,77:86,89,90,93:99,100:114)
df_k <- df_clusters2[,cols]
glimpse(df_k)

Clusters_imputed <- mice(df_k, m=1, maxit = 1, method = 'mean', seed = 45)
glimpse(Clusters_imputed)
summary(Clusters_imputed)

#get complete data 
df_clusters <- complete(Clusters_imputed,1)
glimpse(df_clusters)
summary(df_clusters)

library(data.table)
fwrite(df_clusters, 'data/output/by_block/data_w_clusters_complete.csv')
detach("package:data.table", unload=TRUE)

rm(cl_vector)
rm(Clusters_imputed)
rm(df_clusters2)
rm(df_k)

cluster_means <- df_clusters %>%
  group_by(cluster) %>%
  summarize_all('mean')

glimpse(cluster_means)
summary(cluster_means)

library(data.table)
fwrite(cluster_means, 'data/output/by_block/cluster_means.csv')
detach("package:data.table", unload=TRUE)

# Hierarchical clustering to determine which characteristics most define the cluster segments
df_means_edit <- read.csv('data/output/by_block/cluster_means_edit.csv', row.names = 1)
glimpse(df_means_edit)

cols <- c(3,5)
h_clust_matrix <- df_means_edit[,cols]
glimpse(h_clust_matrix)

matrix_scaled <- scale(h_clust_matrix)

hc_complete <- hclust(dist(matrix_scaled), method = 'complete')
hc_average <- hclust(dist(matrix_scaled), method = 'average')
hc_single <- hclust(dist(matrix_scaled), method = 'single')

plot(hc_complete, main = 'Complete Linkage', xlab = '', sub = '', cex = .9)
plot(hc_average, main = 'Average Linkage', xlab = '', sub = '', cex = .9)
plot(hc_single, main = 'Single Linkage', xlab = '', sub = '', cex = .9)


df<- read_csv('data/output/by_block/data_w_clusters_complete.csv')
glimpse(df)
names <- read_csv('data/output/by_block/cluster_names.csv')
df <- df %>%
  left_join(names, by = 'cluster')
library(data.table)
fwrite(df, 'data/output/by_block/data_w_clusters_complete.csv')
detach("package:data.table", unload=TRUE)

cols <- c(10:77)
df <- df[,cols]

cluster_sums <- df %>%
  group_by(cluster) %>%
  summarize_all('sum')

glimpse(cluster_sums)

write.csv(cluster_sums, 'data/output/by_block/cluster_means.csv')
