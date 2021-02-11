setwd("C:/Users/mitch/OneDrive/ANLT-530")

library(tidyverse)
library(mice)
library(VIM)
library(lattice)
memory.limit(48915)

df <- read_csv('data/output/by_block/Complete_Block_Dataset_CLEAN_SCALED.csv')
glimpse(df)
summary(df)

md.pattern(df)

imputed_Data <- mice(df, m=1, maxit = 1, method = 'mean', seed = 45)
glimpse(imputed_Data)
summary(imputed_Data)

#get complete data ( 2nd out of 5)
df_imputed <- complete(imputed_Data,1)

glimpse(df_imputed)
summary(df_imputed)

library(data.table)
fwrite(df_imputed, 'data/output/by_block/Block_Dataset_IMPUTED.csv')
