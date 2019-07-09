# Aiden
# Description: Cleaning the data
# Created: 07-09-2019
# Last Updated: 07-09-2019
options(scipen=999)
library(tidyverse)
df <- read.csv("data/raw/teaching_training_data.csv")
# READ IN DATA
# now read in data for each assessment
# test for cognitive fluency
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
# communication ability
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
# grit
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
# numeracy
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
# optimism
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")
# each individual should only have one assessment
# set up the data so this is the case...
# also need to only keep the unid and score
df_cft <- df_cft %>% 
  select(unid, cft_score) %>% 
  distinct(unid, .keep_all = TRUE)
# we want to do this for all 5 asssessments
# keep_all keeps the rest of the varables in the data frame instead of just the unid, in this case, keep the scores as well
helper_function <- function(file_name) {
  file_name %>% 
    select(2:3) %>% 
    distinct(unid, .keep_all = TRUE)
}
df_com <- helper_function(df_com)
df_grit <- helper_function(df_grit)
df_num <- helper_function(df_num)
df_opt <- helper_function(df_opt)
df_assess <- full_join(df_cft, df_com, by ="unid")
df_assess <- df_assess %>% 
  full_join(df_grit, by ="unid") %>% 
  full_join(df_num, by ="unid") %>% 
  full_join(df_opt, by ="unid")
df <- full_join(df, df_assess, by ="unid")
rm(df_assess, df_cft, df_com, df_grit, df_num, df_opt)
###############################################################################################################3
# Abe
# Description: Analyses
# Created: 07-09-2019
# Last Updated: 07-09-2019
df <- df[df$survey_num == 1,]