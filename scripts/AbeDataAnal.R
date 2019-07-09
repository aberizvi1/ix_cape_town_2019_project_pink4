
options(scipen=999)
library(tidyverse)
df <- read.csv("data/raw/teaching_training_data.csv")
df_cft <- read.csv("data/raw/teaching_training_data_cft.csv")
df_com <- read.csv("data/raw/teaching_training_data_com.csv")
df_grit <- read.csv("data/raw/teaching_training_data_grit.csv")
df_num <- read.csv("data/raw/teaching_training_data_num.csv")
df_opt <- read.csv("data/raw/teaching_training_data_opt.csv")
df_cft <- df_cft %>% 
  select(unid, cft_score) %>% 
  distinct(unid, .keep_all = TRUE)
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
df <- df[df$survey_num == 1,]
##############################################################################################
df <- subset(df, select = -c(survey_date_month,survey_num,working,job_start_date,job_leave_date,company_size,monthly_pay))
             