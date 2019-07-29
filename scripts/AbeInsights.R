# Aiden
# Created: 07-28-2019
# Last Updated: 07-28-2019
options(scipen=999)
library(tidyverse)
library(dplyr)
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
df <- df %>% distinct(unid, .keep_all = TRUE)
df <- df %>% 
  mutate(age_at_survey = (interval(dob, survey_date_month)/years(1))-0.333) %>% 
  mutate(age = floor(age_at_survey) )
df <- df %>% mutate(ext_supp = case_when((df$anygrant == 'TRUE' | df$anyhhincome == 'TRUE') ~ 'TRUE',
                                         (df$anygrant == 'FALSE' | df$anyhhincome == 'FALSE' ~ 'FALSE')))
df <- df %>% 
  mutate(haschildren = as.numeric(as.character(df$numchildren)) > 0)
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)
df <- df %>% 
  mutate(numincome = parse_number(as.character(numearnincome))) %>% 
  mutate(hasincome = numincome > 0)
df <- df %>% mutate(out_acti = case_when((df$volunteer == 'Yes' | df$leadershiprole == 'Yes') ~ 'TRUE',
                                         (df$volunteer == 'No' | df$leadershiprole == 'No' ~ 'FALSE')))
######################################################################################################################
# Abe
# Created: 07-28-2019
# Last Updated: 07-28-2019
# 1st: anygrant & anyhhincome
# 2nd: volunteer & leadershiprole
# 3rd: numchildren
# insights on age vs working
# insights on scores vs working
# 4th: financial situation change  
# 5th: cft score