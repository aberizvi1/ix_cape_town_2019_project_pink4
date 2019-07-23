options(scipen=999)
library(pastecs)
library(tidyverse)
library(lubridate)
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
df <- subset(df, select = -c(X,survey_date_month,survey_num,job_start_date,job_leave_date,company_size,monthly_pay))
(colSums(is.na(df))*100)/dim(df)[1]
df <- subset(df, select = -c(peoplelive_15plus, num_score, province, numearnincome, com_score, age_at_survey))

df.sub = droplevels(subset(wolf, Population!=3))
df.sub$working = 'Heavy'
df.sub$working[wolf.sub$Population==1] = 'Light'
 Make the variable Hunting a factor
wolf.sub$Hunting = as.factor(wolf.sub$Hunting)
#######################################################################################################################
#Classification model

#######################################################################################################################
#1aPredict who is likely to be in work (in survey 1) so that they can intervene at ‘baseline’

set.seed(1234)
df_train_index <- df %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)
# RUN MODEL
reg1 <- lm(working ~ numchildren, data = df_train)
summary(reg1)
reg2 <- lm(working ~ numchildren + fin_situ_now + anyhhincome, data = df_train)
summary(reg2)
reg3 <- lm(working ~ numchildren + as.factor(fin_situ_now) + anyhhincome, data = df_train)
summary(reg3)
# PREDICT
df_pred3 <- as.data.frame(predict.lm(reg3, df_test)) %>% 
  rename(pred3 = "predict.lm(reg3, df_test)")
df_pred3 <- bind_cols(df_test, df_pred3)
# CLASSIFY
stat.desc(df_pred3$pred3)
quantile(df_pred3$pred3, na.rm = TRUE)
ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3))
ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3, colour = numchildren))
# PICK 30%
df_pred3 <- df_pred3 %>% 
  mutate(binary_pred3 = case_when(pred3 >= 0.3 ~ TRUE, 
                                  pred3 < 0.3 ~ FALSE))
table(df_pred3$binary_pred3, df_pred3$working)
confusion_matrix <- df_pred3 %>% 
  filter(!is.na(binary_pred3)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred3) %>% 
  summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()
ggplot(confusion_matrix) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred3), stat = 'identity')
confusion_matrix <- confusion_matrix %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)

#Is this model good or bad?
#Why?
  #Decent
  #Low p-values
  #Confusion matrix:
  #       FALSE TRUE
  #FALSE  8825  3011
  #TRUE   2152  1025

#1bPredict who is likely to work for more than 6 months


#2Produce insights which might help the organisation think about interventions