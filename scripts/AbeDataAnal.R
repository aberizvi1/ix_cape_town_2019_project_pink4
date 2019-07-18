
options(scipen=999)

if(!require("caret")){
  install.packages("caret")
}
if(!require("skimr")){
  install.packages("skimr")
}
if(!require("RANN")){
  install.packages("RANN")
}
library(caret)
library(skimr)
library(RANN)
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
df <- subset(df, select = -c(survey_date_month,survey_num,job_start_date,job_leave_date,company_size,monthly_pay))
#######################################################################################################################
#Practice decision tree

#Practice linear regression

#######################################################################################################################
set.seed(1234)
df_train_index <- df %>%
  select(unid) %>% 
  distinct() %>% 
  sample_frac(0.7)
# run a regression model
reg1 <- lm(working ~ gender, data = df_train)
summary(reg1)
reg2 <- lm(working ~ gender + fin_situ_now + anyhhincome, data = df_train)
summary(reg2)
reg3 <- lm(working ~ gender + as.factor(fin_situ_now) + anyhhincome, data = df_train)
summary(reg3)
# predict
df_pred3 <- as.data.frame(predict.lm(reg3, df_test)) %>% 
  rename(pred3 = "predict.lm(reg3, df_test)")
# then bind together
df_pred3 <- bind_cols(df_test, df_pred3)
# now manually classify
stat.desc(df_pred3$pred3)
quantile(df_pred3$pred3, na.rm = TRUE)
ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3))
ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3, colour = gender))
# pick 30%
df_pred3 <- df_pred3 %>% 
  mutate(binary_pred3 = case_when(pred3 >= 0.3 ~ TRUE, 
                                  pred3 < 0.3 ~ FALSE))
table(df_pred3$binary_pred3, df_pred3$working)
# Might be easier to group_by
confusion_matrix <- df_pred3 %>% 
  filter(!is.na(binary_pred3)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred3) %>% 
  summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()
# ggplot
ggplot(confusion_matrix) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred3), stat = 'identity')
# proportions
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