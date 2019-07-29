options(scipen=999)
install.packages("skimr")
install.packages("caret")
install.packages("RANN")
library(caret)
library(skimr)
library(RANN)
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
rm(df_assess, df_cft, df_com, df_grit, df_num, df_opt)
df <- df %>% distinct(unid, .keep_all = TRUE)
df <- df %>% 
  mutate(age_at_survey = (interval(dob, survey_date_month)/years(1))-0.333) %>% 
  mutate(age = floor(age_at_survey) )
df <- subset(df, select = -c(X,survey_date_month,survey_num,job_start_date,job_leave_date,company_size,monthly_pay))
(colSums(is.na(df))*100)/dim(df)[1]
df <- subset(df, select = -c(peoplelive_15plus, num_score, province, numearnincome, com_score, age_at_survey, dob))
df$age_sqrd <- (df$age)^2

df <- df %>% 
  mutate(haschildren = as.numeric(as.character(df$numchildren)) > 0)
################################################################################
#MODEL

reg1 <- lm(working ~ gender, data = df)
summary(reg1)
reg2 <- lm(working ~ gender + numchildren, data = df)
summary(reg2)
reg3 <- lm(working ~ age_sqrd, data = df)
summary(reg3)
reg4 <- lm(working ~ haschildren + gender, data = df)
summary(reg4)
################################################################################
#### Validation Techniques ####
#### Cross Validation
# CV is a validation technique where we retrain our model on different splits of our 
# data to get an 'average performance' 
trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
model_rpart <- train(num ~ ., data=trainData, method='rpart', trControl = trControl)
model_rpart$results
model_rpart_kappa <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, metric = 'Kappa')
model_rpart <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, 
                     tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))
model_rpart$results
# NEW MODEL?
new_model <- 
#COMPARE MODELS
model_comp <- resamples(list(my_new_model = new_model, Rpart = model_rpart))
summary(model_comp)
# Helper code to artificially create data with missing values from the iris data set (run this first!)
iris_missVals <- iris
iris_missVals$Petal.Length[sample(1:nrow(iris_missVals), 50)] <- NA

#1bPredict who is likely to work for more than 6 months


#2Produce insights which might help the organisation think about interventions

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