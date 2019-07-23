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

#######################################################################################################################
#Classification model
#######################################################################################################################
#1aPredict who is likely to be in work (in survey 1) so that they can intervene at ‘baseline’

set.seed(100)
#TRAIN
trainRowNumbers <- createDataPartition(heart$num, p=0.8, list=FALSE)
#TRAIN
trainData <- heart[trainRowNumbers,]
#TEST
testData <- heart[-trainRowNumbers,]
#TRAIN
model_rpart <- train(num ~ ., data=trainData, method='rpart')
#PREDICT
predicted <- predict(model_rpart, testData[,-length(testData)])

# CLASSIFY
stat.desc(df_pred3$pred3)
quantile(df_pred3$pred3, na.rm = TRUE)
ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3))
ggplot(df_pred3) + 
  geom_density(mapping = aes(x = pred3, colour = numchildren))
# PICK 30%

confusion_matrix <- confusion_matrix %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)

# Choose a subset of columns.
heart <- heart[, c(1:5, 14)]

# Rename columns.
#
# age      - age in years
# sex      - gender (1 = male; 0 = female) 
# cp       - chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptomatic)
# trestbps - resting blood pressure [mm Hg]
# chol     - serum cholestorol
# num      - diagnosis of heart disease (0 = no disease)
#
################################################################################
#### Validation Techniques ####
#### Cross Validation
# CV is a validation technique where we retrain our model on different splits of our 
# data to get an 'average performance' 
# For more information on cross validation: https://towardsdatascience.com/cross-validation-70289113a072
# To control validation techniques during training we can use the train control function
trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
# this function stipulats:
#     - the method of training: Cross validation (cv) 
#     - Number of folds: 10
#     - I our process is going to be chatty: TRUE
model_rpart <- train(num ~ ., data=trainData, method='rpart', trControl = trControl)
# What did verboseIter actually do?
# Let's check on our hyperparameters, how are we evaluating success?
model_rpart$results
# What about if we want a different metric
model_rpart_kappa <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, metric = 'Kappa')
# What if we don't like the defaults for the hyper parameters we're testing?
model_rpart <- train(num ~ ., data=trainData, method='rpart', trControl = trControl, 
                     tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))
# Let's check on our hyperparameters again
model_rpart$results
# Train a new classification model of your choice
new_model <- 
#Compare the two models using resamples
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