options(scipen=999)

library(tidyverse)
library(lubridate)
library(caret)
library(pastecs)
library(xgboost)
library(dplyr)

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

#Remove repeating survey_num
df <- df %>% distinct(unid, .keep_all = TRUE)
df <- df %>% 
  mutate(age_at_survey = (interval(dob, survey_date_month)/years(1))-0.333) %>% 
  mutate(age = floor(age_at_survey) )

df$survey_date_month <- as.Date(df$survey_date_month)

df$survey_date_month <- df$survey_date_month %m-% months(4)

df$survey_date_month <- as.numeric(df$survey_date_month)

#Removing post-first survey columns
df <- subset(df, select = -c(X,survey_num,job_start_date,job_leave_date,company_size,monthly_pay))

#Plot which shows the non-random distribution of missing values according to the survey date
dfsort <- df[order(df[,2]),] 
par(mar=c(4 ,8 ,0.1 ,0.1))
image(!is.na(dfsort),axes=FALSE,col=gray(0:1))
title(xlab=paste("Inmates sorted by",names(dfsort)[2]))
axis(2,at=(0:23)/23,labels=names(dfsort),las =2)
axis(1,at=100*(0:1000)/850,100*(0:1000))

#Variables which are unnecessary or are missing too many values are removed
df <- subset(df, select = -c(opt_score, num_score, com_score, dob, unid))

#Data is partitioned into 3 dataframes with the aid of the NAs plot.
df_train1 <- df[df$survey_date_month <= 17198,]
df_train2 <- df[df$survey_date_month > 17198 & df$survey_date_month <= 17348,]
df_train3 <- df[df$survey_date_month > 17348,]

#Variables are removed from each partition where necessary
df_train1 <- subset(df_train1, select = -c(peoplelive_15plus, province))
df_train2 <- subset(df_train2, select = -c(numearnincome, cft_score))
df_train3 <- subset(df_train3, select = -c(numearnincome, cft_score, givemoney_yes, financial_situation_now, financial_situation_5years, numearnincome, numchildren, peoplelive_15plus, peoplelive, leadershiprole, volunteer, province))

#Missing numeric values are imputed using KNN imputation
preProcess_missingdata_1 <- preProcess(df_train1, method='knnImpute') 
preProcess_missingdata_1 
df_train1 <- predict(preProcess_missingdata_1, newdata = df_train1)

preProcess_missingdata_2 <- preProcess(df_train2, method='knnImpute') 
preProcess_missingdata_2 
df_train2 <- predict(preProcess_missingdata_2, newdata = df_train2)

preProcess_missingdata_3 <- preProcess(df_train3, method='knnImpute') 
preProcess_missingdata_3 
df_train3 <- predict(preProcess_missingdata_3, newdata = df_train3)

#The small number of remaining observations with missing values are removed
df_train1 <- na.omit(df_train1)
df_train2 <- na.omit(df_train2)
df_train3 <- na.omit(df_train3)

#Convert the dependent variable to a factor variable
df_train1$working <- as.factor(df_train1$working)
df_train2$working <- as.factor(df_train2$working)
df_train3$working <- as.factor(df_train3$working)

#K nearest neighbours is ran on each of the 3 partitions
trControl = trainControl(method="cv", number = 10, verboseIter = T)

system.time(rf_fit1 <- train(working ~ ., 
                             data = df_train1, 
                             method = 'knn', trControl = trControl))

system.time(rf_fit2 <- train(working ~ ., 
                             data = df_train2, 
                             method = 'knn', trControl = trControl))

system.time(rf_fit3 <- train(working ~ ., 
                             data = df_train3, 
                             method = 'knn', trControl = trControl))

#Read in the test data
df_test1 <- read.csv("?/df_test1.csv")
df_test2 <- read.csv("?/df_test2.csv")
df_test3 <- read.csv("?/df_test3.csv")
df_test <- read.csv("?/df_test.csv")

#Generate predictions for each partition
predtest1 <- predict(rf_fit1, df_test1)
(results1 <- confusionMatrix(predtest1, df_test1$working))

predtest2 <- predict(rf_fit2, df_test2)
(results2 <- confusionMatrix(predtest2, df_test2$working))

predtest3 <- predict(rf_fit3, df_test3)
(results3 <- confusionMatrix(predtest3, df_test3$working))

#Overall accuracy
(overall.accuracy <- (results1$overall['Accuracy']*dim(df_test1)[1]) + (results2$overall['Accuracy']*dim(df_test2)[1]) + (results3$overall['Accuracy']*dim(df_test3)[1]))/((dim(df_test1)[1]) + (dim(df_test2)[1]) + (dim(df_test3)[1]))






