options(scipen=999)

library(tidyverse)
library(lubridate)
library(caret)
library(pastecs)
library(xgboost)
library(dplyr)
#Not sure about the file paths and names!
df <- read.csv("data/raw/teaching_TESTING_data.csv")

# READ IN DATA

# now read in data for each assessment
# test for cognitive fluency
df_cft <- read.csv("data/raw/teaching_TESTING_data_cft.csv")
# communication ability
df_com <- read.csv("data/raw/teaching_TESTING_data_com.csv")
# grit
df_grit <- read.csv("data/raw/teaching_TESTING_data_grit.csv")
# numeracy
df_num <- read.csv("data/raw/teaching_TESTING_data_num.csv")
# optimism
df_opt <- read.csv("data/raw/teaching_TESTING_data_opt.csv")


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

#Sort dataframe in order of survey date
df <- df[order(df[,2]),] 

#Missing value count by each covariate
(colSums(is.na(df))*100)/dim(df)[1]

#Plot which shows the non-random distribution of missing values according to the survey date
par(mar=c(4 ,8 ,0.1 ,0.1))
image(!is.na(df),axes=FALSE,col=gray(0:1))
title(xlab=paste("Inmates sorted by",names(df)[2]))
axis(2,at=(0:23)/23,labels=names(df),las =2)
axis(1,at=100*(0:1000)/850,100*(0:1000))

#Variables which are unnecessary or are missing too many values are removed
df <- subset(df, select = -c(opt_score, num_score, com_score, dob, unid))

#Data is partitioned into 3 dataframes with the aid of the NAs plot.
df_1 <- df[df$survey_date_month <= 17198,]
df_1 <- df[df$survey_date_month > 17198 & df$survey_date_month <= 17348,]
df_3 <- df[df$survey_date_month > 17348,]

#Variables are removed from each partition where necessary
df_1 <- subset(df_1, select = -c(peoplelive_15plus, province))
df_2 <- subset(df_2, select = -c(numearnincome, cft_score))
df_3 <- subset(df_3, select = -c(numearnincome, cft_score, givemoney_yes, financial_situation_now, financial_situation_5years, numearnincome, numchildren, peoplelive_15plus, peoplelive, leadershiprole, volunteer, province))

#Missing numeric values are imputed using KNN imputation
preProcess_missingdata_1 <- preProcess(df_1, method='knnImpute') 
preProcess_missingdata_1 
df_1 <- predict(preProcess_missingdata_1, newdata = df_1)

preProcess_missingdata_2 <- preProcess(df_2, method='knnImpute') 
preProcess_missingdata_2 
df_2 <- predict(preProcess_missingdata_2, newdata = df_2)

preProcess_missingdata_3 <- preProcess(df_3, method='knnImpute') 
preProcess_missingdata_3 
df_3 <- predict(preProcess_missingdata_3, newdata = df_3)

#The small number of remaining observations with missing values are removed
df_1 <- na.omit(df_1)
df_2 <- na.omit(df_2)
df_3 <- na.omit(df_3)

#Convert the dependent variable to a factor variable
df_1$working <- as.factor(df_1$working)
df_2$working <- as.factor(df_2$working)
df_3$working <- as.factor(df_3$working)

df_test1 <- df_1
df_test2 <- df_2
df_test3 <- df_3
df_test <- df

write.csv(df_test1, PATH, row.names = FALSE)
write.csv(df_test2, PATH, row.names = FALSE)
write.csv(df_test3, PATH, row.names = FALSE)
write.csv(df_test, PATH, row.names = FALSE)

