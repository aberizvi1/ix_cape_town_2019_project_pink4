# Aiden
# Description: Cleaning the data
# Created: 07-18-2019
# Last Updated: 07-18-2019

options(scipen=999)
library(tidyverse)
library(lubridate)

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

# Remove repeating survey_num
df <- df %>% distinct(unid, .keep_all = TRUE)
df <- df %>% 
  mutate(age_at_survey = (interval(dob, survey_date_month)/years(1))-0.333) %>% 
  mutate(age = floor(age_at_survey) )
# Removing post-first survey columns
df <- subset(df, select = -c(X,survey_date_month,survey_num,job_start_date,job_leave_date,company_size,monthly_pay))
df <- subset(df, select = -c(peoplelive_15plus, num_score, province, numearnincome, com_score, age_at_survey, dob))

# Add haschildren (T or F)
df <- df %>% 
  mutate(haschildren = as.numeric(as.character(df$numchildren)) > 0)

# histogram of cft_score --> normal distribution
ggplot(df, aes(x=cft_score)) + geom_histogram()

# Mean is not a good value to fill in as the far end values pull the mean up
# Mode is not a bad choice
# This is a pretty normal distribution
# great outliners --> median
ggplot(df, aes(x = age)) + geom_histogram()
#Fill in age with median age
df$age[is.na(df$age)] <- median(df$age, na.rm = TRUE)

# Add age squared
df$age_sqrd <- (df$age)^2

(colSums(is.na(df))*100)/dim(df)[1]

# getting rid of missing gender/anygrant/anyincome rows
df <- df %>% filter(!is.na(gender)) %>% filter(!is.na(anygrant))

# since no statistical significance between numchildren and working, drop numchildren column (added haschildren)
df <- subset(df, select = -c(numchildren))

# very high statistical significance between givemoney and working, hence impute (?)
reg_givemoney <- lm(working ~ givemoney_yes, data = df)
summary(reg_givemoney)

# Finding correlation between each variable and working
#df_numeric <- sapply(df, as.numeric)
#install.packages("corrplot")
#library(corrplot)
#corrplot(cor(df_numeric))
#df_numeric.cor = cor(df_numeric)
#df_numeric.cor
plot(df$working, df$age)

#Running linear regression
reg1 <- lm(working ~ gender, data = df)
summary(reg1)

reg2 <- lm(working ~ gender + numchildren, data = df)
summary(reg2)

reg3 <- lm(working ~ age_sqrd, data = df)
summary(reg3)

reg4 <- lm(working ~ haschildren + gender, data = df)
summary(reg4)


# Split data into training and testing sets
set.seed(100)

trainRowNumbers_rpart <- createDataPartition(df$working, p=0.8, list=FALSE)
trainData_rpart <- df[trainRowNumbers_rpart,]
testData_rpart <- df[-trainRowNumbers_rpart,]
model_rpart <- train(as.factor(working) ~ age_sqrd, data=trainData_rpart, method='rpart')
predicted_rpart <- predict(model_rpart, testData_rpart[,-length(testData_rpart)])



# filter out the rows with NAs in gender column
df_gen_filtered <- df %>% filter(!is.na(gender))

# Step 1: Get row numbers for the training data
trainRowNumbers1 <- createDataPartition(df_gen_filtered$working, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData1 <- df_gen_filtered[trainRowNumbers1,]

# Step 3: Create the test dataset
testData1 <- df_gen_filtered[-trainRowNumbers1,]

# Train a logistic regression model (glm)
model_glm1 <- train(as.factor(working) ~ age * gender, data=trainData1, method='glm')

# Now let's predict for our test data
# type='prob' gives the probability of working of each obs
predicted1 <- predict(model_glm1, type='prob', testData1[,-length(testData1)])

model_glm1
predicted1

# Filter out the NAs in anygrant/anyhhincome
df_ext_money <- df_gen_filtered %>% filter(!is.na(anygrant))

# Step 1: Get row numbers for the training data
trainRowNumbers2 <- createDataPartition(df_ext_money$working, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData2 <- df_ext_money[trainRowNumbers2,]

# Step 3: Create the test dataset
testData2 <- df_ext_money[-trainRowNumbers2,]

# Train a logistic regression model (glm)
model_glm2 <- train(as.factor(working) ~ age + gender + anygrant + anyhhincome, data=trainData2, method='glm')

# Now let's predict for our test data
# type='prob' gives the probability of working of each obs
predicted2 <- predict(model_glm2, type='prob', testData2[,-length(testData2)])


predicted2
model_glm2


# Conor's imputation code
preProcess_missingdata_model <- preProcess(df_ext_money, method='knnImpute') 
preProcess_missingdata_model 
heart_2 <- predict(preProcess_missingdata_model, newdata = df_ext_money)
heart_2




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

