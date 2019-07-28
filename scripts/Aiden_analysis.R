# Aiden
# Created: 07-18-2019
# Last Updated: 07-25-2019

options(scipen=999)
library(tidyverse)
library(lubridate)
library(caret)
library(skimr)
library(RANN)
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
# keep_all keeps the rest of the varables in the data frame instead of just the unid, 
# in this case, keep the scores as well
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

# Remove repeating survey_num (survey_num > 2)
df <- df %>% distinct(unid, .keep_all = TRUE)
# Adding age column
df <- df %>% 
  mutate(age_at_survey = (interval(dob, survey_date_month)/years(1))-0.333) %>% 
  mutate(age = floor(age_at_survey) )
# Removing post-first survey columns
df <- subset(df, select = -c(X,survey_date_month,survey_num,job_start_date,job_leave_date,company_size,monthly_pay))

(colSums(is.na(df))*100)/dim(df)[1]
# Removing columns with more than 40% missing values
df <- subset(df, select = -c(peoplelive_15plus, num_score, province, numearnincome, com_score, age_at_survey, dob))

reg_childrennum <- lm(working ~ numchildren, data = df)
summary(reg_childrennum)

# Found no statistical significance between number of children and working
# Feature Engineering: adding haschildren (T or F) children
df <- df %>% 
  mutate(haschildren = as.numeric(as.character(df$numchildren)) > 0)

# since no statistical significance between numchildren and working, drop numchildren column (added haschildren)
df <- subset(df, select = -c(numchildren))

reg_haschildren <- lm(working ~ haschildren, data = df)
summary(reg_haschildren)

# There is great statistical significance between age and working, hence impute age
reg_age <- lm(working ~ age, data = df)
summary(reg_age)

# Mean is not a good value to fill in as the far end values pull the mean up
# Mode is not a bad choice
# This is a pretty normal distribution
# great outliners --> median
ggplot(df, aes(x = age)) + geom_histogram()
# impute age with median age
df$age[is.na(df$age)] <- median(df$age, na.rm = TRUE)

# Add age squared
#df$age_sqrd <- (df$age)^2

(colSums(is.na(df))*100)/dim(df)[1]

# getting rid of missing gender (0.05% NAs)
# OR impute with females (mode)
df <- df %>% filter(!is.na(gender))

# getting rid of unid column
df <- subset(df, select = -c(unid))




# Approach 1:
# seems like the rows with missing values in 'volunteer' also has missing values
# in 'leadershiprole'
# hence, filter out those rows
# extra 40 rows filtered out by filtering the rows with NAs in 'peoplelive'
df <- df %>% filter(!is.na(volunteer)) %>% filter(!is.na(leadershiprole)) %>% 
  filter(!is.na(peoplelive))

# filter out rest of the rows with NAs
# financial sit now & financial sit 5 years have missing values in the same rows
df <- df %>% filter(!is.na(financial_situation_now)) %>% filter(!is.na(financial_situation_5years))

# haschildren has 0.25% NAs and givemoney_yes has 2.25% NAs, hence filter rows with NAs in those out
df <- df %>% filter(!is.na(haschildren)) %>% filter(!is.na(givemoney_yes))

# only cft_score column has NAs now
(colSums(is.na(df))*100)/dim(df)[1]

# since there's only about 24% of NAs in cft_score, impute(?)
# histogram of cft_score --> normal distribution --> impute with mean
ggplot(df, aes(x=cft_score)) + geom_histogram()
df$cft_score[is.na(df$cft_score)] <- mean(df$cft_score, na.rm = TRUE)

# check if all the columns are filled
(colSums(is.na(df))*100)/dim(df)[1]


# Splitting data into training and testing dataset
trainRowNumbers <- createDataPartition(df$working, p=0.8, list=FALSE)
trainData <- df[trainRowNumbers,]
testData <- df[-trainRowNumbers,]

# cv
trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

model_glm_alt <- glm(as.factor(working) ~ ., data=testData, family = binomial("logit"))
summary(model_glm_alt)
model_glm_alt = step(model_glm_alt)
summary(model_glm_alt)

model_glm_alt_pred <- as.data.frame(predict(model_glm_alt, data = testData, type = 'response')) %>% 
  rename(pred1 = 'predict(model_glm_alt, data = testData, type = \"response\")')
model_glm_alt_pred <- bind_cols(testData, model_glm_alt_pred)
model_glm_alt_pred <- model_glm_alt_pred %>% mutate(binary_pred = case_when(pred1 >= 0.30 ~ TRUE,
                                                                           pred1 < 0.30 ~ FALSE))

quantile(model_glm_alt_pred$pred1, na.rm = TRUE)

confusionMatrix(as.factor(model_glm_alt$binary_pred), as.factor(testData$working))

confusion_matrix <- model_glm_alt_pred %>% 
  filter(!is.na(binary_pred)) %>% 
  mutate(total_obs = n()) %>% 
  group_by(working, binary_pred) %>% 
  summarise(nobs = n(), total_obs = mean(total_obs)) %>% 
  group_by(working) %>% 
  mutate(total_working = sum(nobs)) %>% 
  ungroup()
ggplot(confusion_matrix) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred), stat = 'identity')




# glm model on entire df
model_glm <- train(as.factor(working) ~ ., data=trainData, method='glm', trControl = trControl)
predicted_glm <- predict(model_glm, testData, type = 'prob')
summary_glm <- summary(model_glm)


predict_false = predicted_glm[,1]
predict_true = predicted_glm[,2]
predictedData <- testData %>% cbind(predict_false) %>% cbind(predict_true)
names(predictedData)

predictedData <- predictedData %>% mutate(prediction_glm = case_when(as.numeric(predict_false) > 0.62 ~ 'FALSE',
                                                as.numeric(predict_false) < 0.62 ~ 'TRUE'))
list( summary_glm$coefficient, 
      round( 1 - ( summary_glm$deviance / summary_glm$null.deviance ), 2 ) )

caret::confusionMatrix(as.factor(predictedData$prediction_glm), as.factor(testData$working))




# rpart model on entire df (decision tree)
model_rpart <- train(as.factor(working) ~ ., data=trainData, method='rpart', trControl = trControl)
predicted_rpart <- predict(model_rpart, testData)
model_rpart$results

caret::confusionMatrix(as.factor(predicted_rpart), as.factor(testData$working))






# Approach 2
# seems like there are a couple paired variables that have NAs in the same rows
# Pair 1: anygrant & anyhhincome (10% missing) (combine into one col of external support?)
# Pair 2: volunteer & leadershiprole (38% missing) (combine into one col of external activities?)
# Pair 3: fin_situ_now & fin_situ_5years (40% missing) --> big change --> optimistic/working?

# convert to character to replace NAs with 'None'
df$volunteer <- as.character(df$volunteer)
df$volunteer[is.na(df$volunteer)] <- 'None'
df$leadershiprole <- as.character(df$leadershiprole)
df$leadershiprole[is.na(df$leadershiprole)] <- 'None'



# merging anygrants & anyhhincome to an external support column (ext_supp)
df <- df %>% mutate(ext_supp = case_when((df$anygrant == 'TRUE' | df$anyhhincome == 'TRUE') ~ 'TRUE',
                                          (df$anygrant == 'FALSE' | df$anyhhincome == 'FALSE' ~ 'FALSE')))

# roughly 6% more likely to be working if given external support, perhaps due to connection (anyhhincome)
# or better resources (grants) --> great statistical significance
reg_ext_supp = lm(working ~ ext_supp, data = df)
summary(reg_ext_supp)



# merging volunteer & leadershiprole to an outside activities column (out_acti)
df <- df %>% mutate(out_acti = case_when((df$volunteer == 'Yes' | df$leadershiprole == 'Yes') ~ 'TRUE',
                                         (df$volunteer == 'No' | df$leadershiprole == 'No' ~ 'FALSE')))

# roughly 2% more likely to be working if having done outside activities, perhaps due to better leadership
# qualities and/or dedicated to community --> great statistical significance
reg_out_acti = lm(working ~ out_acti, data = df)
summary(reg_out_acti)


# adding financial situation change column, singling out financial situation now/financial situation change
# that is 6 and above, and test with if they are working or not
# no statistical significance
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)
df <- subset(df, select = -c(financial_situation_now, financial_situation_5years))
# if change of financial situation or the current financial situation is 6 and above, means it's working(?)
df <- df %>% mutate(fin_situ_work = case_when((df$fin_situ_now >= 6 | df$fin_situ_change >= 6) ~ 'Yes',
                                                (df$fin_situ_now < 6 & df$fin_situ_change < 6) ~ 'No'))

reg_fin_situ_work = lm(working ~ fin_situ_work, data = df)
summary(reg_fin_situ_work)


# filter out rows with NAs in the two columns that has statistical significance (ext_supp, out_acti)
df_glm2 <- df %>% filter(!is.na(ext_supp)) %>% filter(!is.na(out_acti))

(colSums(is.na(df_glm2))*100)/dim(df_glm2)[1]

# doing logistic regression on the two columns that has statistical significance (ext_supp, out_acti)
# still all false
trainRowNumbers2 <- createDataPartition(df_glm2$working, p=0.8, list=FALSE)
trainData2 <- df_glm2[trainRowNumbers2,]
testData2 <- df_glm2[-trainRowNumbers2,]
trControl2 <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

model_glm2 <- train(as.factor(working) ~ out_acti + ext_supp, data=trainData2, method='glm', trControl = trControl2)
predicted_glm2 <- predict(model_glm2, testData2, type = 'prob')
model_glm2

# setting probability threshold for logistic regression
predict_false = predicted_glm2[,1]
predict_true = predicted_glm2[,2]
predictedData <- testData2 %>% cbind(predict_false) %>% cbind(predict_true)
names(predictedData)

predictedData <- predictedData %>% mutate(prediction_glm = case_when(as.numeric(predict_false) > 0.73 ~ 'FALSE',
                                                                     as.numeric(predict_false) < 0.73 ~ 'TRUE'))

caret::confusionMatrix(as.factor(predictedData$prediction_glm), as.factor(testData2$working))



logmodel <- glm(as.factor(working) ~ as.factor(predictedData$prediction_glm), data = testData2, family = binomial)
cutoffs <- seq(0.1,0.9,0.1)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(logmodel$fitted.values >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(testData2$working ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")




# Approach 3
# comparing NAs to non-NAs











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






# Conor's imputation code
preProcess_missingdata_model <- preProcess(df_ext_money, method='knnImpute') 
preProcess_missingdata_model 
heart_2 <- predict(preProcess_missingdata_model, newdata = df_ext_money)
heart_2



# confusion matrix code
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





