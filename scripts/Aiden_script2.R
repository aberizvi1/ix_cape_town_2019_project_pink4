# Aiden
# Created: 07-28-2019
# Last Updated: 07-28-2019

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




df <- subset(df, select = -c(volunteer, leadershiprole, anygrant, anyhhincome, financial_situation_now, financial_situation_5years))

df <- df %>% filter(!is.na(ext_supp)) %>% filter(!is.na(out_acti))

(colSums(is.na(df))*100)/dim(df)[1]

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

# filter out rest of the rows with NAs
df <- df %>% filter(!is.na(peoplelive)) %>% filter(!is.na(opt_score))

model_glm <- glm(as.factor(working) ~ ., data=df, family = binomial("logit"))
model_glm = step(model_glm)
summary(model_glm)

df_pred1 <- as.data.frame(predict(model_glm, data = df, type = 'response')) %>% 
  rename(pred1 = 'predict(model_glm, data = df, type = \"response\")')

df_pred1 <- bind_cols(df, df_pred1)
df_pred1 <- df_pred1 %>% mutate(binary_pred = case_when(pred1 >= 0.34 ~ TRUE,
                                                        pred1 < 0.34 ~ FALSE))

confusionMatrix(as.factor(df_pred1$binary_pred), as.factor(df_pred1$working))




# out_acti, age, cft_score, givemoney_yes, gender
modified_glm <- glm(as.factor(working) ~ out_acti*age*cft_score*givemoney_yes*gender, data=df, family = binomial("logit"))
modified_glm = step(modified_glm)
summary(modified_glm)

df_pred1 <- as.data.frame(predict(modified_glm, data = df, type = 'response')) %>% 
  rename(pred1 = 'predict(modified_glm, data = df, type = \"response\")')

df_pred1 <- bind_cols(df, df_pred1)
df_pred1 <- df_pred1 %>% mutate(binary_pred = case_when(pred1 >= 0.31 ~ TRUE,
                                                        pred1 < 0.31 ~ FALSE))

confusionMatrix(as.factor(df_pred1$binary_pred), as.factor(df_pred1$working))

quantile(df_pred1$pred1, na.rm = TRUE)










df_numeric <- sapply(df, as.numeric)
install.packages("corrplot")
library(corrplot)
corrplot(cor(df_numeric))
df_numeric.cor = cor(df_numeric)
df_numeric.cor
