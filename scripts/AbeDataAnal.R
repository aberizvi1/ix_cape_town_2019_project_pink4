#Can we build a model to identify people who will get a job?

library(tidyverse)
library(pastecs)

# turn off scientific notation
options(scipen=999)
df <- read.csv("data/raw/teaching_training_data.csv")


# some wrangling (discuss how to structure project workflow)
# use readr::parse_number
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)
# visualise
ggplot(data = df) + 
  geom_bar(mapping = aes(x = fin_situ_now))
ggplot(data = df) + 
  geom_point(mapping = aes(x = fin_situ_now, y = fin_situ_change))
# hmm doesn't look right
ggplot(data = df) + 
  geom_jitter(mapping = aes(x = fin_situ_now, y = fin_situ_change))
# can also 'facet'
ggplot(data = df) + 
  geom_bar(mapping = aes(x = fin_situ_now)) + 
  facet_wrap(~fin_situ_future)
# think about the 'contraints' you have placed on the variable through its construction
# Now split into testing and training

# create training data
set.seed(1234)
df_train_index <- df %>%
  select() %>% 
  distinct() %>% 
  sample_frac(0.7)
# opportunity to discuss joins
# http://stat545.com/bit001_dplyr-cheatsheet.html
df_train <- left_join(df_train_index, df)
df_test <- anti_join(df, df_train_index)

# Modelling time
# run a regression
reg1 <- lm(working ~ gender, data = df_train)
summary(reg1)
#reg2 <- lm(working ~ gender + fin_situ_now + anyhhincome, data = df_train)
reg2 <- lm(working ~ gender + givemoney_yes + province, data = df_train)
summary(reg2)
#reg3 <- lm(working ~ gender + as.factor(fin_situ_now) + anyhhincome, data = df_train)
reg3 <- lm(working ~ gender + monthly_pay + as.factor(fin_situ_change), data = df_train)
summary(reg3)
# discussion of what this means
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
# pick say 30%
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
# ggplot?
ggplot(confusion_matrix) +
  geom_bar(mapping = aes(x = working, y = nobs, fill = binary_pred3), stat = 'identity')
# proportions
confusion_matrix <- confusion_matrix %>% 
  mutate(proportion_pworking = nobs/total_working) %>% 
  mutate(proportion_total = nobs/total_obs)
# Is this model good or bad?
# Why?
#Predict-- highest true positive rates >.068