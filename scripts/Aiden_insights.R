# Aiden
# Created: 07-28-2019
# Last Updated: 07-28-2019
options(scipen=999)
library(tidyverse)
library(dplyr)
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
df <- df %>% mutate(ext_supp = case_when((df$anygrant == 'TRUE' | df$anyhhincome == 'TRUE') ~ 'TRUE',
                                         (df$anygrant == 'FALSE' | df$anyhhincome == 'FALSE' ~ 'FALSE')))
df <- df %>% 
  mutate(haschildren = as.numeric(as.character(df$numchildren)) > 0)
df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)
df <- df %>% 
  mutate(numincome = parse_number(as.character(numearnincome))) %>% 
  mutate(hasincome = numincome > 0)
df <- df %>% mutate(out_acti = case_when((df$volunteer == 'Yes' | df$leadershiprole == 'Yes') ~ 'TRUE',
                                         (df$volunteer == 'No' | df$leadershiprole == 'No' ~ 'FALSE')))

                  # columns that are combined/edited #
#########################################################################################################
# 1st: anygrant & anyhhincome
df_ext_supp <- df %>% filter(!is.na(ext_supp))
ggplot(data = df_ext_supp) +
  geom_bar(mapping = aes(x = anygrant, fill = working))
reg_grant = lm(working ~ anygrant, data = df)
summary(reg_grant)
ggplot(data = df_ext_supp) +
  geom_bar(mapping = aes(x = anyhhincome, fill = working))
reg_hhincome = lm(working ~ anyhhincome, data = df)
summary(reg_hhincome)
# additional 6.5% chance of working if one has external support
ggplot(data = df_ext_supp) +
  geom_bar(mapping = aes(x = ext_supp, fill = working))
reg_ext_supp = lm(working ~ ext_supp, data = df)
summary(reg_ext_supp)
#df_ext_supp <- df_ext_supp %>% filter(!is.na(gender))
#ggplot(data = df_ext_supp, aes(x = age)) +
#  geom_bar(aes(fill = working), position = 'fill') + facet_grid(~gender) +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
#  ylab('probability of working/not working')
#########################################################################################################
# 2nd: volunteer & leadershiprole
df_out_acti <- df %>% filter(!is.na(out_acti))
reg_volunteer = lm(working ~ volunteer, data = df_out_acti)
summary(reg_volunteer)
reg_leadershiprole = lm(working ~ leadershiprole, data = df_out_acti)
summary(reg_leadershiprole)
# additional 2% chance of working if one has volunteer and/or
# leadership experience
reg_out_acti = lm(working ~ out_acti, data = df_out_acti)
summary(reg_out_acti)
#########################################################################################################
# 3rd: numchildren
  # data shows that the higher the number of children, the less likely
  # the person is likely to work --> take care of kids
reg_numchildren = lm(working ~ numchildren, data = df)
summary(reg_numchildren)
  # feature engineering: add column of 'haschildren'
  # no significant statistical result, basically only confirming that
  # having kids decreases the prob of working
reg_haschildren = lm(working ~ haschildren, data = df)
summary(reg_haschildren)
  # insights on province vs working
  # North West and Western Cape stand out as the only two regions
  # that has more than 25% probability a person from there is working
  # https://www.iol.co.za/weekend-argus/western-cape-is-leading-creator-of-employment-21632718
  # helping people to get employed --> higher chance at Western Cape / North West
ggplot(data = df, aes(x = province)) +
  geom_bar(aes(fill = working), position = 'fill') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylab('probability of working/not working')
# insights on scores vs working
ggplot(data = df) +
  geom_bar(mapping = aes(x = c(cft_score, com_score, grit_score, num_score, opt_score), fill = working))
df_fin <- df %>% 
  group_by(fin_situ_change) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ggplot(df, aes(x = fin_situ_change, y = (df_fin$perc)*100, fill = factor(working))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "fin_situ_change", y = "percent", fill = "working") +
  theme_minimal(base_size = 14)
reg_numincome = lm(working ~ numincome, data = df)
summary(reg_numincome)
# insights on age vs working
ggplot(data = df) + 
  geom_bar(mapping = aes(x = age, fill = working))
ggplot(df, aes(x=cft_score,  group=gender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 5) +
  labs(y = "Percent", fill="working") +
  facet_grid(~gender) +
  scale_y_continuous(labels = scales::percent)
# shows that for both females and males, it is more likely
# that they are employed when they are older
ggplot(data = df) + 
  geom_point(mapping = aes(x = gender, y = age, color = working))
#########################################################################################################
# 4th: financial situation change  
ggplot(data = df) + 
  geom_bar(mapping = aes(x = fin_situ_change, fill = working))
#########################################################################################################
# 5th: cft score
ggplot(data = df) + 
  geom_bar(mapping = aes(x = cft_score, fill = working))
ggplot(data = df, aes(x = cft_score)) +  
  geom_bar(aes(y = working/sum(working)), stat = 'identity')
ggplot(data = df) +
  geom_bar(mapping = aes(x = hasincome, fill = working))
#########################################################################################################333
