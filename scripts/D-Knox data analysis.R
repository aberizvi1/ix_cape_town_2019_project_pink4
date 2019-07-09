library(tidyverse)
library(pastecs)
library(lubridate)

options(scipen=999)

df <- read.csv("data/raw/teaching_training_data.csv")

df <- df %>% 
  mutate(fin_situ_now = parse_number(as.character(financial_situation_now))) %>% 
  mutate(fin_situ_future = parse_number(as.character(financial_situation_5years))) %>% 
  mutate(fin_situ_change = fin_situ_future - fin_situ_now)


df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

colSums(is.na(df))

library ( VIM )
aggr_plot <- aggr(df,col=c("navyblue","red"),numbers = TRUE , sortVars = TRUE ,
                  labels=names(df),cex.axis=.7,gap =3 ,
                  ylab =c( " Histogram of missing data " ," Pattern " ))

for (i in 2:28){
  dfsort <- df[order(df[,i]),] 
  par(mar=c(4 ,8 ,0.1 ,0.1))
  image(!is.na(dfsort),axes=FALSE,col=gray(0:1))
  title(xlab=paste("Inmates sorted by",names(dfsort)[i]))
  axis(2,at=(0:27)/27,labels=names(dfsort),las =2)
  axis(1,at=100*(0:832)/840,100*(0:832))
}

df <- df[df$survey_num == 1,]

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="plots")

preProcess_missingdata_model <- preProcess(heart_mv, method='knnImpute') 
preProcess_missingdata_model 
heart_2 <- predict(preProcess_missingdata_model, newdata = heart_mv)

