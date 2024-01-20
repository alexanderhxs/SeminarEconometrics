#install.packages('vars')
library(vars)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tseries)
library(lubridate)

#set environment
setwd("C:/Users/alexa/OneDrive/Dokumente/Studium/7.Semester/Seminar/R")

#get data
raw_data = read.csv('C:/Users/alexa/OneDrive/Dokumente/Studium/7.Semester/Seminar/Daten/49_Industry_Portfolios_Daily.csv',
                     skip = 9, header = TRUE)

#important dates
#shocks = list(c('Black Monday','1987-10-01','1987-11-01'),c('Dotcom','2000-01-01','2002-01-01'),
#               c('Financial Crisis','2008-08-15','2008-08-15'),c('Corona','2020-03-01','2021-12-31'))
shocks = list(c('Financial Crisis (15. September)','2008-09-15','2008-09-15'),
              c('Ukraine Invasion','2022-02-24','2022-04-07'),
              c('Corona Crash','2020-02-20','2020-04-07'),
              c('Dotcom Bubble','2000-03-01','2000-03-31'))

#prepare data
raw_data = raw_data %>% filter(X >= 19690701) %>% select(X,Fin,Oil,Drugs,Food,Guns)
raw_data = raw_data %>% mutate_all(as.numeric)

#look at NAs
X_na = which(is.na(raw_data$X))
print(X_na)
tail(raw_data)

data = na.omit(raw_data)
data = data[1:(nrow(data)/2),]
str(data)

#compute summary statistics
summaries = summary(data %>% select(-X))
print(summaries)

#compute correlation
cor(data[,-1])

#test stationary
adf.test(data[,2])
kpss.test(data[,2])
#->stationary

adf.test(data[,3])
kpss.test(data[,3])
#->stationary

adf.test(data[,4])
kpss.test(data[,4])
#->stationary

adf.test(data[,5])
kpss.test(data[,5])
#->stationary

adf.test(data[,6])
kpss.test(data[,6])
#->stationary

data = data %>% mutate(X = ymd(X))

#plot data 
plot_ts(data)

#weekly data
data_weekly = data %>% group_by(Week = floor_date(X,'week')) %>% 
  summarize_at(vars(Fin,Oil,Drugs,Food,Guns),sum) 
data_weekly = as.data.frame(data_weekly)
plot_ts(data_weekly)

#monthly data
data_monthly = data %>% group_by(Month = floor_date(X,'month')) %>% summarize_at(vars(Fin,Oil,Drugs,Food,Guns),sum)
data_monthly = as.data.frame(data_monthly)
plot_ts(data_monthly)

#bi-weekly data
data_biweekly = data %>% mutate(biweek = cut(X, breaks = '2 weeks', labels = FALSE)) %>%
  group_by(biweek) %>% summarize_at(vars(Fin,Oil,Drugs,Food,Guns),sum) 
data_biweekly = as.data.frame(data_biweekly)
plot_ts(data_biweekly)

#scale data
data_scaled = data %>% mutate_at(vars(c(Fin,Oil,Drugs,Food,Guns)), scale)
summary(data_scaled[,-1])

#plot data
plot_ts(data)
plot_ts(data_monthly)

#compute summary statistics
summary(data %>% select(-X))
summary(data_monthly[,-1])
