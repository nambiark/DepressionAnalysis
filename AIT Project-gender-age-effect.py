# -*- coding: utf-8 -*-
"""AITProjectagegendereffect.ipynb


Original file is located at
    https://colab.research.google.com/drive/1NO5-fF0-aGChxSDcwfkr_J7SMSbszCPI
"""

##Installing required libraries
install.packages('readr')
install.packages('plyr')

library('plyr')

##REading input files
install.packages('dplyr')
library('readr')
library('dplyr')
temp <- read.csv('/content/total_population.csv')

#total_population <- temp %>% select('location_name','year','val')

head(total_population)

library("readxl")

mentalSub = read_excel("/content/MentalhealthData.xlsx", sheet="prevalence-by-mental-and-substa")

depressionEducation = read_excel("/content/MentalhealthData.xlsx", sheet="depression-by-level-of-educatio")

depressionAge = read_excel("/content/MentalhealthData.xlsx", sheet="prevalence-of-depression-by-age")

depressionMales = read_excel("/content/MentalhealthData.xlsx", sheet="prevalence-of-depression-males-")

suicideMental = read_excel("/content/MentalhealthData.xlsx", sheet="suicide-rates-vs-prevalence-of-")

depressionCount = read_excel("/content/MentalhealthData.xlsx", sheet="number-with-depression-by-count")

head(depressionAge)



#total_data = read.csv('/content/IHME-GBD_2019_DATA-b37e1709-1.csv')

##REading input files
mydir <- "/content/data/"
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
total_data <- ldply(myfiles, read_csv)

# head(total_data)
myfiles

#Filtering data based on metric to get number of incidences

data <- total_data %>% filter(metric=='Number' & measure=='Incidence') %>% dplyr::select('location','sex','age','year','val')
head(data)

##Pivoting data to make separate columns for Male and female
library(tidyr)
final_data <- spread(data, key=sex, value=val)
head(final_data)

# # #detach('plyr')
# # detach(plyr)
# pkg <- "package:plyr"
# detach(pkg, character.only = TRUE)
##getting total incidences per age group for male and female
temp<-final_data %>% group_by(age) %>% summarise(female=sum(Female),male=sum(Male))
head(temp)

install.packages('reshape2')
library(ggplot2)
library(reshape2)
options(repr.plot.width=15, repr.plot.height=8)

##Convertig columns to rows for plotting
temp1<-melt(temp,id.vars = 'age')
head(temp1)

##Plotting Depression incidences for male and female per age group
p<- ggplot(temp1,aes(x=age,fill=variable,y=value))+geom_bar(stat = 'identity',position='dodge')+
labs(x='Incidence',y='Age group', title = 'Depression incidence across age group for both gender') +
theme(plot.title = element_text(hjust = 0.5))

p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))#+coord_flip()
p

data$age <- as.factor(data$age)
data$sex <- as.factor(data$sex)

str(data)

##################  Data Exploratory Analysis ################
 ###Finding correlation between depression value and other features
 install.packages('polycor')
 library('polycor')
 hetcor(data)

##Checking the data for skewness using histogram and skewness value
install.packages('moments')
hist(data$val,col='red')
library(moments)

cat("Skewness:",skewness(data$val))

##Positive skewness indicates right skewed

##Taking log of value to reduce skewness and to distribute normally
l <- log(data$val)
summary(l)

hist(l,col='red')

cat("Skewness:", skewness(l))

##The distribution now is normally distributed

data$log.val <- l

ggplot(data = data, aes(x=log.val,y=sex, fill=sex)) + 
  geom_boxplot(alpha = 0.7,
               outlier.colour='blue', 
               outlier.shape=19, 
               outlier.size=3, 
               width = 0.6, color = "#1F3552", fill = "#4271AE"
               )+
  theme_grey() +
  labs(title = 'Do female have higher rate of depression than male?',
       y='% Difference from FEMALES',x='',
       caption  = 'Positive % Difference means Males performed \n better than Females and vice versa',
       subtitle = 'Based on PISA Score 2015')

###Linear regression analysis 
lm.all <- lm(val~.,data=data)
summary(lm.all)

##By taking log of val the accuracy improved from 57.71% to 96.81%
lm.a <- lm(log.val~.,data=data)
summary(lm.a)

###Plot the model to understand normality, homoscedasticity, equal variances and independent errors
par(mfrow = c(2,2))
plot(lm.a)



#Divide the data in training and test sets
set.seed(1)
train = sample(1:nrow(data),0.7*nrow(data))
DepressionTest = data$log.val[-train]

##Fit the model
lm.fit = lm(log.val~.,data=data,subset = train)

#lm.pred = predict(lm.a, data=data,subset =, type="response")
#yhat=predict(rpart.boston.prune, newdata=Boston[-train,])
lm.pred=predict(lm.fit, newdata = data[-train,])

dep.test <- data[-train,"log.val"]
length(dep.test)
length(lm.pred)

cat('Mean Squared Value:',(MSE = mean((lm.pred-dep.test)^2)))
cat('\nMean Square Root Value:',sqrt(MSE))

# Plot test-set  value vs. predicted  value 
ggplot(data.frame(lm.pred, dep.test), aes(x=lm.pred ,y=dep.test)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="predicted depression", 
      y="test-set depression",
      title="regression")

##Linear relation between predicted and test data incicates that the model is good.

lm.gender.age <- lm(data = data, val ~ age + sex)

summary(lm.gender.age)

