library(dplyr)
library(tidyverse)
library(rstanarm)
library(readr)
library(ggplot2)
library(rstanarm)
library(lme4)
library(Matrix)

train <- read_csv("~/Desktop/aug_train.csv")

train1<-na.omit(train)
train1$target<-as.factor(train1$target)


ggplot(train1,aes(gender))+
  geom_histogram(aes(fill=target),stat = 'count')

ggplot(train1,aes(education_level))+
  geom_bar(aes(colour=target))

ggplot(train1, aes(x=major_discipline, y=training_hours)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)


ggplot(train1)+geom_density(aes(x=training_hours, color=target))


ggplot(train1, aes(x=city_development_index, y=target)) + 
  geom_point()+
  geom_smooth(method=lm)


fit<-stan_glm(target~gender+city_development_index+training_hours+education_level
              +major_discipline+experience,family=binomial(link = 'logit'),
              data=train1)
summary(fit)
plot(fit)

fit2 <- glmer(target~gender+city_development_index+training_hours+education_level
              +major_discipline+experience+(1|city),
              family=binomial(link = 'logit'),data=train1)
summary(fit2)



