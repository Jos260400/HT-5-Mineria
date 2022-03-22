getwd()
setwd("D:/UVG/2022/Semestre 1 2022/Mineria de datos/HDT5")
df_test <- read.csv("test.csv")
df_train<- read.csv("train.csv")

library(ggplot2)
library (dplyr)
library(naivebayes)
library(psych)


library(caret)
library(e1071)
library(klaR)

##limites de var categorica tipo de casa barata/ mediana / cara
summary(df_train$SalePrice)
priceRange <- max(df_train$SalePrice)-min(df_train$SalePrice)
baratoMax<- min(df_train$SalePrice)+(priceRange/3)
medianoMax <- baratomax+(priceRange/3)
caroMax<-max(df_train$SalePrice)
max(df_train$SalePrice)
(medianoMax)
baratoMax
df_train['tipoDeCasa']<- ifelse(df_train$SalePrice<baratoMax,"BARATA",ifelse(df_train$SalePrice>=baratoMax & df_train$SalePrice<medianoMax,"MEDIA","CARA"))



modelo<-naiveBayes(df_train$tipoDeCasa~., data=train)


model <- naive_bayes(Launch ~ ., data = df_train, usekernel = T) 
model plot(model)