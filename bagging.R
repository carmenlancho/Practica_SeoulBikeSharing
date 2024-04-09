#Lectura de datos
library(tidyverse)
library(caret)
library(pROC)
library(ipred)
library(rpart)
bank = read.csv('https://raw.githubusercontent.com/rafiag/DTI2020/main/data/bank.csv')
dim(bank)

bank$deposit <- as.factor(bank$deposit)

bank=as.tibble(bank)

# Particionamos los datos
set.seed(2138)
n=dim(bank)[1]
indices=seq(1:n)
indices.train=sample(indices,size=n*.5,replace=FALSE)
indices.test=sample(indices[-indices.train],size=n*.25,replace=FALSE)
indices.valid=indices[-c(indices.train,indices.test)]

bank.train=bank[indices.train,]
bank.test=bank[indices.test,]
bank.valid=bank[indices.valid,]

set.seed(128)
df <- bank.train %>%
  mutate(fmarital=as.factor(marital))%>%
  select(age,job,housing,marital=fmarital,education,duration,poutcome,balance,deposit)


# Bagging
bagging_model <- bagging(deposit ~., data=df,nbagg=100,
                         control = rpart.control(minsplit = 2, cp = 0,maxdepth = 20))
bagging_model

# sobre la partición de entrenamiento
prediction <- predict(bagging_model, df, type = 'class')
prediction_prob <- predict(bagging_model, df, type = 'prob')
cf <- confusionMatrix(prediction, as.factor(df$deposit),positive="yes")
print(cf)

