library(caret)
library(pROC)
library(klaR)
library(dplyr)
library(randomForest)
library(e1071)
library("ROCR")
data <- read.csv2(file="C:\\Users\\VamshidharReddy\\Desktop\\ALDA 522\\Project\\Dataset\\bank-additional\\bank-additional\\bank-additional-full.csv")
summary(data)
DF=data.frame(data)
DF$C = paste(DF$housing, DF$y, sep="")
summary(DF$C)
table(DF$C)
unknownhousing=c(107,883)
knownhousing=c(4533,35665)
housing.survey = as.data.frame(rbind(unknownhousing,knownhousing))
names(housing.survey) = c('yes', 'no')
DF=data.frame(data)
DF$C = paste(DF$education, DF$y, sep="")
summary(DF$C)
table(DF$C)
unknowneducation=c(251,1480)
knowneducation=c(4389,35068)
education.survey = as.data.frame(rbind(unknowneducation,knowneducation))
names(education.survey) = c('yes', 'no')
DF=data.frame(data)
DF$C = paste(DF$marital, DF$y, sep="")
summary(DF$C)
table(DF$C)
unknownmarital=c(12,68)
knownmarital=c(4628,36480)
marital.survey = as.data.frame(rbind(unknownmarital,knownmarital))
names(marital.survey) = c('yes', 'no')
DF=data.frame(data)
DF$C = paste(DF$job, DF$y, sep="")
summary(DF$C)
table(DF$C)
unknownjob=c(37,293)
knownjob=c(4603,36255)
job.survey = as.data.frame(rbind(unknownjob,knownjob))
names(job.survey) = c('yes', 'no')
chisq.test(marital.survey)
chisq.test(job.survey)
chisq.test(housing.survey)
chisq.test(education.survey)
#box plots for checking outliers for continuous variables
boxplot(data$age~data$y, main=" AGE",ylab="age of customers",xlab="Subscribed")
boxplot(data$duration~data$y, main="LAST DURATION",ylab="Last duration of contact",xlab="Subscribed")
boxplot(data$campaign~data$y, main="NUM CONTACTS",ylab="number of contacts",xlab="Subscribed")
boxplot(data$pdays~data$y, main=" Previous DAYS",ylab="Previous days of contact",xlab="Subscribed")
boxplot(data$previous~data$y, main=" Previous Contacts",ylab="Previous Contacts with customers",xlab="Subscribed")
#duration attribute should be remoeved because we don't know its value prior to call
data$duration <- NULL 
#Logistic Regression for housing
sp <- split(data, data$housing)
train <- rbind(data.frame(sp$no), rbind(data.frame(sp$yes))) 
test <- data.frame(sp$unknown)
model <- glm( housing ~ job	+ loan	+ month	+ day_of_week	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed, data=data, family = binomial)
summary(model)
pred <- predict(model, newdata = train, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
train$pred <- pred
train$pred <- NULL
pred <- predict(model, newdata = test, type = "response")
pred <- ifelse(pred >= .5, "yes", "no")
test$housing <- pred
data <- rbind(train, test) 
data <- droplevels(data)
#LDA for education
sp <- split(data, data$education)
train <- rbind(data.frame(sp$basic.4y), data.frame(sp$basic.6y), data.frame(sp$basic.9y), data.frame(sp$high.school), data.frame(sp$illiterate), data.frame(sp$professional.course), data.frame(sp$university.degree)) 
test <- data.frame(sp$unknown)
train <- droplevels(train)
ldaModel<- lda(education ~ age + job + marital  + default	+ housing	+ loan	+ contact	+ month	+ day_of_week	+ campaign	+ pdays	+ previous	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ nr.employed + y, data = train)
ldaModel
pred <- predict(ldaModel, train)
train$pred <- pred$class
train$pred <- NULL
matrix <- confusionMatrix(train$education, pred$class)
matrix$overall[1]
pred <- predict(ldaModel, test)
test$education <- pred$class
data <- rbind(train, test) 
data <- droplevels(data)
#LDA for job
sp <- split(data, data$job)
train <- rbind(data.frame(sp$admin.), data.frame(sp$'blue-collar'), data.frame(sp$entrepreneur), data.frame(sp$housemaid), data.frame(sp$management), data.frame(sp$retired), data.frame(sp$'self-employed'), data.frame(sp$services), data.frame(sp$student), data.frame(sp$technician), data.frame(sp$unemployed)) 
test <- data.frame(sp$unknown)
train <- droplevels(train)
ldaModel<- lda(job ~ age + education + marital  + default  + housing	+ loan	+ contact	+ month	+ day_of_week	+ campaign	+ pdays	+ previous	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed + y, data = train)
ldaModel
pred <- predict(ldaModel, train)
train$pred <- pred$class
train$pred <- NULL
matrix <- confusionMatrix(train$job, pred$class)
matrix$overall[1]
pred <- predict(ldaModel, test)
test$job <- pred$class
data <- rbind(train, test) 
data <- droplevels(data)
#LDA for marital status
sp <- split(data, data$marital)
train <- rbind(data.frame(sp$divorced), data.frame(sp$married), data.frame(sp$single)) 
test <- data.frame(sp$unknown)
train <- droplevels(train)
ldaModel<- lda(marital ~ age + education + job  + default  + housing  + loan	+ contact	+ month	+ day_of_week	+ campaign	+ pdays	+ previous	+ poutcome	+ emp.var.rate	+ cons.price.idx	+ cons.conf.idx	+ euribor3m	+ nr.employed + y, data = train)
ldaModel
pred <- predict(ldaModel, train)
train$pred <- pred$class
train$pred <- NULL
matrix <- confusionMatrix(train$marital, pred$class)
matrix$overall[1]
pred <- predict(ldaModel, test)
test$marital <- pred$class
data <- rbind(train, test) 
data <- droplevels(data)
#Binarizing pdays as 96% values are previously not contacted
data$pdays <- ifelse(data$pdays== 999, 0, 1)
write.csv(data,file = "MyData.csv",row.names=FALSE)




