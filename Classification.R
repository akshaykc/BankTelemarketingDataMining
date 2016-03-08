library(caret)
library(pROC)
library(klaR)
library(dplyr)
library(randomForest)
library(e1071)
library("ROCR")
data <- read.csv(file="C:\\Users\\VamshidharReddy\\Documents\\Mydata.csv")
## Remove column x
data$X<-NULL
## Sample and divide into training and testing data
index <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[index==1,]
testing <- data[index==2,]
varNames <- names(training)
# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]
# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
# Add response variable and convert to a formula object
class_formula <- as.formula(paste("y", varNames1, sep = " ~ "))
#Creating a training control for 10 fold cross valiation
fitControl <- trainControl(method = 'cv', number = 10)
print(class_formula)
lab<-testing$y
#Random Forest Classification
forest_model <- randomForest(class_formula,
                             training,
                             ntree=500,
                             importance=T)
forest_mode<- train(class_formula, training,
                    method = 'rf',
                    trControl = fitControl,
                    n.tree = 500) 

#Naive Bayesian Classification
NB<-naiveBayes(class_formula, data = training, laplace = 100)
fit_nb <- train(class_formula, training,
                method ='nb',
                trControl = fitControl)
#Neural Network Classification
nnet_model<-train(class_formula, training, method = 'nnet', trControl=fitControl,tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1)))
#SVM Classification
train.svm<-svm(class_formula,training,probability=TRUE,type="C-classification",kernel="radial",cost=0.01,scale=TRUE,degree=3,gamma=1)
fit_svmRBF <- train(class_formula,
                    training,
                    method = 'svmRadial',
                    preProcess = 'range',
                    trControl = fitControl,
                    tuneGrid = expand.grid(.C = 0.01,
                                           .sigma = 1))
# Creating performance object -> NNET
library("ROCR")
roc_nnet_predict <- predict(nnet_model, testing,type="prob")[,2]
perf_nnet_obj <- prediction(predictions=roc_nnet_predict,labels=lab)
roc_nnet_obj <- performance(perf_nnet_obj, measure="tpr", x.measure="fpr")
roc_nnet_auc_obj <- performance(perf_nnet_obj, measure="auc")
plot(roc_nnet_obj,
     main="ROC Curves",
     col="blue")
abline(0,1,col="grey")
# Creating performance object -> RF

roc_rf_predict <- predict(forest_model ,testing,type="prob")[,2]
perf_rf_obj <- prediction(predictions=roc_rf_predict,labels=lab)
roc_rf_obj <- performance(perf_rf_obj, measure="tpr", x.measure="fpr")
roc_rf_auc_obj <- performance(perf_rf_obj, measure="auc")
plot(roc_rf_obj,add = TRUE,
     col="red")
# Creating performance object -> NB

roc_nb_predict <- predict(NB ,testing,type="raw")[,2]
perf_nb_obj <- prediction(predictions=roc_nb_predict,labels=lab)
roc_nb_obj <- performance(perf_nb_obj, measure="tpr", x.measure="fpr")

roc_nb_auc_obj <- performance(perf_nb_obj, measure="auc")
roc_nb_lift_obj <- performance(perf_nb_obj, measure="lift")
plot(roc_nb_obj,add = TRUE,
     col="yellow")
# Creating performance object -> SVM
predsvm <- predict(train.svm,testing,probability =TRUE,decision.values =TRUE )
predsvmprob<-attr(predsvm,"probabilities")
perf.objsvm <- prediction(predictions=predsvmprob[,1],labels=lab)
roc.objsvm <- performance(perf.objsvm, measure="tpr", x.measure="fpr")
aucsvm<-performance(perf.objsvm,measure="auc") 
plot(roc.objsvm,
     add=TRUE,
     col="green")
legend("bottomright", legend=c("Neural Network", "Random Forest","Naive Bayes","SVM"), col=c("blue","red","yellow","green"), lwd=2, cex=0.5)
#Lift curves
alift.objsvm <- performance(perf.objsvm,measure="lift", x.measure="rpp")
plot(alift.objsvm,
     main="Lift Curves",
     col="green")
alift.objnnet <- performance(perf_nnet_obj,measure="lift", x.measure="rpp")
plot(alift.objnnet,
     add=TRUE,
     col="blue")
alift.objrf <- performance(perf_rf_obj,measure="lift", x.measure="rpp")
plot(alift.objrf,
     add=TRUE,
     col="red")
alift.objnb <- performance(perf_nb_obj,measure="lift", x.measure="rpp")
plot(alift.objnb,
     add=TRUE,
     col="yellow")
legend("bottomright", legend=c("Neural Network", "Random Forest","Naive Bayes","SVM"), col=c("blue","red","yellow","green"), lwd=2, cex=0.5)

#printing AUC values
print("AUC Values")
print("Neural Network")
print(roc_nnet_auc_obj@y.values)
print("Random Forest")
print(roc_rf_auc_obj@y.values)
print("Naive Bayes")
print(roc_nb_auc_obj@y.values)
print("SVM")
print(aucsvm@y.values)
#plotting variable Importance
importance_nnet <- varImp(nnet_model, scale = TRUE)
plot(importance_nnet, main = 'Feature importance for NNet') 
importance_rf <- varImp(forest_mode, scale = TRUE)
plot(importance_rf, main = 'Feature importance for Random Forest')
importance_svmRBF <- varImp(fit_svmRBF, scale = TRUE)
plot(importance_svmRBF, main = 'Feature importance for SVM-RBF')
importance_nb <- varImp(fit_nb, scale = TRUE)
plot(importance_nb, main = 'Feature importance for Naive Bayes')
#confusion matrices
print("neural network")
# Predicting response variable
prednnet <- predict(nnet_model,testing)
# Create Confusion Matrix
confusionMatrix(data=prednnet,
                reference=testing$y,
                positive='yes')
print("Naive Bayes")
# Predicting response variable
predNB <- predict(NB,testing)
# Create Confusion Matrix
confusionMatrix(data=predNB,
                reference=testing$y,
                positive='yes')
print("Random Forest")
# Predicting response variable
predrf <- predict(forest_model,testing)
# Create Confusion Matrix
confusionMatrix(data=predrf,
                reference=testing$y,
                positive='yes')
print("SVM RBF")
# Create Confusion Matrix
confusionMatrix(data=predsvm,
                reference=testing$y,
                positive='yes')

