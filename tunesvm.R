library(e1071)
library(caret)

## Read data
data <- read.csv(file="C:\\Users\\VamshidharReddy\\Documents\\Mydata.csv")
## Remove column x
data$X<-NULL
## Sample and divide into training and testing data
index <- sample(2, 
                     nrow(data),
                     replace = T,
                     prob = c(0.7,0.3))

training <- data[index==1,]
testing <- data[index==2,]
varNames <- names(training)

tuned1 <- tune.svm(y~.,data=training,kernel="polynomial", cost=10^(-2:1),gamma=10^(-2:1),cross=10)
summary(tuned1)
tuned <- tune.svm(y~.,data=training, kernel="radial", cost=10^(-2:5),gamma=10^(-2:1),cross=10)
summary(tuned)
tuned0 <- tune.svm(y~.,data=training,kernel="sigmoid",coef0= -1:1, gamma=10^(-2:-1), 
                   cost=10^(-2:5),cross=10)
summary(tuned0)
