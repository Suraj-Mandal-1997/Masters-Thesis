library(plyr)
library(tidyverse)
library(ROSE) # package required for rose algorithm
library(pROC) # package required for plotting the roc curve
library(e1071) # package required for Naive Bayes
library(caret) # package required for Confusion Matrix
library(rpart) # package required for Decision Tree
library(rpart.plot) # package required to get nice plots for Decision Tree
library(randomForest) # package required for Random Forest

# ROSE algorithm for unbalanced dataset
str(data)
datas <- ROSE(Casualty_Severity ~ ., data = data,seed=11276)$data
table(data$Casualty_Severity)
table(datas$Casualty_Severity)

par(mfrow = c(1,2))

plot(data$Casualty_Severity, xlab = 'Casualty Severity', 
     col = c('blue','red'), ylim = c(0,10000),
     main = 'Imbalanced target variable')

plot(datas$Casualty_Severity, xlab = 'Casualty Severity', 
     col = c('red','green'), ylim = c(0,10000),
     main = 'Balanced target variable (ROSE)')

# Data modelling

dt <- datas 
set.seed(11276)	# for the consistency

#splitting the dataset in 70:30 ratio
nrows <- nrow(dt)
index <- sample(1:nrows, 0.7 * nrows)	# shuffle and divide
train <- dt[index,]			        
test  <- dt[-index,]  
str(train)
dim(train)
dim(test)

# (1) Naive Bayes: 

# fitting the model
naiveb_train <- naiveBayes(Casualty_Severity~.,data=train)

# predicting test set with the trained model
predicted_nb_train <- predict(naiveb_train, train[,!names(train) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_nb_train <- confusionMatrix(predicted_nb_train, train$Casualty_Severity)		
con_matrix_nb_train


# (2) Logistic Regression:

# fitting the model
lgr_train <- glm(Casualty_Severity ~.,train, family="binomial")
summary(lgr_train)

# making prediction on the training set
res_train <- predict(lgr_train, type="response")  # prediction
summary(res_train)
tapply(res_train, train$Casualty_Severity, mean)
con_matrix_lgr_train<-table(train$Casualty_Severity, res_train > 0.5)
con_matrix_lgr_train 

predictedvalues_train <- rep(0,7893)
predictedvalues_train[res_train>0.5] <- 1

trainlgr <- train
trainlgr$Casualty_Severity <- revalue(trainlgr$Casualty_Severity, 
                                      c('High'='1','Low'='0'))
mean(predictedvalues_train == trainlgr[,15]) # correctness of prediction


# (3) Random Forest:

# fitting the model
ran_forest_train <- randomForest(Casualty_Severity~., data=train)

# identifying the importance of variables based on Mean Decrease
imp_train <- importance(ran_forest_train)

# checking the importance of variables
par(mfrow=c(1,1))
varImpPlot(ran_forest_train,pch=18,col='red',cex=1)

# performance of random forest model
plot(ran_forest_train, main="Random Forest: MSE error vs. no of trees")

# predicting test set with the trained model
predicted_rf_train  <- predict(ran_forest_train, train[,!names(train) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_rf_train <- confusionMatrix(predicted_rf_train, train$Casualty_Severity)
con_matrix_rf_train


# (4) Decision tree:

# fitting the model
detree_train <- rpart(Casualty_Severity~.,train,method='class')
summary(detree_train)
rpart.plot(detree_train)
predicted_dt_train <- predict(detree_train,train,type = 'class')
con_matrix_dt_train <- confusionMatrix(predicted_dt_train,train$Casualty_Severity) 
con_matrix_dt_train

cfdt_train <- table(predicted_dt_train,train$Casualty_Severity)
cfdt_train

# comparison of all the models based on accuracy

col <- c("#6adc04","#ff6b04")
par(mfrow=c(2,2))

fourfoldplot(con_matrix_nb_train$table, color = col, conf.level = 0, 
             margin = 1, main=paste("NaiveBayes (",round(con_matrix_nb_train$overall[1]*100),"%)",sep=""))

fourfoldplot(con_matrix_lgr_train, color = col, conf.level = 0, 
             margin = 1, main=paste("LGR (",round(mean(predictedvalues_train == trainlgr[,15])*100),"%)",sep=""))

fourfoldplot(con_matrix_rf_train$table, color = col, conf.level = 0, 
             margin = 1, main=paste("RandomForest (",round(con_matrix_rf_train$overall[1]*100),"%)",sep=""))

fourfoldplot(cfdt_train, color = col, conf.level = 0, 
             margin = 1, main=paste("Decision Tree (",round(con_matrix_dt_train$overall[1]*100),"%)",sep=""))

# comparison of all the models based on roc curve

par(mfrow=c(2,2))

# (1) Naive Bayes:

predicted_nb1 <- predict(naiveb_train, train, type = 'raw')

auc1 <- auc(train$Casualty_Severity,predicted_nb1[,2])
plot(roc(train$Casualty_Severity,predicted_nb1[,2]),
     main=paste('Naive Bayes (AUC =', round(auc1, digits = 3),')'), sep = '')

# (2) Logistic Regression:

predicted_lgr1 <- predict(lgr_train, train, type = 'response')
auc2 <- auc(train$Casualty_Severity,predicted_lgr1)
plot(roc(train$Casualty_Severity,predicted_lgr1),
     main=paste('LGR (AUC =', round(auc2, digits = 3),')'), sep = '')

# (3) Random Forest:

predicted_rf1 <- predict(ran_forest_train, train, type = 'prob')

auc3 <- auc(train$Casualty_Severity,predicted_rf1[,2])
plot(roc(train$Casualty_Severity,predicted_rf1[,2]),
     main=paste('Random Forest (AUC =', round(auc3, digits = 3),')'), sep = '')

# (4) Decision Tree:

predicted_dt1 <- predict(detree_train,train,type = 'prob')

auc4 <- auc(train$Casualty_Severity,predicted_dt1[,2])
plot(roc(train$Casualty_Severity,predicted_dt1[,2]),
     main=paste('Decision Tree (AUC =', round(auc4, digits = 3),')'), sep = '')


#############################################################

# removing the insignificant variables

str(dt)
train1 <- train[,c(-7,-8,-10,-11,-12,-13)] 
test1 <- test[,c(-7,-8,-10,-11,-12,-13)]
str(train1)

# (1) Naive Bayes
# fitting the model
naiveb_train1 <- naiveBayes(Casualty_Severity~.,data=train1)

# predicting test set with the trained model
predicted_nb_train1<- predict(naiveb_train1, train1[,!names(train1) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_nb_train1 <- confusionMatrix(predicted_nb_train1, train1$Casualty_Severity)		
con_matrix_nb_train1


# (2) Logistic Regression:

# fitting the model
lgr_train1 <- glm(Casualty_Severity ~.,train1, family="binomial")
summary(lgr_train1)

#making prediction on the training set
res_train1 <- predict(lgr_train1, type="response")  # prediction
summary(res_train1)
tapply(res_train1, train1$Casualty_Severity, mean)
con_matrix_lgr_train1<-table(train1$Casualty_Severity, res_train1 > 0.5)
con_matrix_lgr_train1

predictedvalues_train1 <- rep(0,7893)
predictedvalues_train1[res_train1>0.5] <- 1

trainlgr1 <- train1
trainlgr1$Casualty_Severity <- revalue(trainlgr1$Casualty_Severity, 
                                      c('High'='1','Low'='0'))
mean(predictedvalues_train1 == trainlgr1[,9]) # correctness of prediction

# (3) Random Forest:

# fitting the model
ran_forest_train1 <- randomForest(Casualty_Severity~., data=train1)

# identifying the importance of variables based on Mean Decrease
imp_train1 <- importance(ran_forest_train1)

# checking the importance of variables
par(mfrow=c(1,1))
varImpPlot(ran_forest_train1,pch=18,col='red',cex=1)

# performance of random forest model
plot(ran_forest_train1, main="Random Forest: MSE error vs. no of trees")

# predicting test set with the trained model
predicted_rf_train1  <- predict(ran_forest_train1, train1[,!names(train1) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_rf_train1 <- confusionMatrix(predicted_rf_train1, train1$Casualty_Severity)
con_matrix_rf_train1


# (4) Decision Tree:

# fitting the model
detree_train1 <- rpart(Casualty_Severity~.,train1,method='class')
summary(detree_train1)
rpart.plot(detree_train1)
predicted_dt_train1 <- predict(detree_train1,train1,type = 'class')
con_matrix_dt_train1 <- confusionMatrix(predicted_dt_train1,train1$Casualty_Severity) 
con_matrix_dt_train1

cfdt_train1 <- table(predicted_dt_train1,train1$Casualty_Severity)
cfdt_train1

