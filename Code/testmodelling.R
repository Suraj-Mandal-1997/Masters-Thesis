library(plyr)
library(tidyverse)
library(ROSE) # package required for rose algorithm
library(pROC) # package required for plotting the roc curve
library(e1071) # package required for Naive Bayes
library(caret) # package required for Confusion Matrix
library(rpart) # package required for Decision Tree
library(rpart.plot) # package required to get nice plots for Decision Tree
library(randomForest) # package required for Random Forest

str(data)
dt1 <- data
set.seed(11276)	# for the consistency
str(dt1)

#splitting the dataset in 70:30 ratio
nrows1 <- nrow(dt1)
index1 <- sample(1:nrows1, 0.7 * nrows1)	# shuffle and divide
train_1 <- dt1[index1,]			        
test_1  <- dt1[-index1,]  

# (1) Naive Bayes:
# fitting the model
naiveb_test_1 <- naiveBayes(Casualty_Severity~.,data=test_1)

# predicting test set with the tested model
predicted_nb_test_1 <- predict(naiveb_test_1, test_1[,!names(test_1) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_nb_test_1 <- confusionMatrix(predicted_nb_test_1, test_1$Casualty_Severity)		
con_matrix_nb_test_1

# (2) Logistic Regression:

# fitting the model
lgr_test_1 <- glm(Casualty_Severity ~.,test_1, family="binomial")
summary(lgr_test_1)

#making prediction on the testing set
res_test_1 <- predict(lgr_test_1, type="response")  # prediction
summary(res_test_1)
tapply(res_test_1, test_1$Casualty_Severity, mean)
con_matrix_lgr_test_1<-table(test_1$Casualty_Severity, res_test_1 > 0.5)
con_matrix_lgr_test_1

predictedvalues_test_1 <- rep(0,7893)
predictedvalues_test_1[res_test_1>0.5] <- 1

test_1lgr <- test_1
test_1lgr$Casualty_Severity <- revalue(test_1lgr$Casualty_Severity, 
                                     c('High'='1','Low'='0'))
mean(predictedvalues_test_1 == test_1lgr[,15]) # correctness of prediction


# (3) Random Forest:

# fitting the model
ran_forest_test_1 <- randomForest(Casualty_Severity~., data=test_1)

# identifying the importance of variables based on Mean Decrease
imp_test_1 <- importance(ran_forest_test_1)

# checking the importance of variables
par(mfrow=c(1,1))
varImpPlot(ran_forest_test_1,pch=18,col='red',cex=1)

# performance of random forest model
plot(ran_forest_test_1, main="Random Forest: MSE error vs. no of trees")

# predicting test set with the tested model
predicted_rf_test_1   <- predict(ran_forest_test_1, test_1[,!names(test_1) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_rf_test_1 <- confusionMatrix(predicted_rf_test_1, test_1$Casualty_Severity)
con_matrix_rf_test_1


# (4) Decision Tree:

# fitting the model
detree_test_1 <- rpart(Casualty_Severity~.,test_1,method='class')
summary(detree_test_1)
rpart.plot(detree_test_1)
predicted_dt_test_1 <- predict(detree_test_1,test_1,type = 'class')
con_matrix_dt_test_1 <- confusionMatrix(predicted_dt_test_1,test_1$Casualty_Severity) 
con_matrix_dt_test_1

cfdt_test_1 <- table(predicted_dt_test_1,test_1$Casualty_Severity)
cfdt_test_1

# comparison of all the models based on accuracy

col <- c("#6adc04","#ff6b04")
par(mfrow=c(2,2))

fourfoldplot(con_matrix_nb_test_1$table, color = col, conf.level = 0, 
             margin = 1, main=paste("NaiveBayes (",round(con_matrix_nb_test_1$overall[1]*100),"%)",sep=""))

fourfoldplot(con_matrix_lgr_test_1, color = col, conf.level = 0, 
             margin = 1, main=paste("LGR (",round(mean(predictedvalues_test_1 == test_1lgr[,15])*100),"%)",sep=""))

fourfoldplot(con_matrix_rf_test_1$table, color = col, conf.level = 0, 
             margin = 1, main=paste("RandomForest (",round(con_matrix_rf_test_1$overall[1]*100),"%)",sep=""))

fourfoldplot(cfdt_test_1, color = col, conf.level = 0, 
             margin = 1, main=paste("Decision Tree (",round(con_matrix_dt_test_1$overall[1]*100),"%)",sep=""))


# comparison of all the models based on roc curve

par(mfrow=c(2,2))

# (1) Naive Bayes:

predicted_nb1 <- predict(naiveb_test_1, test_1, type = 'raw')

auc1 <- auc(test_1$Casualty_Severity,predicted_nb1[,2])
plot(roc(test_1$Casualty_Severity,predicted_nb1[,2]),
     main=paste('Naive Bayes (AUC =', round(auc1, digits = 3),')'), sep = '')

# (2) Logistic Regression:

predicted_lgr1 <- predict(lgr_test_1, test_1, type = 'response')

auc2 <- auc(test_1$Casualty_Severity,predicted_lgr1)
plot(roc(test_1$Casualty_Severity,predicted_lgr1),
     main=paste('LGR (AUC =', round(auc2, digits = 3),')'), sep = '')


# (3) Random Forest:

predicted_rf1 <- predict(ran_forest_test_1, test_1, type = 'prob')

auc3 <- auc(test_1$Casualty_Severity,predicted_rf1[,2])
plot(roc(test_1$Casualty_Severity,predicted_rf1[,2]),
     main=paste('Random Forest (AUC =', round(auc3, digits = 3),')'), sep = '')


# (4) Decision Tree:

predicted_dt1 <- predict(detree_test_1,test_1,type = 'prob')

auc4 <- auc(test_1$Casualty_Severity,predicted_dt1[,2])
plot(roc(test_1$Casualty_Severity,predicted_dt1[,2]),
     main=paste('Decision Tree (AUC =', round(auc4, digits = 3),')'), sep = '')


##########################################################################################

# removing the insignificant variables
train2 <- train_1[,c(-7,-8,-10,-11,-12,-13)] 
test2 <- test_1[,c(-7,-8,-10,-11,-12,-13)]
str(train1)

# (1) Naive Bayes:

# fitting the model
naiveb_test2 <- naiveBayes(Casualty_Severity~.,data=test2)

# predicting test set with the tested model
predicted_nb_test2 <- predict(naiveb_test2, test2[,!names(test2) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_nb_test2 <- confusionMatrix(predicted_nb_test2, test2$Casualty_Severity)		
con_matrix_nb_test2

# (2) Logistic Regression:

# fitting the model
lgr_test2 <- glm(Casualty_Severity ~.,test2, family="binomial")
summary(lgr_test2)

#making prediction on the testing set
res_test2 <- predict(lgr_test2, type="response")  # prediction
summary(res_test2)
tapply(res_test2, test2$Casualty_Severity, mean)
con_matrix_lgr_test2<-table(test2$Casualty_Severity, res_test2 > 0.5)
con_matrix_lgr_test2

predictedvalues_test2 <- rep(0,7893)
predictedvalues_test2[res_test2>0.5] <- 1
testlgr1 <- test2
testlgr1$Casualty_Severity <- revalue(testlgr1$Casualty_Severity, 
                                     c('High'='1','Low'='0'))
mean(predictedvalues_test2 == testlgr1[,9]) # correctness of prediction


# (3) Random Forest:

# fitting the model
ran_forest_test2 <- randomForest(Casualty_Severity~., data=test2)

# identifying the importance of variables based on Mean Decrease
imp_test2 <- importance(ran_forest_test2)

# checking the importance of variables
par(mfrow=c(1,1))
varImpPlot(ran_forest_test2,pch=18,col='red',cex=1)

# performance of random forest model
plot(ran_forest_test2, main="Random Forest: MSE error vs. no of trees")

# predicting test set with the tested model
predicted_rf_test2   <- predict(ran_forest_test2, test2[,!names(test2) %in% "Casualty_Severity"])

# confusion matrix to see the performance
con_matrix_rf_test2 <- confusionMatrix(predicted_rf_test2, test2$Casualty_Severity)
con_matrix_rf_test2


# (4) Decision Tree:

detree_test2 <- rpart(Casualty_Severity~.,test2,method='class')
summary(detree_test2)
rpart.plot(detree_test2)
predicted_dt_test2 <- predict(detree_test2,test2,type = 'class')
con_matrix_dt_test2 <- confusionMatrix(predicted_dt_test2,test2$Casualty_Severity) 
con_matrix_dt_test2

cfdt_test2 <- table(predicted_dt_test2,test2$Casualty_Severity)
cfdt_test2

