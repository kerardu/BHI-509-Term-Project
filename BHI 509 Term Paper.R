mm<-read.csv('/Users/geraldgaitos/Desktop/Graduate School Courses/BHI 509/mammographic_masses.data.csv')
dim(mm)

#Preprocessing the dataset
mm[mm=='?']<-NA
colnames(mm)[colSums(is.na(mm))>0]
na<-is.na(mm)
colSums(na)

sapply(as.list(mm),mode)

mm$BI.RADS<-as.integer(mm$BI.RADS)
mm$Shape<-as.integer(mm$Shape)
mm$Margin<-as.integer(mm$Margin)
mm$Density<-as.integer(mm$Density)
mm$Age<-as.integer(mm$Age)

sapply(as.list(mm),mode)

corr_matrix <- cor(mm)
corr_matrix

mm[is.na(mm$Age),'Age']<-mean(mm$Age,na.rm=TRUE)
mm[is.na(mm$BI.RADS),'BI.RADS']<-mean(mm$BI.RADS,na.rm=TRUE)
mm[is.na(mm$Shape),'Shape']<-mean(mm$Shape,na.rm=TRUE)
mm[is.na(mm$Margin),'Margin']<-mean(mm$Margin,na.rm=TRUE)
mm[is.na(mm$Density),'Density']<-mean(mm$Density,na.rm=TRUE)

colnames(mm)[colSums(is.na(mm))>0]
corr_matrix_2 <- cor(mm)
corr_matrix_2

#Statistical Analysis
library(mlogit)
library(car)

model<-lm(Severity~Age+BI.RADS+Shape+Margin+Density,data=mm)

# summarize the model
summary(model)

# test for overall significance of the model
anova(model)

# test for significance of each independent variable
summary(model)$coefficients

# calculate the correlation between the observed and predicted values
cor(mm$Severity, predict(model))

# plot the heatmap
library(ComplexHeatmap)
Heatmap(corr_matrix_2)

#Partitioning the dataset
set.seed(833)
mm$Severity<-as.factor(mm$Severity)

#randomizing sample 70%-30%
rnd<-sample(1:nrow(mm),as.integer(0.7*nrow(mm)))

#setting the training set
train<-mm[rnd,]
dim(train)

#setting the test set
test<-mm[-rnd,]
dim(test)

#CLASSIFICATION TREE
library(rpart)
library(rpart.plot)
library(caret)

train_tree<-train
test_tree<-test

#creating a model using the training set
set.seed(833)
tree_model<-rpart(Severity~.,data=train_tree,method='class')
prp(tree_model)

#testing the model
tree_pred<-predict(tree_model,test_tree,type="class")
#unname(tree_pred)

#confusion matrix
tree_conf<-confusionMatrix(tree_pred,test_tree$Severity)
tree_conf$table
tree_conf$overall[1]

#ARTIFICIAL NEURAL NETWORK
library(nnet)
library(NeuralNetTools)
library(caret) 

train_ann<-train
test_ann<-test

#creating the model
set.seed(833)

#ann size = 6
ann_model<-nnet(Severity~.,train_ann,size=6,trace=FALSE)

#testing the model
ann_pred<-predict(ann_model,test_ann,type="class")

#creating confusion matrix
ann_conf<-confusionMatrix(as.factor(ann_pred),test_ann$Severity)
ann_conf$table

#accuracy
ann_conf$overall[1]
plotnet(ann_model)

#ann size = 3
ann_model<-nnet(Severity~.,train_ann,size=3,trace=FALSE)

#testing the model
ann_pred<-predict(ann_model,test_ann,type="class")

#creating confusion matrix
ann_conf<-confusionMatrix(as.factor(ann_pred),test_ann$Severity)
ann_conf$table

#accuracy
ann_conf$overall[1]

#ann size = 9
ann_model<-nnet(Severity~.,train_ann,size=9,trace=FALSE)

#testing the model
ann_pred<-predict(ann_model,test_ann,type="class")

#creating confusion matrix
ann_conf<-confusionMatrix(as.factor(ann_pred),test_ann$Severity)
ann_conf$table

#accuracy
ann_conf$overall[1]

#SUPPORT VECTOR MACHINE
library(e1071)
library(caret)

train_svm<-train
test_svm<-test
set.seed(833)

#creating a prediction model using SVM kernel=radial
svm_model<-svm(Severity~.,train_svm,kernel="radial")
svm_pred<-predict(svm_model,test_svm,type="class")

#confusion matrix for SVM kernel=radial
svm_conf<-confusionMatrix(svm_pred,test_svm$Severity)
svm_conf$table

#accuracy for SVM kernel=radial
svm_conf$overall[1]

#creating a prediction model using SVM kernel=sigmoid
svm_model<-svm(Severity~.,train_svm,kernel="sigmoid")
svm_pred<-predict(svm_model,test_svm,type="class")

#confusion matrix for SVM kernel=sigmoid
svm_conf<-confusionMatrix(svm_pred,test_svm$Severity)
svm_conf$table

#accuracy for SVM kernel=sigmoid
svm_conf$overall[1]

#creating a prediction model using SVM kernel=polynomial
svm_model<-svm(Severity~.,train_svm,kernel="polynomial")
svm_pred<-predict(svm_model,test_svm,type="class")

#confusion matrix for SVM kernel=polynomial
svm_conf<-confusionMatrix(svm_pred,test_svm$Severity)
svm_conf$table

#accuracy for SVM kernel=polynomial
svm_conf$overall[1]

#K NEAREST NEIGHBORS
library(FNN)
library(caret)

train_knn<-train
test_knn<-test

set.seed(833)

#creating a prediction model for KNN k=3
knn_pred<-knn(train=train_knn[,1:5],test=test_knn[,1:5],cl=train_knn[,6],k=3)
knn_conf<-confusionMatrix(knn_pred,test_knn[,6])

#confusion matrix for KNN k=3
knn_conf$table
knn_conf$overall[1]

#creating a prediction model for KNN k=2
knn_pred<-knn(train=train_knn[,1:5],test=test_knn[,1:5],cl=train_knn[,6],k=2)
knn_conf<-confusionMatrix(knn_pred,test_knn[,6])

#confusion matrix for KNN k=2
knn_conf$table
knn_conf$overall[1]

#creating a prediction model for KNN k=4
knn_pred<-knn(train=train_knn[,1:5],test=test_knn[,1:5],cl=train_knn[,6],k=4)
knn_conf<-confusionMatrix(knn_pred,test_knn[,6])

#confusion matrix for KNN k=4
knn_conf$table
knn_conf$overall[1]
