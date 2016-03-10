rm(list=ls())
workd<-'C:/Users/jiayu/OneDrive/!CS498_aml/hw1'
setwd(workd)
filename<-'pima-indians-diabetes.data'
wdat<-read.csv(filename, header=FALSE)
library(klaR)
library(caret)
library(e1071)

bigx<-wdat[,-c(9)]
bigy<-as.factor(wdat[,9])
wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
trax<-bigx[wtd,]
tray<-bigy[wtd]
model<-train(trax, tray, 'nb', trControl=trainControl(method='cv', number=10))
teclasses<-predict(model,newdata=bigx[-wtd,])
confusionMatrix(data=teclasses, bigy[-wtd])
