rm(list=ls())
workd<-'C:/Users/jiayu/OneDrive/!CS498_aml/hw1'
setwd(workd)
filename<-'pima-indians-diabetes.data'
wdat<-read.csv(filename, header=FALSE)
library(klaR)
library(caret)
bigx<-wdat[,-c(9)]
bigy<-as.factor(wdat[,9])
wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
svm<-svmlight(bigx[wtd,], bigy[wtd], pathsvm='C:/Users/jiayu/OneDrive/!CS498_aml/svm_light_windows64/')
labels<-predict(svm, bigx[-wtd,])
foo<-labels$class
sum(foo==bigy[-wtd])/(sum(foo==bigy[-wtd])+sum(!(foo==bigy[-wtd])))
