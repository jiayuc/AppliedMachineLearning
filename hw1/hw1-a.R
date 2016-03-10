rm(list=ls())
workd<-'C:/Users/jiayu/OneDrive/!CS498_aml/hw1'
setwd(workd)
filename<-'pima-indians-diabetes.data'
wdat<-read.csv(filename, header=FALSE)
library(klaR)
library(caret)

bigx<-wdat[,-c(9)]
bigy<-wdat[,9]

#perform on training data
wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
nbx<-bigx
ntrbx<-nbx[wtd, ]
ntrby<-bigy[wtd]
#perform on test data
ntebx<-nbx[-wtd, ]
nteby<-bigy[-wtd]


#seperate pos/neg data of TRAINING data
trposflag<-ntrby>0
ptregs<-ntrbx[trposflag, ]
ntregs<-ntrbx[!trposflag,]


ptrmean<-sapply(ptregs, mean)
ntrmean<-sapply(ntregs, mean)
ptrsd<-sapply(ptregs, sd)
ntrsd<-sapply(ntregs, sd)
ptroffsets<-t(t(ntrbx)-ptrmean)
ptrscales<-t(t(ptroffsets)/ptrsd)
ptrlogs<--(1/2)*rowSums(apply(ptrscales,c(1, 2), function(x)x^2))-sum(log(ptrsd))
ntroffsets<-t(t(ntrbx)-ntrmean)
ntrscales<-t(t(ntroffsets)/ntrsd)
ntrlogs<--(1/2)*rowSums(apply(ntrscales,c(1, 2), function(x)x^2))-sum(log(ntrsd))
lvwtr<-ptrlogs>ntrlogs

#got the training result, p(y|x) of each patient
gotrighttr<-lvwtr==ntrby #compare with right result

trscore<-sum(gotrighttr)/(sum(gotrighttr)+sum(!gotrighttr))
pteoffsets<-t(t(ntebx)-ptrmean)
ptescales<-t(t(pteoffsets)/ptrsd)
ptelogs<--(1/2)*rowSums(apply(ptescales,c(1, 2), function(x)x^2) )-sum(log(ptrsd))
nteoffsets<-t(t(ntebx)-ntrmean)
ntescales<-t(t(nteoffsets)/ntrsd)
ntelogs<--(1/2)*rowSums(apply(ntescales,c(1, 2), function(x)x^2) )-sum(log(ntrsd))
lvwte<-ptelogs>ntelogs
gotright<-lvwte==nteby
myscore<-sum(gotright)/(sum(gotright)+sum(!gotright))
show(myscore)