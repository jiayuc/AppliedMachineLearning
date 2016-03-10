rm(list=ls())

workd<-'~/GitHub/AppliedMachineLearning/hw2/'
filename <- 'adult.data'
library(caret)

#training parameters
epochs <- 50      #number of epochs. step size changes in each epoch
epochStep <- 300  #number of steps in each epoch. a/b updates in each step
batchSize <- 30   #number of steps in each batch. we run a 50-sample accuracy test for each batch
totalLambdas <- 4

#legend text and color to mark on the graph
legendText <- c(
  expression(paste(lambda, "=1e0   ")),
  expression(paste(lambda, "=1e-1   ")),
  expression(paste(lambda, "=1e-2   ")),
  expression(paste(lambda, "=1e-3   "))
)
legendColor <- 1:4

#to find accuracy of a SVM on given data
runTest <-function(a, b, xs, ys){
  result <- ys
  for(i in 1 : dim(xs)[1]){
    result[i] <- ys[i] * (sum(a*xs[i,])+b)
  }
  result <- result > 0
  ratio <- sum(result)
  ratio / dim(xs)[1]
}

accuAnalyze <- function(a, b, xs, ys){
  truePossitive <- 0
  trueNegative <- 0
  falsePossitive <- 0
  falseNegative <- 0
  for(i in 1 : dim(xs)[1]){
    if(ys[i]==1){
      if(sum(a*xs[i,])+b > 0){
        truePossitive <- truePossitive+1
      }else{
        falseNegative <- falseNegative+1
      }
    }else{
      if(sum(a*xs[i,])+b > 0){
        falsePossitive <- falsePossitive+1
      }else{
        trueNegative <- trueNegative+1
      }
    }
  }
  sampleSize <- dim(xs)[1]
  c(truePossitive/sampleSize, trueNegative/sampleSize, falsePossitive/sampleSize, falseNegative/sampleSize)
}

setwd(workd)
wdat <- read.csv(filename, header = FALSE)

#data preprocess: setup label, select continuous features, scale features. 
wdat[,15] <- ( wdat[,15]==' >50K' )*2 - 1 #scale label into -1 or 1
contData <- wdat[,-c(2,4,6,7,8,9,10,14)]
for(col in 1:6)
  contData[,col] <- scale(contData[,col], center=mean(contData[,col]), scale=sd(contData[,col]))

#seperate feature and label
bigx <- contData[,-c(7)]
bigy <- contData[,c(7)]

#seperate train, validate, test data set
trainIndex <- createDataPartition(y=bigy, p=0.8, list=FALSE)
trainX <- bigx[trainIndex, ]
trainY <- bigy[trainIndex]
evalX <- bigx[-trainIndex, ]  #eval contains validation data and testing data
evalY <- bigy[-trainIndex]

validIndex <- createDataPartition(y=evalY, p=0.5, list=FALSE)
validX <- evalX[validIndex, ]
validY <- evalY[validIndex]
testX <- evalX[-validIndex, ]
testY <- evalY[-validIndex]

#try out lambdas, store a/b/accu for each lambda
lambdas <- 10^( 0 : (1-totalLambdas) )
lambdaA <- matrix(0, totalLambdas, dim(trainX)[2])
lambdaB <- 1:totalLambdas
lambdaAccu <- 1:totalLambdas

#accuracy for each batch and each epoch (average of the batches in the epoch)
batchAccu <- matrix(0, epochs*(epochStep/batchSize), totalLambdas)
epochAccu <- matrix(0, epochs, totalLambdas)

initialA <- runif(dim(trainX)[2], -10, 10)
initialB <- runif(1, -10, 10)
for( numLam in 1:totalLambdas ){
  lambda <- lambdas[numLam]
  #set initial value
  epochA <- 0.1  #this is for the neng=1/(ar+b)
  epochB <- 50
  a <- initialA
  b <- initialB
  for(epoch in 1:epochs){ #for each epoch
    #print(epoch)
    neng <- 1/(epochA * epoch + epochB) #calc stepSize=neng=1/(ar+b)
    for(batch in 1:(epochStep/batchSize)){  #for each batch, we need to do accuracy eval on 50 samples
      for(step in 1:batchSize){ #for each step, update a/b
        #random select one entry
        rand <- ceiling(runif(1, 1, dim(trainX)[1]))
        #update a, b
        r <- (sum(trainX[rand,]*a) + b) * trainY[rand]
        if( r >= 1 ){
          a <- a - lambda*a*neng
        } else {
          a <- a - neng*( lambda * a - trainY[rand]*trainX[rand,])
          b <- b - neng*(0 - trainY[rand] )
        }
      }
      #select 50 entrys
      rands <- ceiling(runif(50, 1, dim(trainX)[1]))
      #eval accu, and record
      tmpResult <- runTest(a, b, trainX[rands, ], trainY[rands])
      batchAccu[ batch + (epoch-1)*(epochStep/batchSize), numLam ] <- tmpResult
      print(tmpResult)
      epochAccu[epoch, numLam] <- epochAccu[epoch, numLam]+tmpResult
    }
    epochAccu[epoch, numLam] <- epochAccu[epoch, numLam]/(epochStep/batchSize)
  }
  
  #use validation data to determine accuracy for this lambda. record a, b
  validAccu <- runTest(a, b, validX, validY)
  lambdaAccu[numLam] <- validAccu
  for(dima in 1:dim(a)[2])
    lambdaA[numLam, dima] <- a[[dima]]
  lambdaB[numLam] <- b
  
  #plot training curve for this lambda
  matplot(batchAccu, type = c('l'),lty=1, ylim=c(0,1), col=1:totalLambdas)
  legend("bottomright", inset=.05, legend=legendText, 
         lty=1, col=legendColor, horiz=FALSE)
}

#select best SVM by different Lambda
bestLambda <- 1
for(numLam in 1:4){
  if(lambdaAccu[numLam] > lambdaAccu[bestLambda])
    bestLambda <- numLam
}
bestA <- lambdaA[bestLambda, ]
bestB <- lambdaB[bestLambda]

#test on test data
testAccu <- runTest(bestA, bestB, testX, testY)
accuAnalysis <- accuAnalyze(bestA, bestB, testX, testY)


#plot graph by batch
matplot(batchAccu, type = c('l'),lty=1, ylim=c(0,1), col=1:totalLambdas, 
        xlab='batch', ylab='accuracy')
legend("bottomright", inset=.05, legend=legendText, 
       lty=1, col=legendColor, horiz=FALSE)

#plot graph by epoch
matplot(epochAccu, type = c('l'),lty=1, ylim=c(0,1), col=1:totalLambdas, 
        xlab='epoch', ylab='accuracy')
legend("bottomright", legend=legendText, inset=.05,
       lty=1, col=legendColor, horiz=FALSE)

