# Citation
# Houtao Deng, Interpreting Tree Ensembles with inTrees, Houtao Deng, arXiv:1408.5456, 2014
# The code below produces the results in the paper above (randomness of the tree ensembles can lead to slight differences of the results)

rm(list=ls(all=TRUE))
library(inTrees);
library(randomForest);
graphics.off()

# The regular expression are used to find a match with the ending characters of .R, .r
## If you want to source() a bunch of files, something like## the following may be useful:
sourceDir <- function(path, trace = TRUE) {
  for (nm in list.files(path, pattern = "\\.[Rr]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm))
    if(trace) cat("\n")
  }
}

sourceDir("R/")

nRep <- 10 # in the paper it is set to be 100

dataV <- c("iris.data","auto.data","breast.data","pima.data","heart.data","german.data","waveform.data","horse.data","austra.data",
"crx.data","vehicle.data","labor.data","wine.data","zoo.data","tic-tac.data","hepati.data","lymph.data","anneal.data",
"glass.data","led7.data")

eAll <- NULL;fAll <- NULL

for(dataI in 1:length(dataV)){
thisData <- dataV[dataI]
print(thisData)
path <- paste(getwd(), "/data/",thisData,sep=""); #musk vehicle is good austra

# R 3.1.1 seems to have a bug on parsing '?' as NA when it is in the begining of a row
# hence use a temporary solution below
X <- read.table(path,header=TRUE,sep = ",")
X[X[,]=="?"] <- NA
write.table(X,"tmp.txt",col.names = TRUE,quote=FALSE,sep = ",")
X <- read.table("tmp.txt",header=TRUE,sep = ",",na.strings <- c(''))

Y <-  X[,ncol(X)]
X <- X[,-ncol(X)]
for(ii in ncol(X):1){ # remove attributes with 1 or 0 level
 if( length(unique(X[!is.na(X[,ii]),ii])) <= 1  ) X <- X[,-ii] 
}

X <- na.roughfix(X)#Impute Missing Values by median/mode

for(foldI in 1:nRep)
{
ixTrain <- sample(1:nrow(X),round(2*nrow(X)/3),replace=FALSE)
ixTest <- setdiff(1:nrow(X),ixTrain)

testX <- X[ixTest,];trainX <- X[ixTrain,]
testY <- as.character(Y[ixTest]);trainY <- as.character(Y[ixTrain])

#Orig RF and calculate the importance
rf <- randomForest(trainX, as.factor(trainY),ntree=100) 

treeList <- RF2List(rf)
ruleExec <- extractRules(treeList,trainX) 
ruleExec <- unique(ruleExec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step

ix <- sample(1:length(ruleExec),min(2000,length(ruleExec))) #randomly select 2000 rules
ruleExec <- ruleExec[ix,,drop=FALSE]

ruleMetric <- getRuleMetric(ruleExec,trainX,trainY)
ruleMetric <- pruneRule(ruleMetric,trainX,trainY,typeDecay = 1)
ruleMetric <- unique(ruleMetric)

ruleClassifier <- buildLearner(ruleMetric,trainX,trainY)
readable <- presentRules(ruleClassifier,colnames(trainX))
pred <- applyLearner(ruleClassifier,testX)
errRFInTrees=1-sum(pred==testY)/length(pred);

library(rpart)
trainX <- data.frame(trainX)
D <- data.frame(cbind(trainX,trainY))
D[,"trainY"] <- as.factor(D[,"trainY"])
rp <- rpart(trainY ~. , D)
pred <- as.character(predict(rp,data.frame(testX),type="class"))
errRPART <- 1-sum(pred==testY)/length(pred);

thisE <- c(errRFInTrees, errRPART)
cat("foldI",foldI,thisE,"-  ")
eAll <- rbind(eAll,thisE)

}
}
colnames(eAll) <- c("STEL","rpart")

# avearge error for each data set
aveMat <- NULL
nRep <- nrow(eAll)/length(dataV)
for(I in 1:length(dataV))
{
thisE <- eAll[((I-1)*nRep+1):(I*nRep),]
aveMat <- rbind(aveMat,apply(thisE,2,mean))
}
aveMat - aveMat[,1]

