# This is file is temporary


rm(list=ls(all=TRUE))
library(randomForest);
library(RRF);
graphics.off()

sourceDir <- function(path, trace = TRUE) {
  for (nm in list.files(path, pattern = "\\.[Rr]$")) {
    if(trace) cat(nm)
    source(file.path(path, nm))
    if(trace) cat("\n")
  }
}

sourceDir("devR/")

thisData <- c("german.data")
path <- paste(getwd(), "/data/",thisData,sep=""); #musk vehicle is good austra
X <- read.table(path,header=TRUE,sep = ",")
X[X[,]=="?"] <- NA
X <- na.roughfix(X)#Impute Missing Values by median/mode
Y <-  X[,ncol(X)]
X <- X[,-ncol(X)]

#Orig RF and calculate the importance
rf <- randomForest(X, as.factor(Y),ntree=100) 

treeList <- RF2List(rf)
ruleExec <- extractRules(treeList,X) 
ruleExec <- unique(ruleExec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step
ix <- sample(1:length(ruleExec),min(2000,length(ruleExec))) #randomly select 2000 rules
ruleExec <- ruleExec[ix,,drop=FALSE]
ruleMetric <- getRuleMetric(ruleExec,X,Y)

ruleMetric <- pruneRule(ruleMetric,X,Y,typeDecay = 1)
ruleMetric <- unique(ruleMetric)

ruleClassifier <- buildLearner(ruleMetric,X,Y)
readable <- presentRules(ruleClassifier,colnames(X))
pred <- applyLearner(ruleClassifier,X)
errRFInTrees=1-sum(pred==Y)/length(pred);

# Test XGBoost
library(data.table)
# test data set 1: iris
X <- within(iris,rm("Species")); Y <- iris[,"Species"]
X <- within(iris,rm("Species")); Y <- iris[,"Species"]
model_mat <- model.matrix(~. -1, data=X)
xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 50,objective = "multi:softprob", num_class = 3 )

# test data set 2: german data
path <- paste(getwd(), "/data/","german.data",sep=""); #musk vehicle is good austra
data <- read.table(path,header=TRUE,sep = ",")
data[data[,]=="?"] <- NA; data <- na.roughfix(data)#Impute Missing Values by median/mode
X <-  within(data,rm("Y")); Y <- data[,"Y"]
model_mat <- model.matrix(~. -1, data=X)
xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 50,objective = "binary:logistic" )

tree_list <- XGB2LIST(xgb,model_mat)

ruleExec <- extractRules(tree_list,model_mat,digits=3) 
ruleExec <- unique(ruleExec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step
ix <- sample(1:length(ruleExec),min(2000,length(ruleExec))) #randomly select 2000 rules
ruleExec <- ruleExec[ix,,drop=FALSE]
ruleMetric <- getRuleMetric(ruleExec,model_mat,Y)
ruleMetric <- pruneRule(ruleMetric,model_mat,Y,typeDecay = 1)
ruleSelect <- selectRuleRRF(ruleMetric, model_mat, Y)
ruleMetric <- unique(ruleMetric)

ruleClassifier <- buildLearner(ruleMetric,model_mat,Y)
readable <- presentRules(ruleClassifier,colnames(model_mat),digits=3)
pred <- applyLearner(ruleClassifier,model_mat)
errXGBoost=1-sum(pred==Y)/length(pred);


# Test GBM
require("gbm")
path <- paste(getwd(), "/data/","german.data",sep=""); #musk vehicle is good austra
data <- read.table(path,header=TRUE,sep = ",")
data[data[,]=="?"] <- NA; data <- na.roughfix(data)#Impute Missing Values by median/mode
Y <- data$Y
data$Y <- as.numeric(data$Y) - 1


gbm.model = gbm(Y~., data=data, shrinkage=0.01, distribution = 'bernoulli', cv.folds=5, n.trees=100, verbose=F)
tree_list <- GBM2List(gbm.model,within(data,rm("Y")) )

ruleExec <- extractRules(tree_list,data,digits=3) 
ruleExec <- unique(ruleExec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step
ix <- sample(1:length(ruleExec),min(2000,length(ruleExec))) #randomly select 2000 rules
ruleExec <- ruleExec[ix,,drop=FALSE]
ruleMetric <- getRuleMetric(ruleExec,data,Y)
ruleMetric <- pruneRule(ruleMetric,data,Y,typeDecay = 1)
ruleSelect <- selectRuleRRF(ruleMetric, data, Y)
ruleMetric <- unique(ruleMetric)

ruleClassifier <- buildLearner(ruleMetric,data,Y)
readable <- presentRules(ruleClassifier,colnames(data),digits=3)
pred <- applyLearner(ruleClassifier,data)
errGBM <- 1-sum(pred==Y)/length(pred);


# //
# 
# source("devR/selectRuleLinear.R")
# ruleMetricLinear <- ruleSelectLinear(ruleExec,X,Y)
# 
# # cvob1=cv.glmnet(as.matrix(X[,,drop=FALSE]),Y, type.measure="mae")
# 
# 
# rWeights <- cbind(ix=as.numeric(ruleIx), imp=as.numeric(coef[ix.effective.rules,]) )
# rownames(rWeights) <- NULL
# rWeights <- rWeights[order(-abs(rWeights[,"imp"])),]
# 
# 
# coefReg <- 0.95 - 0.01*as.numeric(ruleMetric[,"len"])/max(as.numeric(ruleMetric[,"len"]))
# rf <- RRF(ruleI,as.factor(target), flagReg = 1, coefReg=coefReg, mtry = (ncol(ruleI)*1/2) , ntree=50, maxnodes= 10,replace=FALSE) 
# imp <- rf$importance/max(rf$importance)
# feaSet <- which(imp > 0.01)
# 
# ruleSetPrunedRRF <- cbind(ruleMetric[feaSet,,drop=FALSE],impRRF=imp[feaSet])
# ix = order(as.numeric(ruleSetPrunedRRF[,"impRRF"]),
#            - as.numeric(ruleSetPrunedRRF[,"err"]),
#            - as.numeric(ruleSetPrunedRRF[,"len"]),
#            decreasing=TRUE)
# ruleSelect <- ruleSetPrunedRRF[ix,,drop=FALSE]
# 
# glmModel <- cv.glmnet(as.matrix(X),Y, type.measure="mae")
# coef <- coef(glmModel)
# pred <- predict(glmModel,as.matrix(X[,])) #ixTest
# 
# ruleMetric <- pruneRule(ruleMetric,trainX,trainY,typeDecay = 1)
# ruleMetric <- unique(ruleMetric)
# 
# ruleClassifier <- buildLearner(ruleMetric,trainX,trainY)
# readable <- presentRules(ruleClassifier,colnames(trainX))
# pred <- applyLearner(ruleClassifier,testX)
# errRFInTrees=1-sum(pred==testY)/length(pred);
# 
# ///