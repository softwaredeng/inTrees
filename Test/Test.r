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

nRep <- 10 # in the paper it is set to be 100

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
ruleMetric <- selectRuleRRF(ruleMetric,X,Y)

# cvob1=cv.glmnet(as.matrix(X[,,drop=FALSE]),Y, type.measure="mae")
library(glmnet)
Y <- as.numeric(Y)
if (is.numeric(Y) == FALSE) stop("Target variable needs to be numerical!")
# if (length(unique(Y)) > 2) stop("Currently only support two class problem!")


ruleI = sapply(ruleMetric[,"condition"],rule2Table,X,Y)
colnamesSave <- colnames(ruleI)
colnames(ruleI) <- paste0("R_",1:ncol(ruleI))
glmModel <- cv.glmnet(as.matrix(ruleI),Y, type.measure="mae")
coef <- coef(glmModel)

grepl("^[^_]+_2",s)

ix.rules <- which( grepl("R_",rownames(coef) ))
ix.non.zero <- which (as.numeric(coef) != 0)
ix.effective.rules <- intersect(ix.rules, ix.non.zero)
coef[ix.effective.rules,]


coefReg <- 0.95 - 0.01*as.numeric(ruleMetric[,"len"])/max(as.numeric(ruleMetric[,"len"]))
rf <- RRF(ruleI,as.factor(target), flagReg = 1, coefReg=coefReg, mtry = (ncol(ruleI)*1/2) , ntree=50, maxnodes= 10,replace=FALSE) 
imp <- rf$importance/max(rf$importance)
feaSet <- which(imp > 0.01)

ruleSetPrunedRRF <- cbind(ruleMetric[feaSet,,drop=FALSE],impRRF=imp[feaSet])
ix = order(as.numeric(ruleSetPrunedRRF[,"impRRF"]),
           - as.numeric(ruleSetPrunedRRF[,"err"]),
           - as.numeric(ruleSetPrunedRRF[,"len"]),
           decreasing=TRUE)
ruleSelect <- ruleSetPrunedRRF[ix,,drop=FALSE]

glmModel <- cv.glmnet(as.matrix(X),Y, type.measure="mae")
coef <- coef(glmModel)
pred <- predict(glmModel,as.matrix(X[,])) #ixTest

ruleMetric <- pruneRule(ruleMetric,trainX,trainY,typeDecay = 1)
ruleMetric <- unique(ruleMetric)

ruleClassifier <- buildLearner(ruleMetric,trainX,trainY)
readable <- presentRules(ruleClassifier,colnames(trainX))
pred <- applyLearner(ruleClassifier,testX)
errRFInTrees=1-sum(pred==testY)/length(pred);

