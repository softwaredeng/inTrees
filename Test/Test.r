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

ruleMetric <- pruneRule(ruleMetric,X,Y,typeDecay = 1)
ruleMetric <- unique(ruleMetric)

ruleClassifier <- buildLearner(ruleMetric,X,Y)
readable <- presentRules(ruleClassifier,colnames(X))
pred <- applyLearner(ruleClassifier,X)
errRFInTrees=1-sum(pred==Y)/length(pred);

# Test XGBoost
X1 <- data.table(X, keep.rownames = F)
sparse_matrix <- model.matrix(X20~.-1, data=X)
xgb <- xgboost(sparse_matrix, label = as.numeric(Y) - 1, nrounds = 20,objective = "binary:logistic" )
feature_names <- colnames(sparse_matrix)
xt<-xgb.model.dt.tree(feature_names = as.character(1:length(feature_names)), model=xgb)

xt[Feature == 'Leaf', Feature := '-1']
xt[, 'split var' := as.integer(Feature)]
xt[, 'split point' := Split]
xt[, 'left daughter' := as.integer(tstrsplit(Yes, '-')[[2]]) + 1]
xt[, 'right daughter' := as.integer(tstrsplit(No, '-')[[2]]) + 1]
xt[, MissingNode := as.integer(tstrsplit(Missing, '-')[[2]]) + 1]
xt[, Weight := Cover]
xt[, Prediction := Quality]
xt[, Node := Node + 1]
xt[, c('ID', 'Yes', 'No', 'Split','Missing', 'Quality', 'Cover', 'Feature') := NULL]
for (f in c('left daughter', 'right daughter', 'MissingNode'))
  set(xt, which(is.na(xt[[f]])), f, -1)
treeList1 <- NULL
treeList1$ntree <- length(unique(xt$Tree))
treeList1$list <- split(xt, by="Tree")
formatXGB <-
  function(tree){
    rownames(tree) <- 1:nrow(tree)
    tree$status <- ifelse(tree$`split var`==-1,-1,1)
    tree$`split point` <- as.numeric(tree$`split point`)
    tree <- tree[,c("left daughter","right daughter","MissingNode","split var","split point","status")]
    # ix <- tree$MissingNode[which(tree$MissingNode>0)]
    # if(length(ix)>0)  tree$status[ix] <- 10 #missing 
    tree <- tree[,c("left daughter","right daughter","split var","split point","status")]
    tree <- as.data.frame(tree)
}
treeList1$list <- lapply(treeList1$list,formatXGB)

sourceDir("devR/")
ruleExec1 <- extractRules(treeList1,sparse_matrix) 


# ----- 

source("devR/selectRuleLinear.R")
ruleMetricLinear <- ruleSelectLinear(ruleExec,X,Y)

# cvob1=cv.glmnet(as.matrix(X[,,drop=FALSE]),Y, type.measure="mae")


rWeights <- cbind(ix=as.numeric(ruleIx), imp=as.numeric(coef[ix.effective.rules,]) )
rownames(rWeights) <- NULL
rWeights <- rWeights[order(-abs(rWeights[,"imp"])),]


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

