rm(list=ls(all=TRUE));graphics.off() 
library(inTrees)
library(randomForest); library(RRF); library(gbm); set.seed(1) 
data(iris); X <- iris[,1:(ncol(iris)-1)]; target <- iris[,"Species"] 
# res <- dataSimulate(1); X <- res$X; target <- res$target;
lists = list()

# measure user-defined conditions
myRule <- "X[,3] > 5 & X[,4] > 1"
measureRule(myRule,X,target)  # without providing the outcome of the condition
measureRule(myRule,X,target,"versicolor")  # providing the outcome of the condition

rf <- randomForest(X,as.factor(target),ntree=100) # random forest
lists[['rf']] <- RF2List(rf) # extract a list of trees

rrf <- RRF(X,as.factor(target),ntree=100) # regularized random forest
lists[['rrf']] <- RF2List(rrf)

gbmFit <- gbm(target~ ., data=cbind(X,target), n.tree = 100, # boosted trees
                interaction.depth = 10,distribution="multinomial")
lists[['gbm']] <- GBM2List(gbmFit,X)

v <- c("rf","rrf","gbm")
for(i in v){
 treeList <- lists[[i]]
 ruleExec0 <- extractRules(treeList,X) # transform to R-executable conditions
 ruleExec <- unique(ruleExec0) # unique rules
 cat( paste("There are ", length(ruleExec), " unique conditions. \n",sep="") )
 ruleMetric <- getRuleMetric(ruleExec,X,target) # measure rules

 lookup <- lookupRule(ruleMetric,c("X[,4]","X[,3]")) # look up rules including X[,4] and X[,3]
 ruleMetric <- pruneRule(ruleMetric,X,target) # prune each rule

 # selecting rules by threholds of frequency & error
 ix <- which(as.numeric(ruleMetric[,"freq"])>0.01 & as.numeric(ruleMetric[,"err"])< 0.4)
 ruleMetric <- ruleMetric[ix,]

 ruleMetric <- selectRuleRRF(ruleMetric,X,target) # rule selection
 learner <- buildLearner(ruleMetric,X,target) #build the simplified tree ensemble learner
 pred <- applyLearner(learner,X) #appy learner to data
 readableLearner <- presentRules(learner,colnames(X)) # present the rules with a more readable format

 # print(readableLearner)

 # -- frequent variable interactions or conditions in a tree ensemble
 ruleMetric <- getRuleMetric(ruleExec0,X,target) 
 freqPattern <- getFreqPattern(ruleMetric)
 #ruleMetric <- getRuleMetric(freqPattern,X,target)


}

#format the rule and metrics as a table in latex code
library(xtable)
print(xtable(ruleMetric), include.rownames=FALSE)
print(xtable(readableLearner), include.rownames=FALSE)


# --- decision tree and logistic regression
X.double <- apply(X,2,as.numeric)
library(glmnet)
cvNet <- cv.glmnet(X.double,as.factor(target), family = "multinomial",type.measure = "class")
coef <- coef(cvNet)

library(rpart)
r <- rpart(target ~. , X)
