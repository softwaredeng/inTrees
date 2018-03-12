The R package for inTrees framework. 

Reference: https://arxiv.org/abs/1408.5456

Python wrapper by @GillesVandewiele: https://github.com/IBCNServices/GENESIM/blob/master/constructors/inTrees.py

# V1.2: add xgboost

library(randomForest);
library(RRF);
library(xgboost)
library(inTrees)
require(data.table)
require(xgboost)

X <- within(iris,rm("Species")); Y <- iris[,"Species"]
model_mat <- model.matrix(~. -1, data=X)
xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 50,objective = "multi:softprob", num_class = 3 )

tree_list <- XGB2List(xgb,model_mat)

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