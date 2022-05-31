rm(list= ls())
library(xgboost)
library(inTrees)

# data 1
X <- within(iris,rm("Species")); Y <- iris[,"Species"]
model_mat <- model.matrix(~. -1, data=X)
xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 40,objective = "multi:softprob", num_class = 3 )

# data set 2
# path <- paste(getwd(), "/Test/data/german.data",sep="") #musk vehicle is good austra
# data <- read.table(path,header=TRUE,sep = ",")
# X <- within(data,rm("Y")); Y <- data$Y
# model_mat <- model.matrix(~. -1, data=X)
# xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 50,objective = "binary:logistic" )


tree_list <- XGB2List(xgb,model_mat)

rule_exec <- extractRules(tree_list,model_mat,digits=3) 
rule_exec <- unique(rule_exec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step
ix <- sample(1:length(rule_exec),min(2000,length(rule_exec))) #randomly select 2000 rules
rule_exec <- rule_exec[ix,,drop=FALSE]
rule_metric <- getRuleMetric(rule_exec,model_mat,Y)
# freq_patterns <- getFreqPattern(rule_metric,maxlen = 3) # can be less useful for numeric variables.
rule_metric <- pruneRule(rule_metric,model_mat,Y,typeDecay = 1)
rule_select <- selectRuleRRF(rule_metric, model_mat, Y)
rule_metric <- unique(rule_metric)

rule_classifier <- buildLearner(rule_metric,model_mat,Y)
readable <- presentRules(rule_classifier,colnames(model_mat),digits=3)
pred <- applyLearner(rule_classifier,model_mat)
print( 1-sum(pred==Y)/length(pred) )