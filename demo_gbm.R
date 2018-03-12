rm(list= ls())
library(gbm)
library(inTrees)

path <- paste(getwd(), "/Test/data/german.data",sep="") #musk vehicle is good austra
data <- read.table(path,header=TRUE,sep = ",")
X <- within(data,rm("Y")); Y <- data$Y

data$Y <- as.numeric(data$Y) - 1

gbm.model = gbm(Y~., data=data, shrinkage=0.01, distribution = 'bernoulli', cv.folds=5, n.trees=50, verbose=F)
tree_list <- GBM2List(gbm.model,X)

rule_exec <- extractRules(tree_list,X,digits=3) 
rule_exec <- unique(rule_exec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step
ix <- sample(1:length(rule_exec),min(2000,length(rule_exec))) #randomly select 2000 rules
rule_exec <- rule_exec[ix,,drop=FALSE]
rule_metric <- getRuleMetric(rule_exec,X,Y)
# freq_patterns <- getFreqPattern(rule_metric,maxlen = 3) # can be less useful for numeric variables.
rule_metric <- pruneRule(rule_metric,X,Y,typeDecay = 1)
rule_select <- selectRuleRRF(rule_metric, X, Y)
rule_metric <- unique(rule_metric)

rule_classifier <- buildLearner(rule_metric,X,Y)
readable <- presentRules(rule_classifier,colnames(X),digits=3)
pred <- applyLearner(rule_classifier,X)
print( 1-sum(pred==Y)/length(pred) )