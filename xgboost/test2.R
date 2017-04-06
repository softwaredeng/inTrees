library(inTrees)
library(randomForest) 
data(iris)
X <- iris[, 1:(ncol(iris) - 1)]  # X: predictors
target <- iris[,"Species"]  # target: class
rf <- randomForest(X, as.factor(target))
treeList <- RF2List(rf)  # transform rf object to an inTrees' format
exec <- extractRules(treeList, X)  # R-executable conditions
exec[1:2,]

ruleMetric <- getRuleMetric(exec,X,target)  # get rule metrics
ruleMetric[1:2,]

ruleMetric <- pruneRule(ruleMetric, X, target)
ruleMetric[1:2,]

(ruleMetric <- selectRuleRRF(ruleMetric, X, target))

(learner <- buildLearner(ruleMetric, X, target))

readableRules <- presentRules(ruleMetric, colnames(X))
readableRules[1:2, ]

rf <- randomForest(X, as.factor(target))
treeList <- RF2List(rf)  # transform rf object to an inTrees' format
exec <- extractRules(treeList, X)  # R-executable conditions
ruleMetric <- getRuleMetric(exec, X, target)  # get rule metrics
freqPattern <- getFreqPattern(ruleMetric)
# interactions of at least two predictor variables
freqPattern[which(as.numeric(freqPattern[, "len"]) >= 2), ][1:4, ]

