evaluate_tree_list <- function(tree_list, xTrain, yTrain, xTest, yTest) {
  rule_exec <- extractRules(tree_list,xTrain,digits=10) 
  rule_exec <- unique(rule_exec) # remove same rules. NOTE: for variable interaction analysis, you should NOT perform this step
  ix <- sample(1:length(rule_exec),min(500,length(rule_exec))) #randomly select 2000 rules
  rule_exec <- rule_exec[ix,,drop=FALSE]
  rule_metric <- getRuleMetric(rule_exec,xTrain,yTrain)
  # freq_patterns <- getFreqPattern(rule_metric,maxlen = 3) # can be less useful for numeric variables.
  rule_metric <- pruneRule(rule_metric,xTrain,yTrain,typeDecay = 1)
  rule_select <- selectRuleRRF(rule_metric, xTrain, yTrain)
  rule_metric <- unique(rule_metric)
  
  rule_classifier <- buildLearner(rule_metric,xTrain,yTrain)
  readable <- presentRules(rule_classifier,colnames(xTrain),digits=3)
  pred <- applyLearner(rule_classifier,xTrain)
  err_train <- 1-sum(pred==yTrain)/length(pred)
  
  pred <- applyLearner(rule_classifier,xTest)
  err_test <- 1-sum(pred==yTest)/length(pred)
  
  return(list(err_train=err_train,err_test=err_test))
  
}