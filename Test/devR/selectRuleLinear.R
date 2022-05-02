ruleSelectLinear <- function(ruleMetric,X,target){
  # Input: ruleMetric data, X: predictor data; target: target varible
  # Output: ruleMetric with selected rules and their importance scores
  library(glmnet)
  Y <- as.numeric(Y)
  if (is.numeric(Y) == FALSE) stop("Target variable needs to be numerical!")
  
  ruleI = sapply(ruleMetric[,"condition"],rule2Table,X,Y)
  colnamesSave <- colnames(ruleI)
  colnames(ruleI) <- paste0("R_",1:ncol(ruleI))
  glmModel <- cv.glmnet(as.matrix(ruleI),Y, type.measure="mae",nfolds=3)
  coef <- coef(glmModel)
  
  ix.rules <- which( grepl("R_",rownames(coef) ))
  ix.non.zero <- which (as.numeric(coef) != 0)
  ix.effective.rules <- intersect(ix.rules, ix.non.zero)
  ruleIx <- gsub("R_","",names( coef[ix.effective.rules,] ))
  feaSet <- as.numeric(ruleIx)
  imp <- as.numeric(coef[ix.effective.rules,])
  ruleSet <- cbind(ruleMetric[feaSet,,drop=FALSE],imp=imp)
  ix = order(as.numeric(ruleSet[,"imp"]),decreasing=TRUE)
  ruleSet <- ruleSet[ix,,drop=FALSE]
  return(ruleSet)
}