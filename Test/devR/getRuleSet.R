getRuleSet<-function(treeList,X, n, maxdepth=6,random=FALSE){
  levelX = list()
  for(iX in 1:ncol(X))
    levelX <- c(levelX,list(levels(X[,iX])))
  # X <- NULL; target <- NULL
  allRulesList = list()
  if(random==TRUE){max_length = sample(1:maxdepth,1,replace=FALSE)}else{
    max_length = maxdepth}
  rule = list(); count = 0; rowIx = 1; 
  tree <- treeList$list[[n]]
  ruleSet = vector("list", length(which(tree[,"status"]==-1)))
  res = treeVisit(tree,rowIx = rowIx,count,ruleSet,rule,levelX,length=0,max_length=max_length)
  allRulesList = c(allRulesList, res$ruleSet)
  
  
  allRulesList <- allRulesList[!unlist(lapply(allRulesList, is.null))]
  cat(paste(length(allRulesList)," rules (length<=",  
            max_length, ") were extracted from the tree ", n,"\n",sep=""))
  
  rulesExec <- ruleList2Exec(X,allRulesList)
  return(rulesExec)
  
}