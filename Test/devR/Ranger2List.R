require(ranger)
Ranger2List <- function(rf_ranger)
{
  formatRanger <- function(tree){
    rownames(tree) <- 1:nrow(tree)
    tree$`status` <- ifelse(tree$`terminal`==TRUE,-1,1)
    tree$`left daughter` <- tree$`leftChild` + 1
    tree$`right daughter` <- tree$`rightChild` + 1
    tree$`split var` <- tree$`splitvarID` + 1
    tree$`split point` <- tree$`splitval`
    tree$`prediction` <- tree$`prediction`
    tree <- tree[,c("left daughter","right daughter","split var","split point","status")]
    tree <- as.data.frame(tree)
    return(tree)
  }
  treeList <- NULL
  treeList$ntree <- rf_ranger$num.trees
  treeList$list <- vector("list",rf_ranger$num.trees)
  for(i in 1:rf_ranger$num.trees){
    treeList$list[[i]] <- formatRanger( treeInfo(rf_ranger, tree = i) )
  }
  return(treeList)
}