require(xgboost)
require(data.table)
XGB2List<-
  function(xgb, X)
  {
    feature_names <- colnames(X)
    xt <- xgb.model.dt.tree(feature_names = as.character(1:length(feature_names)), model=xgb)
    # avoid cran note: no visible binding for global variable
    Feature=Split=Yes=No=MissingNode=Missing=Weight=Cover=Prediction=Quality=Node=NULL
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
    treeList <- NULL
    treeList$ntree <- length(unique(xt$Tree))
    treeList$list <- split(xt, by="Tree")
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
    treeList$list <- lapply(treeList$list,formatXGB)
    return(treeList)
  }