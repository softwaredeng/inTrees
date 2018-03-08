require(xgboost)
require(data.table)
XGB2LIST<-
  function(xgb, X)
  {
    dtrain <- xgb.DMatrix(as.matrix(X))
    xt<-xgb.model.dt.tree(feature_names = as.character(1:ncol(dtrain)), model=xgb)
    xt[Feature == 'Leaf', Feature := '-1']
    xt[, 'split var' := as.integer(Feature)]
    xt[, 'split point' := ifelse(`split var` > -1, Split, Quality)]
    xt[, 'left daughter' := as.integer(tstrsplit(Yes, '-')[[2]]) + 1]
    xt[, 'right daughter' := as.integer(tstrsplit(No, '-')[[2]]) + 1]
    xt[, MissingNode := as.integer(tstrsplit(Missing, '-')[[2]]) + 1]
    xt[, Weight := Cover]
    xt[, Prediction := Quality]
    xt[, Node := Node + 1]
    xt[, c('ID', 'Yes', 'No', 'Split','Missing', 'Quality', 'Cover', 'Feature') := NULL]
    for (f in c('left daughter', 'right daughter', 'MissingNode'))
      set(xt, which(is.na(xt[[f]])), f, -1)
    treeList<-split(xt, by="Tree")
    
    formatXGB <-
      function(tree){
        a <- tree
        rownames(a) <- 1:nrow(a)
        a$status <- a$`split var`
        a <- a[,c("left daughter","right daughter","MissingNode","split var","split point","status")]
        
        ix <- a$MissingNode[which(a$MissingNode>0)]
        if(length(ix)>0)  a$status[ix] <- 10 #missing #a <- a[-ix,]
        a <- a[,c("left daughter","right daughter","split var","split point","status")]
        
      }
    
    return(treeList=lapply(treeList,formatXGB))
    
  }