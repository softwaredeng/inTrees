# do not install xgboost from R cran that would produce R version error instead do the following. 
# https://github.com/dmlc/xgboost/tree/master/R-package
# install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")

library(randomForest)
rf <- randomForest(Species ~ ., data=iris)
getTree(rf, 1, labelVar=TRUE) 


library('xgboost')
# data(iris)
# X <- iris[, 1:(ncol(iris) - 1)]  # X: predictors
#target <- iris[,"Species"]  # target: class
#bst <- xgboost(data = as.matrix(X), label = target, max.depth = 2, 
#               eta = 20, nround = 20, nthread = 20, objective = "binary:logistic")

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
class(train$label)
class(train$data)
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, 
               eta = 20, nround = 20, nthread = 20, objective = "binary:logistic")


bst.str <- xgb.dump(bst, with_stats=F)
tree.mats <- NULL
tree.num <- 0
for(item in 1:length(bst.str)){
  # get tree number
  if( regexpr( "booster\\[.*\\]$", bst.str[item] ) == 1 ){
    tree.num <- tree.num + 1
    tree.mats[[ tree.num ]] <- NULL
    next
  }else if( regexpr( "leaf=", bst.str[item] ) == 1 ){
    node.index <- regexpr( "^.*:", bst.str[2] )
    node.index <- as.numeric( substr( bst.str[2], node.index, node.index + attr(node.index,"match.length") - 2 ) )
    this.node <- c(node=node.index, left = 0, right = 0, split = NA, split_value = 0, status = -1, prediction = NA)
  }else{
    # identify decision node: node number
    node.index <- regexpr( "^.*:", bst.str[2] )
    node.index <- as.numeric(  substr( bst.str[2], node.index, node.index + attr(node.index,"match.length") - 2 )  )
    
    # split variable
    feature.index <- regexpr( "\\[f[[:digit:]]*<", bst.str[2] )
    feature.index <- as.numeric(  substr( bst.str[2], feature.index + 2, feature.index + attr(feature.index,"match.length") - 2 )   )
    # split value
    split.value <- regexpr( "<.*]", bst.str[2] )
    split.value <- substr( bst.str[2], split.value + 1, split.value + attr(split.value,"match.length") - 2 )  
    split.value <- as.numeric(split.value)
    
    # left node
    left.index <- regexpr( "yes=[[:digit:]]*,", bst.str[2] )
    left.index <- as.numeric( substr( bst.str[2], left.index + 4, left.index + attr(left.index,"match.length") - 2 )  )
    # right node
    right.index <- regexpr( "no=[[:digit:]]*,", bst.str[2] )
    right.index <- substr( bst.str[2], right.index + 3, right.index + attr(right.index,"match.length") - 2 )  
    this.node <- c(node=node.index, left = left.index, right = right.index, split = feature.index, split_value = split.value, status = 1, prediction = NA)
  }
  tree.mats[[ tree.num ]] <- rbind( tree.mats[[ tree.num ]] , this.node )

}

