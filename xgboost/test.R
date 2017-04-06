# do not install xgboost from R cran that would produce R version error instead do the following. 
# https://github.com/dmlc/xgboost/tree/master/R-package
# install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")

 
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
for(item in 1:length(bst.str)){
  # get tree number
  regexpr( "^booster\\[.*\\]$", bst.str[1] )
  # identify a leaf. so status would be zero, children nodes and split values would be 0
  regexpr( "leaf=", bst.str[4] )
  # identify decision node: node number
  node.index <- regexpr( "^.*:", bst.str[2] )
  node.index <- substr( bst.str[2], node.index, node.index + attr(node.index,"match.length") - 2 )

  # split variable
  feature.index <- regexpr( "\\[f[[:digit:]]*<", bst.str[2] )
  feature.index <- substr( bst.str[2], feature.index + 2, feature.index + attr(feature.index,"match.length") - 2 )  
  # split value
  split.value <- regexpr( "<.*]", bst.str[2] )
  split.value <- substr( bst.str[2], split.value + 1, split.value + attr(split.value,"match.length") - 2 )  
  split.value <- as.numeric(split.value)
    
  # left node
  left.index <- regexpr( "yes=[[:digit:]]*,", bst.str[2] )
  left.index <- substr( bst.str[2], left.index + 4, left.index + attr(left.index,"match.length") - 2 )  
  # right node
  right.index <- regexpr( "no=[[:digit:]]*,", bst.str[2] )
  right.index <- substr( bst.str[2], right.index + 3, right.index + attr(right.index,"match.length") - 2 )  
  
}

