Test: 
library('xgboost')
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
class(train$label)
class(train$data)
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, 
               eta = 1, nround = 2, nthread = 2, objective = "binary:logistic")


install xgboost from R may produce R version error. 

https://github.com/dmlc/xgboost/tree/master/R-package

install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")

the above workds