
# print("Test inTrees Package ...")

# suppressMessages(library(inTrees))

# print(paste0("intrees version:",packageVersion("inTrees")))

# source("demo_rf.R")
# source("demo_gbm.R")
# source("demo_xgboost.R")
# source("demo_ranger.R")


rm(list= ls())

print("Test inTrees dev ...")

sourceDir <- function(path, trace = TRUE) {
  for (nm in list.files(path, pattern = "\\.[Rr]$")) {
    if(trace) cat(nm)
    source(file.path(path, nm))
    if(trace) cat("\n")
  }
}
sourceDir("Test/devR/")


path <- paste(getwd(), "/Test/data/german.data",sep="") #musk vehicle is good austra
data <- read.table(path,header=TRUE,sep = ",",stringsAsFactors=TRUE)
X <- within(data,rm("Y")); Y <- data$Y
ix <- sample(1:nrow(X), round(nrow(X)/2) ) 
dataTrain <- data[ix,]
dataTest <- data[-ix,]
xTrain <- X[ix,]
yTrain <- Y[ix]
xTest <- X[-ix,]
yTest <- Y[-ix]

rf <- randomForest(xTrain, as.factor(yTrain),ntree=100) 
evaluate_tree_list( RF2List(rf), xTrain, yTrain, xTest, yTest )

gbm.model = gbm(Y~., data=data, shrinkage=0.01, distribution = 'bernoulli', cv.folds=2, n.trees=50, verbose=F)
evaluate_tree_list( GBM2List(gbm.model,xTrain), xTrain, yTrain, xTest, yTest )

rf_ranger <- ranger(yTrain ~ ., data = dataTrain, num.trees = 100)
evaluate_tree_list( Ranger2List(rf_ranger), xTrain, yTrain, xTest, yTest )


model_mat <- model.matrix(~. -1, data=X)
xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 40,objective = "multi:softprob", 
               eval_metric= 'mlogloss', num_class = 3, 
               verbose = 0)

rf <- randomForest(xTrain, as.factor(yTrain),ntree=100) 
evaluate_tree_list( RF2List(rf), xTrain, yTrain, xTest, yTest )

source("demo_rf.R")
source("demo_gbm.R")
source("demo_xgboost.R")
source("demo_ranger.R")