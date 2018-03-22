
# --- the following contains the code producing the results
# since the results are saved in the R space, don't need to run this. but just giving the context. 

# rm(list= ls())
# library(ranger)
# library(inTrees)
# library(RRF)
# set.seed(1)
# sourceDir <- function(path, trace = TRUE) {
#   for (nm in list.files(path, pattern = "\\.[Rr]$")) {
#     if(trace) cat(nm)
#     source(file.path(path, nm))
#     if(trace) cat("\n")
#   }
# }
# 
# sourceDir("Test/devR/")
# 
# # data 1
# X <- within(iris,rm("Species")); Y <- iris[,"Species"]
# rf_ranger <- ranger(Species ~ ., data = iris)
# 
# # data set 2
# path <- paste(getwd(), "/Test/data/german.data",sep="") #musk vehicle is good austra
# data <- read.table(path,header=TRUE,sep = ",")
# X <- within(data,rm("Y")); Y <- data$Y
# rf_ranger <- ranger(Y ~ ., data = data)


library(ranger)
library(inTrees)

tree_list <- Ranger2List(rf_ranger)

debug_tree_list <- list()
debug_tree_list$ntree <- 1
debug_tree_list$list[[1]] <- tree_list$list[[1]]
rule_exec <- extractRules(debug_tree_list,X,digits=10) 
rule_metric <- getRuleMetric(rule_exec[1,,drop=FALSE],X,Y)

# original tree from ranger
orig_tree <- treeInfo(rf_ranger, tree = 1) 

# the first rule from the tree
print("the first rule from the tree (top down and most left)")
print(rule_exec[1,,drop=FALSE])

# this rule does not have any row qualify this rule, which should not be the case?

which(X[,1] %in% c('less-200DM') & X[,3] %in% c(' bank-paid-duly') & X[,6] %in% c(' less100DM') & X[,12] %in% c(' car') & X[,13]<=51 & X[,13]<=44.5)


print("original tree structure")
print(orig_tree[1:55,])

# The first rule, i.e., most left rule from the first tree
# it goes through the following path:
# node 0 (root node) split by (X3,2.5), 
# node 1 by (X13,51.0)
# node 3 by (X1, 2.5)
# node 7 by (X12, 2.5)
# node 13 by (X6, 2.5)
# node 25 stop due to max length of 5


print("converted tree structure from ranger.")
print( tree_list$list[[1]][1:55,])
# it goes through the following path:
# node 1 (root node) split by (X3,2.5), 
# node 2 by (X13,51.0)
# node 4 by (X1, 2.5)
# node 8 by (X12, 2.5)
# node 14 by (X6, 2.5)
# node 26 stop due to max length of 5


save.image("intrees_ranger_debug.RData")