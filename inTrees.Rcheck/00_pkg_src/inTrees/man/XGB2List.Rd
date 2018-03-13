\name{XGB2List}
\alias{XGB2List}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform an xgboost object to a list of trees
}
\description{
Transform an xgboost object to a list of trees
}
\usage{
XGB2List(xgb, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xgb}{
%%     ~~Describe \code{gbm1} here~~
xgboost object
}
  \item{X}{
predictor variable matrix
}
}
\value{
  a list of trees in an inTrees-required format
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{XGB2List}}
}
\examples{
	library(data.table)
	library(xgboost)
	# test data set 1: iris
	X <- within(iris,rm("Species")); Y <- iris[,"Species"]
	X <- within(iris,rm("Species")); Y <- iris[,"Species"]
	model_mat <- model.matrix(~. -1, data=X)
	xgb <- xgboost(model_mat, label = as.numeric(Y) - 1, nrounds = 20, 
		objective = "multi:softprob", num_class = 3 )
	tree_list <- XGB2List(xgb,model_mat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ xgboost }
