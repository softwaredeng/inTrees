\name{Ranger2List}
\alias{Ranger2List}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform a ranger object to a list of trees
}
\description{
Transform a ranger object to a list of trees
}
\usage{
Ranger2List(rf_ranger)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rf_ranger}{
%%     ~~Describe \code{rf} here~~
ranger object
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
a list of trees 
}
% \references{
% Houtao Deng, Interpreting Tree Ensembles with Intrees, technical report, 2014
% }
% \author{
% Houtao Deng
% }
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Ranger2List}}
}
\examples{
library(ranger)
data(iris)
rf_ranger <- ranger(Species ~ ., data = iris)
tree_list <- Ranger2List(rf_ranger)
X <- iris[,1:(ncol(iris)-1)]
ruleExec <- extractRules(tree_list,X) # transform to R-executable rules
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ranger }
