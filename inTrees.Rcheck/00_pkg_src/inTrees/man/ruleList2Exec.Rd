\name{ruleList2Exec}
\alias{ruleList2Exec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal
}
\description{
internal
}
\usage{
ruleList2Exec(X, allRulesList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{allRulesList}{
%%     ~~Describe \code{allRulesList} here~~
}
}
\value{
	data frame containing rule conditions
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, allRulesList) 
{
    typeX = getTypeX(X)
    ruleExec <- unique(t(sapply(allRulesList, singleRuleList2Exec, 
        typeX = typeX)))
    ruleExec <- t(ruleExec)
    colnames(ruleExec) <- "condition"
    return(ruleExec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
