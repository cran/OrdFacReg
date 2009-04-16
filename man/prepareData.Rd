\name{prepareData}
\alias{prepareData}
\title{Prepare input data to be used in active set algorithm}
\description{This function takes a matrix consisting of quantitative variables, unordered, and ordered factors
and generates the corresponding matrix of dummy variables, and some further quantities that are used
by the active set algorithm in \code{\link{ordFacReg}} and \code{\link{ordFacRegCox}}.
}
\usage{prepareData(Z, fact = NA, ordfact, intercept = TRUE)}
\arguments{
  \item{Z}{Matrix with quantitative variables in the first \eqn{c} columns, unordered factors in the next columns, 
  and finally unordered factors. The latter two need to have levels from \eqn{1} to \eqn{j}.}
  \item{fact}{Specify columns in \eqn{Z} that correspond to unordered factors.}
  \item{ordfact}{Specify columns in \eqn{Z} that correspond to ordered factors.}
  \item{intercept}{If \code{TRUE}, an intercept (= column of all 1's) is added to the design matrix.}
}
\value{Quantities that are used by the active set algorithm. The names of the objects roughly correspond to
those in Rufibach (2009).}
\author{Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@ifspm.uzh.ch} \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\seealso{This function is used by the ordered factor computation functions \code{\link{ordFacReg}} 
and \code{\link{ordFacRegCox}}.}
\references{
Rufibach, K. (2009)
An Active Set Algorithm to Estimate Parameters in Generalized Linear Models with Ordered Predictors.
Preprint, available at \url{http://arxiv.org/abs/0902.0240}.
}

