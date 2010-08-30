\name{OrdFacReg-package}
\alias{OrdFacReg-package}
\alias{OrdFacReg}
\concept{ordered factor}
\concept{ordered predictor}
\concept{ordered explanatory variable}
\docType{package}
\title{Least squares, logistic, and Cox-regression with ordered predictors}
\description{In biomedical studies, researchers are often interested in assessing the association between one or more
ordinal explanatory variables and an outcome variable, at the same time adjusting for covariates of any type.
The outcome variable may be continuous, binary, or represent censored survival times. In the absence of a precise
knowledge of the response function, using monotonicity constraints on the ordinal variables improves efficiency in
estimating parameters, especially when sample sizes are small. This package implements an active set algorithm
that efficiently computes such estimators.}
\details{
\tabular{ll}{
Package:  \tab OrdFacReg  \cr
Type:     \tab Package    \cr
Version:  \tab 1.0.2      \cr
Date:     \tab 2010-08-30 \cr
License:  \tab GPL (>=2)  \cr
LazyLoad: \tab yes        \cr
}
Use this package to get estimates in least squares, logistic, or Cox-regression where coefficients corresponding to
dummy variables of ordered factors are estimated to be in non-decreasing order and at least 0.
The package offers an active set algorithm implemented in the functions \code{\link{ordFacReg}} for least squares
and logistic regression and \code{\link{ordFacRegCox}} for Cox-regression. 
}
\references{
Rufibach, K. (2010).
An Active Set Algorithm to Estimate Parameters in Generalized Linear Models with Ordered Predictors.
\emph{Comput. Statist. Data Anal.}, \bold{54}, 1442-1456.
}
\author{Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@ifspm.uzh.ch} \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\seealso{Examples are given in the help files of the functions \code{\link{ordFacReg}} and \code{\link{ordFacRegCox}}.}
\keyword{regression}

