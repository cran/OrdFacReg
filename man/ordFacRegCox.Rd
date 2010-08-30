\name{ordFacRegCox}
\alias{ordFacRegCox}
\title{Compute Cox-regression for ordered predictors}
\description{
This function computes estimates in Cox-regression where coefficients corresponding to
dummy variables of ordered factors are estimated to be in non-decreasing order and at least 0.
An active set algorithm as described in Duembgen et al. (2007) is used.
}
\usage{ordFacRegCox(ttf, tf, Z, fact, ordfact, ordering = NA, intercept = TRUE, 
    display = 0, eps = 0)}
\arguments{
  \item{ttf}{Survival times.}
  \item{tf}{Censoring indicator (1 = event, 0 = censored).}
  \item{Z}{Matrix of predictors. Factors are coded with levels from 1 to \eqn{j}.}
  \item{fact}{Specify columns in \eqn{Z} that correspond to unordered factors.}
  \item{ordfact}{Specify columns in \eqn{Z} that correspond to ordered factors.}
  \item{ordering}{Vector of the same length as \code{ordfact}. Specifies ordering of ordered factors: \code{"i"} means
  that the coefficients of the corresponding ordered factor are estimated in non-decreasing order and \code{"d"} means
  non-increasing order. See the examples in \code{ordFacReg} for details.}  
  \item{intercept}{If \code{TRUE}, an intercept (= column of all 1's) is added to the design matrix.}
  \item{display}{If \code{display == 1} progress of the algorithm is output.}
  \item{eps}{Quantity to which the criterion in the Basic Procedure 2 in Duembgen et al. (2007) is compared.}
}
\details{For a detailed description of the problem and the algorithm we refer to Rufibach (2010).}
\value{
  \item{L}{Value of the criterion function at the maximum.}
  \item{beta}{Computed regression coefficients.}
  \item{A}{Set \eqn{A} of active constraints.}
  \item{design.matrix}{Design matrix that was generated.}
}
\references{
Duembgen, L., Huesler, A. and Rufibach, K. (2010).
Active set and EM algorithms for log-concave densities based on complete and censored data.
Technical report 61, IMSV, Univ. of Bern, available at \url{http://arxiv.org/abs/0707.4643}.

Rufibach, K. (2010).
An Active Set Algorithm to Estimate Parameters in Generalized Linear Models with Ordered Predictors.
\emph{Comput. Statist. Data Anal.}, \bold{54}, 1442-1456.
}
\author{Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@ifspm.uzh.ch} \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\seealso{\code{\link{ordFacReg}} computes estimates for least squares and logistic regression.}
\examples{

## ========================================================
## Artificial data is used to illustrate Cox-regression.
## ========================================================

## --------------------------------------------------------
## initialization
## --------------------------------------------------------
set.seed(1977)
n <- 500
Z <- NULL
intercept <- FALSE

## --------------------------------------------------------
## quantitative variables
## --------------------------------------------------------
n.q <- 2
if (n.q > 0){for (i in 1:n.q){Z <- cbind(Z, rnorm(n, rgamma(2, 2, 1)))}}

## --------------------------------------------------------
## unordered factors
## --------------------------------------------------------
un.levels <- c(8, 2)[2]
for (i in 1:length(un.levels)){Z <- cbind(Z, sample(round(runif(n, 0, 
    un.levels[i] - 1)) + 1))}
fact <- n.q + 1:length(un.levels)

## --------------------------------------------------------
## ordered factors
## --------------------------------------------------------
levels <- c(4, 5, 10)
for (i in 1:length(levels)){Z <- cbind(Z, sample(round(runif(n, 0, 
    levels[i] - 1)) + 1))}
ordfact <- n.q + length(un.levels) + 1:length(levels)

## --------------------------------------------------------
## generate response
## --------------------------------------------------------
ttf <- rexp(n)
tf <- round(runif(n))

## --------------------------------------------------------
## generate design matrix
## --------------------------------------------------------
Y <- prepareData(Z, fact, ordfact, ordering = NA, intercept)$Y

## --------------------------------------------------------
## compute estimates
## --------------------------------------------------------
res1 <- coxreg.fit(Y, Surv(ttf, tf), max.survs = length(tf), 
    strats = rep(1, length(tf)))$coefficients
res2 <- ordFacRegCox(ttf, tf, Z, fact, ordfact, ordering = NA, 
    intercept = intercept, display = 1, eps = 0)
b1 <- matrix(res1, ncol = 1)
g1 <- coxDeriv(b1, ttf, tf, Y)$dL
b2 <- res2$beta
g2 <- coxDeriv(b2, ttf, tf, Y)$dL
Ls <- c(coxLoglik(b1, ttf, tf, Y)$L, res2$L)
names(Ls) <- c("MLE", "ordFact") 
disp <- cbind(1:length(b1), round(cbind(b1, g1, cumsum(g1)), 4), 
    round(cbind(b2, g2, cumsum(g2)), 4))

## --------------------------------------------------------
## display results
## --------------------------------------------------------
disp
Ls
}
\keyword{regression}

