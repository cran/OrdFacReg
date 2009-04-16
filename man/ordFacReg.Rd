\name{ordFacReg}
\alias{ordFacReg}
\title{Compute least squares or logistic regression for ordered predictors}
\description{
This function computes estimates in least squares or logistic regression where coefficients corresponding to
dummy variables of ordered factors are estimated to be in non-decreasing order and at least 0.
An active set algorithm as described in Duembgen et al. (2007) is used.
}
\usage{ordFacReg(D, Z, fact, ordfact, type = c("LS", "logreg"), 
    intercept = TRUE, display = 0, eps = 0)}
\arguments{
  \item{D}{Response vector, either in \eqn{R^n} (least squares) or in \eqn{\{0, 1\}^n} (logistic).}
  \item{Z}{Matrix of predictors. Factors are coded with levels from 1 to \eqn{j}.}
  \item{fact}{Specify columns in \eqn{Z} that correspond to unordered factors.}
  \item{ordfact}{Specify columns in \eqn{Z} that correspond to ordered factors.}
  \item{type}{Specify type of response variable.}
  \item{intercept}{If \code{TRUE}, an intercept (= column of all 1's) is added to the design matrix.}
  \item{display}{If \code{display == 1} progress of the algorithm is output.}
  \item{eps}{Quantity to which the criterion in the Basic Procedure 2 in Duembgen et al. (2007) is compared.}
}
\details{For a detailed description of the problem and the algorithm we refer to Rufibach (2009).}
\value{
  \item{L}{Value of the criterion function at the maximum.}
  \item{beta}{Computed regression coefficients.}
  \item{A}{Set \eqn{A} of active constraints.}
  \item{design.matrix}{Design matrix that was generated.}
}
\references{
Duembgen, L., Huesler, A. and Rufibach, K. (2007)
Active set and EM algorithms for log-concave densities based on complete and censored data.
Technical report 61, IMSV, Univ. of Bern, available at \url{http://arxiv.org/abs/0707.4643}.

Rufibach, K. (2009)
An Active Set Algorithm to Estimate Parameters in Generalized Linear Models with Ordered Predictors.
Preprint, available at \url{http://arxiv.org/abs/0902.0240}.
}
\author{Kaspar Rufibach (maintainer) \cr \email{kaspar.rufibach@ifspm.uzh.ch} \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\seealso{\code{\link{ordFacRegCox}} computes estimates for Cox-regression.}
\examples{

## ========================================================
## To illustrate least squares estimation, we generate the same data
## that was used in Rufibach (2009), Table 1.
## ========================================================

## --------------------------------------------------------
## initialization
## --------------------------------------------------------
n <- 200
Z <- NULL
intercept <- FALSE

## --------------------------------------------------------
## quantitative variables
## --------------------------------------------------------
n.q <- 3
set.seed(14012009)
if (n.q > 0){for (i in 1:n.q){Z <- cbind(Z, rnorm(n, mean = 1, sd = 2))}}

## --------------------------------------------------------
## unordered factors
## --------------------------------------------------------
un.levels <- 3
for (i in 1:length(un.levels)){Z <- cbind(Z, sample(rep(1:un.levels[i], 
    each = ceiling(n / un.levels)))[1:n])}
fact <- n.q + 1:length(un.levels)

## --------------------------------------------------------
## ordered factors
## --------------------------------------------------------
levels <- 8
for (i in 1:length(un.levels)){Z <- cbind(Z, sample(rep(1:levels[i], 
    each = ceiling(n / levels)))[1:n])}
ordfact <- n.q + length(un.levels) + 1:length(levels)

## --------------------------------------------------------
## generate data matrices
## --------------------------------------------------------
Y <- prepareData(Z, fact, ordfact, intercept)$Y

## --------------------------------------------------------
## generate response
## --------------------------------------------------------
D <- apply(Y * matrix(c(rep(c(2, -3, 0), each = n), rep(c(1, 1), each = n), 
    rep(c(0, 2, 2, 2, 2, 5, 5), each = n)), ncol = ncol(Y)), 1, sum) + 
    rnorm(n, mean = 0, sd = 4)

## --------------------------------------------------------
## compute estimates
## --------------------------------------------------------
res1 <- lmLSE(D, Y)
res2 <- ordFacReg(D, Z, fact, ordfact, type = "LS", intercept, 
    display = 1, eps = 0)
b1 <- res1$beta
g1 <- lmSS(b1, D, Y)$dL
b2 <- res2$beta
g2 <- lmSS(b2, D, Y)$dL
Ls <- c(lmSS(b1, D, Y)$L, lmSS(b2, D, Y)$L)
names(Ls) <- c("LSE", "ordFact") 
disp <- cbind(1:length(b1), round(cbind(b1, g1, cumsum(g1)), 4), 
    round(cbind(b2, g2, cumsum(g2)), 4))

## --------------------------------------------------------
## display results
## --------------------------------------------------------
disp
Ls

## ========================================================
## Artificial data is used to illustrate logistic regression.
## ========================================================

## --------------------------------------------------------
## initialization
## --------------------------------------------------------
set.seed(23041977)
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
un.levels <- c(8, 2)
for (i in 1:length(un.levels)){Z <- cbind(Z, sample(round(runif(n, 0, 
    un.levels[i] - 1)) + 1))}
fact <- n.q + 1:length(un.levels)

## --------------------------------------------------------
## ordered factors
## --------------------------------------------------------
levels <- c(2, 4, 10)
for (i in 1:length(levels)){Z <- cbind(Z, sample(round(runif(n, 0, 
    levels[i] - 1)) + 1))}
ordfact <- n.q + length(un.levels) + 1:length(levels)

## --------------------------------------------------------
## generate response
## --------------------------------------------------------
D <- sample(c(rep(0, n / 2), rep(1, n/2)))

## --------------------------------------------------------
## generate design matrix
## --------------------------------------------------------
Y <- prepareData(Z, fact, ordfact, intercept)$Y

## --------------------------------------------------------
## compute estimates
## --------------------------------------------------------
res1 <- matrix(glm.fit(Y, D, family = binomial(link = logit))$coefficients, ncol = 1)
res2 <- ordFacReg(D, Z, fact, ordfact, type = "logreg", intercept = intercept, 
    display = 1, eps = 0)
b1 <- res1
g1 <- logRegDeriv(b1, D, Y)$dL
b2 <- res2$beta
g2 <- logRegDeriv(b2, D, Y)$dL
Ls <- unlist(c(logRegLoglik(res1, D, Y), res2$L))
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


