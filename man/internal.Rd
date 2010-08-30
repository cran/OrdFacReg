\name{internal}
\alias{Abeta}
\alias{constraintMats}
\alias{coxDeriv}
\alias{coxLoglik}
\alias{coxSubspace}
\alias{dummy}
\alias{expandBeta}
\alias{indexDummy}
\alias{lmLSE}
\alias{lmSS}
\alias{logRegDeriv}
\alias{logRegLoglik}
\alias{logRegMLE}
\alias{logRegSubspace}
\alias{LSEsubspace}
\alias{maxStep}
\alias{phi_jl}
\alias{setminus}
\alias{shrinkBeta}
\title{Internal functions for ordered factor regression functions}
\description{Internal functions for ordered factor regression functions.}
\details{These functions are not intended to be called by users directly. 
\itemize{

\item{\code{\link{Abeta}}}{Function \eqn{A(\bold{\beta})} in Rufibach (2010) that collects the indices of
the inequalities violated by \eqn{\bold{\beta}}.}

\item{\code{\link{constraintMats}}}{Function that computes the matrices \eqn{\bold{B}} (collects the basis vectors
given in Theorem 3.1 of Duembgen et al. (2007)) and \eqn{\bold{V}} (collects the vectors \eqn{\bold{v}_i} that 
make up the cone \eqn{K} in Section 3.1 of Duembgen et al. (2007)).}

\item{\code{\link{coxDeriv}}}{Computes gradient of (pseudo-)log-likelihood function in Cox-regression.}

\item{\code{\link{coxLoglik}}}{Computes value of (pseudo-)log-likelihood function in Cox-regression.}

\item{\code{\link{coxSubspace}}}{Computes maximizer on subspace, denoted by \eqn{\widetilde{\psi}(A)} in Table 1
of Duembgen et al. (2007).}

\item{\code{\link{dummy}}}{Generate a matrix of dummy variables corresponding to the levels of the inputed factor.
The dummy variable corresponding to the lowest level of the factor is omitted.}

\item{\code{\link{expandBeta}}}{After computation of \eqn{\bold{\beta}} on subspace ``blow up'' this vector again
to original dimension.}

\item{\code{\link{indexDummy}}}{Compute column numbers of the dummy variables of the ordered factor(s).}

\item{\code{\link{lmLSE}}}{Compute value of least squares criterion and least squares estimate.}

\item{\code{\link{lmSS}}}{Compute value of least squares criterion and its gradient.}

\item{\code{\link{logRegDeriv}}}{Gradient of log-likelihood function in logistic regression.}

\item{\code{\link{logRegLoglik}}}{Compute value of log-likelihood function in logistic regression.}

\item{\code{\link{logRegSubspace}}}{Computes maximizer on subspace, denoted by \eqn{\widetilde{\psi}(A)} in 
Table 1 of Duembgen et al. (2007).}

\item{\code{\link{LSEsubspace}}}{Computes maximizer on subspace, denoted by \eqn{\widetilde{\psi}(A)} in 
Table 1 of Duembgen et al. (2007).}

\item{\code{\link{maxStep}}}{Compute maximal permissible steplength, denoted by \eqn{t} in Table 1 in 
Duembgen et al. (2007).}

\item{\code{\link{phi_jl}}}{Function \eqn{\phi} in Rufibach (2010) that maps the original indices \eqn{(i, j)} to
the inequality index \eqn{i}.}

\item{\code{\link{setminus}}}{Remove elements in vector \eqn{B} from vector \eqn{A}.}

\item{\code{\link{shrinkBeta}}}{Collapse \eqn{\bold{\beta}} according to the active constraints specified by the set \eqn{A}.}
}
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
\seealso{All these functions are used by the ordered factor computation functions \code{\link{ordFacReg}} 
and \code{\link{ordFacRegCox}}.}
\keyword{regression}

