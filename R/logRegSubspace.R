`logRegSubspace` <-
function (D, Y, A, JJs, q) 
{
    mle <- matrix(glm.fit(Y, D, family = binomial(link = logit))$coefficients, 
        ncol = 1)
    if (length(A) == 0) {
        beta <- mle
    }
    all.act <- identical(unlist(JJs), A)
    if ((length(A) > 0) && (all.act == 1)) {
        beta <- matrix(0, nrow = length(mle))
    }
    if ((length(A) > 0) && (all.act == 0)) {
        res <- shrinkBeta(Y, A, JJs, q)
        Y.col <- res$Y.col
        sums <- res$sums
        rems <- res$rems
        JJs.A <- res$JJs.A
        beta.col <- matrix(glm.fit(Y.col, D, family = binomial(link = logit))$coefficients, 
            ncol = 1)
        beta <- expandBeta(beta.col, sums, JJs.A)$beta
    }
    return(list(beta = beta))
}
