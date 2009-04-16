`coxSubspace` <-
function (ttf, tf, Y, A, JJs, q) 
{
    mle <- matrix(coxreg.fit(Y, Surv(ttf, tf), max.survs = length(tf), 
        strats = rep(1, length(tf)))$coefficients, ncol = 1)
    if (length(A) == 0) {
        beta <- mle
    }
    all.act <- identical(unlist(JJs), A)
    if ((length(A) > 0) && (all.act == 1)) {
        beta <- matrix(0, nrow = length(mle))
    }
    if ((length(A) > 0) && (all.act == 0)) {
        mini <- min(unlist(JJs))
        res <- shrinkBeta(Y, A, JJs, q)
        Y.col <- res$Y.col
        sums <- res$sums
        rems <- res$rems
        JJs.A <- res$JJs.A
        beta.col <- matrix(coxreg.fit(Y.col, Surv(ttf, tf), max.survs = length(tf), 
            strats = rep(1, length(tf)))$coefficients, ncol = 1)
        beta <- expandBeta(beta.col, sums, JJs.A)$beta
    }
    return(list(beta = beta))
}
