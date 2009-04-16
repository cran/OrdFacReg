`LSEsubspace` <-
function (D, Y, A, JJs, q) 
{
    lse <- lmLSE(D, Y)$beta
    if (length(A) == 0) {
        beta <- lse
    }
    all.act <- identical(unlist(JJs), A)
    if ((length(A) > 0) && (all.act == 1)) {
        beta <- matrix(0, nrow = length(lse))
    }
    if ((length(A) > 0) && (all.act == 0)) {
        res <- shrinkBeta(Y, A, JJs, q)
        Y.col <- res$Y.col
        sums <- res$sums
        rems <- res$rems
        JJs.A <- res$JJs.A
        mat <- t(Y.col) %*% Y.col
        beta.col <- ginv(mat) %*% t(Y.col) %*% D
        beta <- expandBeta(beta.col, sums, JJs.A)$beta
    }
    return(list(beta = beta))
}
