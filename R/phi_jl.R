phi_jl <-
function (p, f, kj) 
{
    c <- p - f
    kj0 <- c(rep(0, c), kj)
    mjls <- NULL
    for (j in (c + 1):p) {
        for (l in 2:(kj0[j])) {
            h <- (c + 1):j
            i <- sum(kj0[h - 1]) + (l - 1) - (j - c - 1)
            mjls <- rbind(mjls, c(j, l, i))
        }
    }
    dimnames(mjls) <- list(NULL, c("j", "l", "i"))
    return(mjls)
}
