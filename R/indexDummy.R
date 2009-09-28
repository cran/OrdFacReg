indexDummy <-
function (kj, f, p) 
{
    c <- p - f
    JJs <- list()
    JJs[[1]] <- c + 1:(kj[1] - 1)
    if (f > 1) {
        for (j in 2:f) {
            JJs[[j]] <- max(JJs[[j - 1]]) + 1:(kj[j] - 1)
        }
    }
    return(JJs)
}
