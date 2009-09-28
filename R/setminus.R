setminus <-
function (A, B) 
{
    res <- A[((A %in% B) == FALSE)]
    return(res)
}
