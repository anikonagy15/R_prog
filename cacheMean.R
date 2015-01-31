cachemean <- function(x, ...) {
        t <- x$getmean()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        t <- mean(data, ...)
        x$setmean(t)
        t
}
