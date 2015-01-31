makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function(){
                print("test")
                print(x)
        } 
        setmean <- function(k) {
                m <<- k      
        }
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}