## The two functions wotk together to avoid doing repeating computations. Whenever you try to invert a Matrix,
##cacheSolve will check if the result is already available through getSolve in makeCacheMatrix. If this is not posible,
##then cacheSolve will effectively do the computations and update the result through setSolve (defined in mCM)

## The function defines a set of auxiliary functions and a vector of cached matrices and its inverses (if already computed)

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
            x <<- y
            minv <<- NULL
        }
        get <- function() x
        setminv <- function(minv_set) minv <<- minv_set
        getminv <- function() minv
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## The function returns the inverted matrix. It will avoid computing the result if already available in the cache,
## otherwise it inverts the matrix

cacheSolve <- function(x=matrix()) {
    minv<-x$getminv()
    if(!is.null(minv)){
        message("Retrieving cached data")
        return(minv)
    }else{
        message("Inverting matrix and caching results")
        data<-x$get()
        x$setminv(solve(data))
        minv<-x$getminv()
        minv
    }
}
