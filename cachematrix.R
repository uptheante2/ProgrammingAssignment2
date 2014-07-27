## The function makeCacheMatrix takes in a matrix 'x' and generates the funciton calls to cache the variable 
## values when called.

## The code used is modified from the template vector example.  The output of makeCacheMatrix generates the 
## function calls in a list format to be used in the cacheSolve function.  The 'set' varlables are cache 
## and the 'get' variables are not.

makeCacheMatrix <- function(x = matrix()) {
        

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function evaluates the functions set up in makeCacheMatrix and returns either the cached value or
## the newly solved value.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
