## Coursera - R Programming
## Programming Assessment 2
## written by Dmytro Kovalchuk <kdmytro@gmail.com>
##

## makeCacheMatrix creates a "cache-able" matrix from its argument x, assigns getters and setters methods 

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    
    set <- function (y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    
    ## saving "value" to cache 's'
    setSolve <- function(value) s <<- value
    
    ## return cached value '"'s'
    getSolve <- function() s
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve function returns a chached inverse of matrix 'x' 

cacheSolve <- function(x, ...) {
    ## get cached value
    s <- x$getSolve();
    
    ## return cached value if exists
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    
    ## solve for 's'
    data <- x$get()
    s <- solve(data, ...)
    
    ## save 's' to cache
    x$setSolve(s)
    
    s
}
