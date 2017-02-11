## These are a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object >>
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        invM <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
       
        ## set the value of the inverse matrix
        setinv <- function(solve) invM <<- solve
        
        ## get the value of the inverse matrix
        getinv <- function() invM
        
        ## returns the list of values for cached inverse matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        }


## cacheSolve computes the inverse of the special "matrix" >> 
## returned by makeCacheMatrix above. 

## If the inverse has already been calculated >>
## (and the matrix has not changed), >> 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getinv()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data, ...)
        x$setinv(invM)
        invM
        
}