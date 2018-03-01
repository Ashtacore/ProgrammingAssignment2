## This function creates a special "matrix" object
## that can cache its inverse.

## Start by initializing variables x and i
## x is the matrix and i is the inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Define behavior functions for the "special" matrix object
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## Return a list of all functions to transport enviroment to cacheSolve
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Start by checking if matrix inverse is already cached
        ## If it is, then return cached value
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If the inverse is not in the cache then compute it,
        ## store it in "i", and finally return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
