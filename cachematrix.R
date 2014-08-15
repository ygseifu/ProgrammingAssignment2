## Matrix inversion is usually a costly computation and there may be some benefit  
## to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then the cache solve retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
