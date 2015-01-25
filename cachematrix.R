## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## The following are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m.inv <- NULL
        set <- function(y) {
                x <<- y
                if (!is.null(m.inv)) {
                        ## Check if the matrix has changed ...
                        ## If it is the same matrix, then, the m.inv stays cached
                        ## To check if the matrix has changed --
                        ## Multiply the given matrix with the cached inv -- 
                        ## if the result is not the identity matrix, then the matrix has changed...
                        if (length(which((round(x %*% m.inv, 2) == diag(nrow(x))) == FALSE)) > 0) {
                                message("Matrix has changed")
                                m.inv <<- NULL
                        }
                }
        }
        get <- function() x
        setinv <- function(inv) m.inv <<- inv
        getinv <- function() m.inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m.inv <- x$getinv()
	  m <- x$get()
        ## Check if the inv is cached
        if (!is.null(m.inv)) {
                ## A cached inv is found!
                message("Cached inv exists! Getting cached inv")
                return(m.inv)
        }
        ## No cached inv found. Cache the inv of the given matrix
        message("No cached inv exists. Caching the inv")
        m.inv <- solve(m, ...)
        x$setinv(m.inv)
        m.inv
}
