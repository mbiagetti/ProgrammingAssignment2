##  Assignment: Caching the Inverse of a Matrix


##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## this variable is the value of the cached function
        m <- NULL
        ## this function return  the value of the matrix
        get <- function() x
        ## this function set the cached value of the inverse of the matrix 
        setsolve <- function(solve) m <<- solve
        ## this function return  the cached value of the matrix
        getsolve <- function() m
        ## return the functions
        list(get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        # compute the inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        # put the result in cache
        x$setsolve(m)
        m
}


# Usage Example
## Create a matrix
##> simple_matrix <- cbind(c(1,2,3),c(0,4,5),c(1,0,6))
## Create a cacheable matrix
##> function_matrix = makeCacheMatrix(simple_matrix)
## Get the inverse of the matrix (first time cache miss)
##> cacheSolve(function_matrix)
##            [,1]       [,2]        [,3]
##[1,]  1.09090909  0.2272727 -0.18181818
##[2,] -0.54545455  0.1363636  0.09090909
##[3,] -0.09090909 -0.2272727  0.18181818
## Get the inverse of the matrix (second time cache hit!)
##> cacheSolve(function_matrix)
## getting cached data
##            [,1]       [,2]        [,3]
##[1,]  1.09090909  0.2272727 -0.18181818
##[2,] -0.54545455  0.1363636  0.09090909
##[3,] -0.09090909 -0.2272727  0.18181818

