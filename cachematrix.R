## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation, 
# so these two functions are designed for storing a square matrix and caching its inverse to make your program more efficient. 

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inverse
#    get the value of the inverse
    
makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(matrixinverse) matrix <<- matrixinverse
        getinverse <- function() matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# The following function calculates the inverse of a matrix, using the special "vector" created with the above function。
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inversion")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
