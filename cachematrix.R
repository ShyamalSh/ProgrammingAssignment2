## The following functions calculates the inverse of a matrix and
# stores its value as cache so that it can be reused again and again
# without calculating it from scratch. Thus improving the efficiency 
# of the program.

## The first functions basically stores a matrix and catches its mean.
# It is a list containing a function to:
# 1. Set the value of matrix. 2. Get the value of matrix.
# 3. Set the value of inverse. 4. Get the value of inverse.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function () m
    list(set = set, get = get,
         setonverse = setinverse,
         getinverse = getinverse)
}
## This function calculates the inverse of the matrix created with
# the above function. It first checks if the inverse is already 
# calculated. If it is calculated before, it returns the same value
# without calculating it again. If it is not calculated, it first 
# calculates the inverse, stores it in set cache through the setinverse
# function and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
         m <- x$getinverse()
         if(!is.null(m)) {
             message("getting cached data")
             return(m)
         }
         mat <- x$get()
         m <- solve(mat, ...)
         x$setinverse(m)
         m
}
