## Programming Assignment 2 - R Programming

# The first function called "makeCacheMatrix" creates a special 'matrix' object 
# that can cache its inverse. The rationale of doing this is that matrix inversion is
# a costly computation, so that it is computationally beneficial to cache the inverse 
# of a matrix rather than compute it several times.

# The structure of The first function called "makeCacheMatrix" follows the lines of
# the assignment example dealin with the mean of a vector. More specifically, we have
# four functions encapsulated in a list:
# - set the matrix
# - get the matrix
# - set the inverse of the matrix
# - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This  second function computes the inverse of the special "matrix" created by 
# makeCacheMatrix function written above. If it founds that the inverse has 
# already been computed, then the inverse is taken from the cache. 
# Otherwise, the matrix inverse is computed by using the "solve" function in R.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}


# A simple (commented) example is reported below:
#
#my_matrix <- makeCacheMatrix(matrix(c(1, 2, 1, 3), 2, 2))
#my_matrix$get()
#my_matrix$getInverse()
#cacheSolve(my_matrix)
#cacheSolve(my_matrix)