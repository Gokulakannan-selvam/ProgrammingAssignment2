# Matrix inversion is usually a costly computation and there may be some benefit
# to reduce the such intense efforts by using the following commands

# makeCacheMatrix creates a list containing a function and sets, gives the matrix and sets and gives the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. 

# We assumed that the matrix is invertible

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
    if(!is.null(inv)) {  # checking whether it is null or not
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv  # Returns the inverse of matrix
}
