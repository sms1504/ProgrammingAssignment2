## Here we have a pair of functions which co-operate in the efficient
## management and calculation of matrix inverses.

##  The makeCacheMatrix function creates a special "matrix", which is really
##  a list containing functions to:
##
##  set the value of the matrix.
##  get the value of the matrix.
##  set the value of the inverse matrix.
##  get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
    # Initially, the cache is empty.
    inv <- NULL
    
    # Create the set matrix function.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Create he get matrix function.
    get <- function() x
    
    # Create the set inverse function.
    setinv <- function(solve) inv <<- solve
    
    # Create the get inverse function.
    getinv <- function() inv
    
    # Return the functions we've just made in a list.
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


##  The cacheSolve function calculates the inverse of the special "matrix"
##  created with makeCacheMatrix. However, it first checks to see if the
##  inverse has already been calculated. If so, it gets the inverse from 
##  the cache and skips the computation. Otherwise, it calculates the
##  inverse matrix and sets its value in the cache via the setinv function.
##
cacheSolve <- function(x, ...) {
    
    # Get the inverse matrix from the cache
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # No inverse was found, get the original matrix and calculate its
    # inverse using the solve function.
    data <- x$get()
    inv <- solve(data, ...)
    
    # Save the inverse matrix in the cache.
    x$setinv(inv)
    
    # And return the calculated inverse.
    inv
}
