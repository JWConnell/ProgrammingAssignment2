##The assignment is to write a pair of functions
##that cache the inverse of a matrix

##Put comments here that give an overall description of what your functions do
##This file contains two functions, makeCacheMatrix() and cacheSolve().  The first
##function creates an object that stores a matrix and its inverse.  The second
##requires an argument returned by makeCacheMatrix() in order to retrieve
##the matrix inverse stored in that object's environment. 


## Write a short comment describing this function

##makeCacheMatrix creates two data objects (a matrix and its inverse) and four functions
##step 1: initialize data objects, x as a function argument, minv is set to NULL
##step 2: define the functions, set and get for the matrix,
##        setinv and getinv for the inverse.

makeCacheMatrix <- function(x = matrix()) {

    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
    ##Starts with the matrix (x) as a single function argument
    ##(and an ellipsis for additional arguments).
    ##It uses getinv() to check the cache for a result.  If the value is not NULL
    ##it is retrieved; if this is NULL, get() retrieves (x) to compute the inverse.
    ##setinv() then returns the result to the parent environment.
    ##Thus, cacheSolve() completes makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)){
       message("getting cached data")
       return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setinv(minv)
    minv      
}

##This code is a test of makeCacheMatrix() 
mtrx <- makeCacheMatrix()
mtrx$set(matrix(rnorm(4*4),4))
mtrx$get()

cacheSolve(mtrx)
cacheSolve(mtrx)


