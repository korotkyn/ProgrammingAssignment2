## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          ##set is a function that changes the matrix stored in the main function
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          ##get is a function that returns the matrix x stored in the main function
          get <- function() x
          ##setsolve and getsolve are functions very similar to set and get. 
          ##They don't calculate the solve, they simply store the value of the input 
          ##in a variable m into the main function makeCacheMatrix (setsolve) and return it (getsolve).
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          
          ##To store the 4 functions in the function makeCacheMatrix, we need the function list(), 
          ##so that when we assign makeCacheMatrix to an object, the object has all the 4 functions.
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}



## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## Checking if Matrix is already in cache
        if(!is.null(m)) {
          message("getting cached matrix")
          return(m)
        }
        ## If not - initialize the new one and make inversion
        data <- x$get()
        ## Making inversion
        m <- solve(data, ...)
        ## Store inversion in cache
        x$setsolve(m)
        m
}
