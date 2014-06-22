# A pair of functions to set up variables for persisting state, and logic to determine
# whether the state has changed since the data was loaded. 

## makeCacheMatrix() - a function that accepts a matrix and contains four functions to cache
## and retrieve the inverse of the passed matrix. 

makeCacheMatrix <- function(x = matrix()) {
   # variable to store cached solve value:
   m <- NULL
   # create x setter function
   set <- function(y) {
      # sets x in parent environment
      x <<- y
      # nulls out m in case cached value exists
      m <<- NULL
   }
   # one-liner - returns current state of x:
   get <- function() x
   # receiving setter function for call from cacheSolve:
   setsolve <- function(solve) m <<- solve
   # returns value of m set by setSolve if it exists:
   getsolve <- function() m
   # attaches functions to object accessible via obj$functionName call
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

# helper function to retrieve value of a makeCacheMatrix object's inverse if it has already been computed and stored.
# otherwise it calculates the solve(), stores it via makeCacheMatrix's $setsolve() function, 
# and returns it to the function caller.
cacheSolve <- function(x, ...) {
   #call original object's getter function for current value of solve
   m <- x$getsolve()
   # if it's already set, return it
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   # else compute the solve for x and return it
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
}
