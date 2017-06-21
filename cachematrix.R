## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to

#   1.  set the value to Matrix
#   2.  get the value of Matrix
#   3.  set the value of the Inverse of Matrix
#   4.  get the value of the Inverse of Matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }     
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the Inverse of the special "vector"
#created with the above function. However, it first checks to see if the
#Matrix has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the inverse in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
