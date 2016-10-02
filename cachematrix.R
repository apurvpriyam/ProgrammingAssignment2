
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # To get the matrix
  get <- function() x
  
  # TO set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # TO get the inverse
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}
