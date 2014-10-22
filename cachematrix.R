## Store the inverse of a matrix and provides inner functions that 
## allows to manipulate the matrix and its inverse
makeCacheMatrix <- function(m = matrix()) {
  ## initialisation of the inverse
  inv <- NULL
  ## setting the values of the matrix and initializing the inverse
  set <- function(y) {
    m <<- y
    inv <<- NULL  
  }
  ## getting the matrix
  get <- function () m
  ## setting the inverse
  setInverse <- function(inverse) inv <<- inverse
  ## getting the inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns the inverse of matrix if it has not already been computed
cacheSolve <- function(m, ...) {
  # getting the inverse
  inv <- m$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # the inverse is NULL, it needs to be computed
  # getting the matrix
  mat <- m$get()
  # computing the inverse
  inv <- solve(mat)
  # caching the inverse 
  m$setInverse(inv)
  inv  
}