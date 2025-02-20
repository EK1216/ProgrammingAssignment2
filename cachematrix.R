## These functions will take a square matrix and cache its inverse

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function inverts the matrix cache'd in the previous function
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#This function integrates the above functions and inverts and caches a matrix in a single function
invert_and_cache <- function (x = matrix()){
  cache<-makeCacheMatrix(x)
  invertedcache <-cacheSolve(cache) 
  return(invertedcache)
}
