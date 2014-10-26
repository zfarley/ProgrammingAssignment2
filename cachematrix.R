## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse by 1. Set the value of the matrix. 2. Get the value of the matrix.
## 3. Set the value of the inverse. 4. Set the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y) {
    x<<- y
    m<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}    


