makeCacheMatrix <- function (x = matrix ()) {
  invrs <- NULL
    set <- function(y) {
      x <<- y
      invrs <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse    
  getinverse <- function() invrs
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("Getting cached data.")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data)
  x$setinverse(inverse_x)
  inverse_x
}
