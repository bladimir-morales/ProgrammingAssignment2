# Se implementa el código de la función makeCacheMatrix que almacena la información en cache
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

# En esta función se calcula el inverso de la matriz y se utiliza para el cache la matriz makeCacheMatrix
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

#Ejemplo
m<-matrix(1:4,2,2)
cacheSolve(makeCacheMatrix(m))
solve(m)
