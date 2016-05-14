##This function creates a special "matrix" object that can cache its inverse
##
##The first function, makeVector creates a special "vector", which is really a list containing a function to
##
## ==> set the value of the matrix
## ==> get the value of the matrix
## ==> set the value of the inversed  matrix
## ==> get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
 set <- function(y) {
  x <<- y
  i <<- NULL
   }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##inverse the matrix with the @solve function@

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
    if (!is.null(i)) {
      message("getting the cached data")
    return(i)
  }
    mat <- x$get()
    i <- solve(mat, ...)
    x$setInverse(i)
    i
}