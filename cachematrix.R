## this function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv_mat  <<- NULL
      
      set <- function(y) {
          x <<- y
          inv_mat <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) inv_mat <<- solve
        getmatrix <- function() inv_mat
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
      }



}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_mat <- x$getmatrix()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setmatrix(inv_mat)
  inv_mat
}



### example
#x = rbind(c(1, -1/4), c(-1/4, 1))
#> m = makeCacheMatrix(x)
#> cacheSolve(m)
#          [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
