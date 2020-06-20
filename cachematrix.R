##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}


######### Example of aplication

# example <- makeCacheMatrix(matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4),3))

# cacheSolve(example)
#     [,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1

# cacheSolve(example)
#getting cached data
#     [,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1








