# Matrix inversion is a potentially very time consuming computation calculation 
# (especially when large datasets or looping are involved)
# therefore caching the process can speed up the calculation.
# The function belows do that.

# The makeCacheMatrix function a list that:
# 1. sets the matrix value
# 2. gets the matrix value
# 3. sets the inverse of the matrix value
# 4. gets the inverse of the matrix value

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The function first checks if the inverse has already been performed. 
# If not, it computes the inverse and sets the value in the cache via setinverse function
# If yes, it skips the computation and goes directly to the result. 

# The function assumes that the matrix is always invertible.

CacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  
  data <- x$get()
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
}
