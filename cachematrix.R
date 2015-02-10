# Example how to use the implemented functions
# 1. Create a simple matrix: m <- matrix(c(1,2,3,4), 2, 2)
# 2. e <- makeCacheMatrix(m)
# 3. r <- cacheSolve(e)
# 4. r
# Output is inverted matrix m:
#      [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# Repeat calc
# r <- cacheSolve(e)
# The call prints a message: getting cached data
# The inverted matrix is read from cache
# Attempt to set the same matrix
# e$set(m)
# The call prints a message: getting cached data. The function checks if the new value is identical to old.
# The cache is preserved in case of identical values
# Change the matrix:
# m  <- matrix(c(5, 6, 7, 9), 2, 2)
# Set a new value:
# e$set(m)
# The cache is invalidated. 
# Call:
# r <- cacheSolve(e)
# A new inverted matrix will be recalculate

# Caches a matrix in the environment different from the current
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  # return a matrix
  get <- function() x
  # set a new matrix and invalidates the cache for previously inverted one
  set  <- function(y) {
    # check if a new matrix is different before invalidating cache
    if(identical(get(),y)) {
      return(message("new matrix is identical to old one")) 
    }
    x <<- y
    cache <<- NULL
  }
  # cache inverted matrix
  set.cache <- function(value) cache <<- value
  # return cached inverted matrix
  get.cache <- function() cache
  # return "special" matrix with all supported functions
  list(set = set, get = get, set.cache = set.cache, get.cache = get.cache)
}


# Checks if the inverted matrix is in cache already before time consuming calculation
cacheSolve <- function(x, ...) {
  data <- x$get.cache()
  if(!is.null(data)) {
    message("getting cached data")
    return(data)
  }
  data <- solve(x$get())
  # Store inverted matrix in cache
  x$set.cache(data)
  # Return an inverted matrix
  data
}
