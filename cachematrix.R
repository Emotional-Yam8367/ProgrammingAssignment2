## Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation.
# Write functions to cache the value of matrix inverses so that when we need it again, 
# it can be looked up in the cache rather than recomputed

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# Input is a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
# Otherwise, it calculates the inverse and sets the value of the inverse in the cache via the setinv function.
# Input is a "matrix" object created using makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# Example
# A = matrix(1:4,2,2)
# B = matrix(c(-1:-7,23,12),3,3)
# solve(A) # view inverse of A
# M1 = makeCacheMatrix(A)
# cacheSolve(M1)
# M2 = makeCacheMatrix(B)
# cacheSolve(M2) == solve(B)
# cacheSolve(M2) # get cached data
# cacheSolve(M1) # get cached data
