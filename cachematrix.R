## My functions calculate the inverse of a matrix with cache.

## This function makes a special matrix object that can cashe its inverse.

makeCacheMatrix <- function(x = matrix()) {
  stored_inv <- NULL
  set <- function (y) {
    x <<- y
    stored_inv <<- NULL
  }
  get <- function () {
    return (x)
  }
  set_inv <- function (rep_inv) {
    stored_inv <<- rep_inv
  }
  get_inv <- function () {
    return (stored_inv)
  }
  list (set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}

## This function returns the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  local_inv <- x$get_inv()
  
  if (!is.null(local_inv)) {
    message("getting cached data")
    return (local_inv) # return the inverse from the cache
  }
  else {
    local_mat <- x$get()
    local_inv <- solve(local_mat,...)
    x$set_inv(local_inv) # set the inverse into the cache
    return (local_inv) # return the inverse after calculation
  }
}
