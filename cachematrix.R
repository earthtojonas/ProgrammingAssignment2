##makeCacheMatrix creates a cached matrix and space for its corresponding inverse.
##The inverse is either cached or solved for using the cacheSolve function.  The cacheSolve
##function works by either returning a cached value for the matrix, or solving and caching for
##values that are not already cached. The purpose of these functions are to save computational time
## and memory.


makeCacheMatrix <- function(x = matrix()) {##Creates a matrix and its inverse to be cached
  inv <- NULL  
  set <- function(y) 
    x <<- y
    inv <<- NULL
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function()inv
    list(
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


##Returns cached value of inverse if previously solved. If not previously cached,  
##the function solves for the inverse, caches it, and returns it. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat, ...)
  x$setinv(inv)
  inv
}


