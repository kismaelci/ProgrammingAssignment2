## these functions looks like functions "makeVector" and "cachemean"
## we must first replace m(mean) with s(solve)
## replace function mean() with function solve()
## replace functions setmean() and getmean() with functions setmatrix() and getmatrix()

## makecacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
}
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}






## cacheSolve calculates the solve of the special “matrix” created with the 
## above function. However, it first checks to see if the solve has already 
## been calculated. If so, it gets the solve from the cache and skips the 
## computation. Otherwise, it calculates the solve of the data and sets the 
## value of the solve in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
   s <- x$getsolve()
   if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

