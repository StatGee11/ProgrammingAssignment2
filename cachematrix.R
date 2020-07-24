####################################################
## Put comments here that give an overall description of what your functions do

## This first function "makeCacheMatrix" makes a special "matrix" which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This second function "cacheSolve" computes the inverse of the special "matrix" returned by the "makeCacheMatrix" function above. If the inverse has already been calculated and the matrix has not changed, then this "cacheSolve" function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     m <- x$getInverse() ## Return a matrix that is the inverse of 'x'
     if(!is.null(m)){
       message("getting cached data")
       return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
####################################################
