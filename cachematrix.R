# Matrix inversion is usually a costly computation and there may be some benefits
# to cache the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to do so.

# makeCacheMatrix creates a list containing :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This Function returns a inverse of a Cached Matrix supposed invertible
## If the inverse is already calculated it gives this inverse wuthout computation
## Otherwise it computes the inverse.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


# A Simple Example
Mat = matrix(c(1,2,3,5), byrow = T, nrow = 2)
CachedMat = makeCacheMatrix(Mat) #Caching Data
cacheSolve(CachedMat) #First Computation 
cacheSolve(CachedMat) #Second Computation gives the cached data



