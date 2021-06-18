## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "vector" which
## really a list containing function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
#3 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y=matrix()){
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)

}


##  The following function compute the inversion of the matrix created
## with the above function. However, it first checks to see if the matrix
## inversion has already been computed. If yes, it gets the matrix inversion
## from the cache and skips the computation. Otherwise, it calculates the 
## matrix inversion and set the matrix inversion via the setinv function

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
    }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}



