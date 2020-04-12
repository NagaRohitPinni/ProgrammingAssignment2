## Put comments here that give an overall description of what your
## functions do

#write 2 functions - 1.Cache matrix 2.fetch the inverse from cache if not compute

## Write a short comment describing this function
## Objective is cache inverse of an input matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(matrixinv) i <<- matrixinv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## function to check and fetch if inverse is already cached if not compute inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("fetching cached inverse")
    return(i)
  }
  inputmatrix <- x$get()
  i <- solve(inputmatrix, ...)
  x$setinv(i)
  i
}
