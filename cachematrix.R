## Two functions are available to perform matrix operations
## makeCacheMatrix calculate the inverse of matrix and retains 
## the matrix object as well
## cacheSolve gets the inverse matrix from the cache or computes it
## and stores it in makeCacheMatrix object

## function to store a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setInverse,
       getinverse = getInverse)  
}


## function to retrieve inverse of matrix through cache/computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached inverse matrix")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data)
  x$setinverse(mat)
  mat  
}
