## R Programming assignment # 2 by M. Chitnis
## April 22, 2015
## Functions for Matrix Inversion and Caching the inversion for future use

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set value of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of matrix
  get <- function() x
  
  ## set the inverse of the matrix 
  setmatrix <- function(solve) m <<- solve
  
  ## get the inverse of the matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache. 

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  ## check if inverse exists in cache
  if(!is.null(m)) {
    message("getting cached data")
    ## return the inverse from cache, no need to calculate the invserse
    return(m)
  }
  
  ## Inverse does not exist in the cache
  matrix <- x$get()
  ## Calculate inverse using solve function
  m <- solve(matrix, ...)
  ## Cache the inverse for future use 
  x$setmatrix(m)
  ## Return the computed inverse
  m
}
