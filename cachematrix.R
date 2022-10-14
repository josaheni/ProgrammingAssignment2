## Caching inverted matrix data to prevent repeated computations 

## takes matrix and cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  get <- function(){
    x
  } 
  setInv <- function(inverse){
    m <<- inverse
  }
  getInv <- function(){
    m
  } 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## compute inverse of makeCacheMatrix unless already computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}