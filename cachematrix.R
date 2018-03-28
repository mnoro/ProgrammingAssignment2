##  Assignment for Week 3 of the Data Scientist Coursera course
##
## Massimo Noro

## makeCacheMatrix allows the creation of an R Matrix object
## That provides methods to get or set Matrixes and their inverse
## The inverse of a Matrix is stored within the environment of the object
## and can be used to optmize calculation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - Using the special Matrix object defined with the previous
## function makeCacheMatrix, calculates the inverse matrix
## In case the object is already available, then retrieves the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() # m is like a state variable defined within the object
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # breaks off, returns
  }
  data <- x$get()
  if(dim(data)[1] != dim(data)[2]){
    message("The Matrix has to be a square")
    return()
  }
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#
# Run the following code lines for examples
##
# examples
##
# set.seed(42)
# mtest <- makeCacheMatrix(matrix(rnorm(16), ncol = 4))
# mtest$get()
# cacheSolve(mtest)
# m2test <- makeCacheMatrix(matrix(rnorm(16), ncol = 8))
# cacheSolve(m2test)
