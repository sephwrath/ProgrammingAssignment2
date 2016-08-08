## Two functions for helping to manage caching of inverse matrixes
## can be used as follows -
## > test <- matrix(c(2,3,2,2), 2,2)
## > mtr<-makeCacheMatrix(test)
## > cacheSolv(mtr)
## [,1] [,2]
## [1,] -1.0    1
## [2,]  1.5   -1
## > cacheSolv(mtr)
## getting cached data
## [,1] [,2]
## [1,] -1.0    1
## [2,]  1.5   -1

## This function creates a special "matrix" object that can cache its inverse.
## Inputs: x is a matrix
## Outputs: a list of four functions used to get and set the matrix as well as
## the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv <- function(inv) m <<- inv
      getInv <- function() m
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function takes matrix objects created by the makeCacheMatrix function
## and returns the inverse matrix. If the inverse has been calcualted it returns
## the cached version otherwise it recalcuates the inverse, caches it and returns
## it
## Inputs: only accepts lists created by makeCacheMatrix that contain invertable
## matrix objects
## Outputs: the inverted matrix object
cacheSolve <- function(x, ...) {
      m <- x$getInv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setInv(m)
      m
}
