## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing four functions to
# set the value of the matrix
# get the value of the matrix
# set the value of the invers of the matrix
# get the value of the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
      xi <- NULL
      set <- function(y) {
            x <<- y
            xi <<- NULL
      }
      get <- function() x
      set_inverse <- function(solve) xi <<- solve
      get_inverse <- function() xi
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## This function calculates the inverse of the matrix by
# first checking if the inverse has already been calculated
# if the inverse has   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      xi <- x$get_inverse()
      if(!is.null(xi)) {
            message("getting cached data")
            return(xi)
      }
      data <- x$get()
      xi <- solve(data, ...)
      x$set_inverse(xi)
      xi
}
