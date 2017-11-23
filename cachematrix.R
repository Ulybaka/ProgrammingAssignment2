## inverse matrix cashed
## this code contains two fuction: maceCashefunction and casheSolve

## makecasheMatrix function consists of four functions:
## set - function which helps to change the argument matrix x to a new one
## get - function which gets the matrix x from the parent environment, when called
## getsSolve - helps to get inverse matrix from the parrent environment
## setSolve - writes down cashed inverse matrix
## this function returns a list with values,which each of functions return

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function (y) {
    x <<- y
    n <<- NULL
  }
  get <- function () x
  setSolve <- function (solve) n <<- solve
  getSolve <- function () n
  ##the returned list written in this way allows to call values y their names using $ operators
  list (set=set, get=get, setSolve=setSolve,getSolve=getSolve)
}


## this function gets the value of inverse matrix from the cashe if it exists
## or calculates inverse if it does not exist

cacheSolve <- function(x, ...) {
  n <- x$getSolve()
  if (!is.null(n)){
    message ("getting cashed data")
    return (n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setSolve (n)
  n
}
