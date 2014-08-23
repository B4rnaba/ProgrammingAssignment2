## This 2 functions return an inverted matrix (y) from a given
## matrix (x). In addition, when 'cacheSolve' computes an inverted
## matrix it is stored in 'makeCacheMatrix'. While running 'cacheSolve'
## first check if there is record for given matrix (x) in 'makeCacheMatrix'.
## If so, returns stored inverted matrix (y), otherwise make new computation.


## First function contains 4 other functions:
## set - assigns new matrix as a given matrix
## get - returns the given matrix
## getinverse - returns the inverted matrix
## setinverse - cache the inverted matrix
## I named 'setinverse' argument as 'z' because
## it is only an argument, not a predefined function
## name. If you use predefined function name from
## parent environment you will change the meaning
## of the predefined funtcion. You can do it, but
## what for?

makeCacheMatrix <- function (x = matrix()){
      y <- NULL
      set <- function(v) {
            x <<- v
            y <<- NULL
      }
      get <- function() x
      setinverse <- function(z) y <<- z
      getinverse <- function() y
      list (set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}

## Second function cumputes matrix inversion using solve function,
## but firstly check if it is cache matrix for 'x' value stored
## within makeCacheMatrix (using 'getinverse' subfunction. If it 
## is, the function just returns the inverted cached matrix. Otherwise,
## 'solve' function is executed on a given matrix. The computation's
## result is stored within previous function (using 'setinverse' subfuntion). 

cacheSolve <- function(x, ...){
      y <- x$getinverse()
      if (!is.null(y)) {
            message ("Getting cached matrix...")
            return (y)
      }
      dane <- x$get()
      y <- solve (dane)
      x$setinverse(y)
      y
}