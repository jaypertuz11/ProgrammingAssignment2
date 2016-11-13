## The functions below first create a special object that is basically a vector that has a list
## of functions that invert a matrix and cache the value for future use.
## The second function checks to see if the inverse of the matrix has been calculated. If it has 
## not been calcualted then it calculates it otherwise it pulls it from the cache.

## The "makeCacheMatrix" function creates a special "matrix" object
## that can cache its inverse, the function:
## 1.  sests the value of x
## 2.  gets the value of x
## 3.  Inverts x
## 4.  returns a list of functions


makeCacheMatrix <- function(x = matrix()) {
                        m <- NULL
                      set <- function(y) {
                                x <<- y
                                m <<- NULL
                              }
                      get <- function() x
                      setsolve <- function(solve) m<<-solve
                      getsolve <- function() m
                      list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
                    }


## The "cacheSolve" uses the resulting object from "makeCacheMatrix" as the argument to 
## calculate the inverse of the Matrix or to determine if it has already been calculated
## and thus it has been cached.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
