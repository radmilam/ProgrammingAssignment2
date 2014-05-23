## This function is able to cache potentially time-consuming 
##computations

## The below function creates a special "matrix" object, which can
## catche its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL

}
get <- function() x
setmean <- function(mean) m <<- mean
getmean <- function() m
list(set = set, get = get,
     setmean = setmean,
     getmean = getmean)
}

## The function below calculates the inverse of the above special "matrix"
## returned by makeCatcheMatrix. It first checks if the inverse has already
## been calculated. If so and the matrix didn't change, it retreives 
## the inverse from the catche and skips calculation 

cacheSolve <- function(x, ...) {
      m <- x$getSolve()
      if(!is.null(m)) {
           message("getting cached data")
           return(m)
      }
      data <- x$get()
      m <- Solve(data, ...)
      x$setSolve(m)
                m
        }        
        
        ## Return a matrix that is the inverse of 'x'
