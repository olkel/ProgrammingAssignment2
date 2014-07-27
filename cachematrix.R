## makeCacheMatrix adds some functionality to a matrix that can recover stored inverses

makeCacheMatrix <- function(x = matrix()) {
      myINV <- NULL
      set <- function(y) {
            x <<- y
            myINV <<- NULL
      }
      get <- function() x
      setINV <- function(solve) myINV <<- solve
      getINV <- function() myINV
      list(set = set, get = get,
           setINV = setINV,
           getINV = getINV)
}


## cacheSolve will find an invers and store it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      myINV <- x$getINV()
      if(!is.null(myINV)) {
            message("getting cached data")
            return(myINV)
      }
      data <- x$get()
      myINV <- solve(data, ...)
      x$setINV(myINV)
      myINV
}
