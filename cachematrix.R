## These two functions are used to calculate the inverse of a matrix 

## makecacheMatrix function is used to store the inverse of a matix we calculated
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
             }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      }


## This main function is used to calculate the inverse of a matix if the invese has not been calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
            }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
# the code below is to test the functions. First generate a matrix assigned to a, and then call the two functions and test the results.
a <- matrix(data = c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3)
matrixx <- makeCacheMatrix(a)
matrixx$get()
matrixx$getinverse()
cacheSolve(matrixx)
matrixx$getinverse()
class(matrixx$get())
class(matrixx$getinverse())
