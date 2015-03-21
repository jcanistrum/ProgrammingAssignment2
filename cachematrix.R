## Program Assignment 2

## pair of functions that cache the inverse of a matrix
## makeCacheMatrix , cacheSolve

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
      # inverse initially NULL
      i <- NULL
      # set the matrix
      set <- function(y) {
        x <<- y
        i <<- NULL
      }
      # get the matrix
      get <- function() x
      
      # calculates de inverse 
      setinverse <- function(solve) i <<- solve
      
      # get the inverse
      getinverse <- function() i
      
      # return 
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


##  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if(!is.null(i)) {
          message("getting cached matrix")
          return(i)
        }
        
        data <- x$get()
        
        i <- solve(data, ...)
        
        x$setinverse(i)
        
        i
}
