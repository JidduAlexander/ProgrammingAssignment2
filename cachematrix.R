## The two functions can be used to craete special object that 
## store a matrix and caches it's inverse.

## makeCacheMatrix creates a special "matrix", which is a list 
## containing a function to set and get the matrix and to set 
## and get the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<-NULL
      }
      get=function()x
      setinv=function(inv) i <<- inv
      getinv=function()i
      list(set=set, get=get, setinv = setinv, getinv=getinv)
}

## cacheSolve checks if the inverse matrix of the special matrix
## from the function before has already been calculated. If it has 
## it skips the computation, if not it computes the inverse and 
## sets the value in the cache.

cacheSolve <- function(x, ...) {
      i = x$getinv()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data = x$get()
      i = solve(data, ...)
      x$setinv(i)
      i
}
