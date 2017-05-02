
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {   # set the matrix
      x <<- y
      inv <<- NULL
    }
    get = function() x   # get the matrix
    setinv = function(inverse) inv <<- inverse  # set the inverse
    getinv = function() inv # get the inverse
    list(set=set, get=get, setinv=setinv, getinv=getinv)   # list is used as the input to cacheSolve()
  }
  
  
  cacheSolve <- function(x, ...) {
    # return: inverse of the original matrix input to makeCacheMatrix()
    
    inv = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
      # get it from the cache and skips the computation. 
      return(inv)
    }
    
}

  # otherwise, calculates the inverse 
  mat.data = x$get() # get the info an save it as an object, mat.data
  inv = solve(mat.data, ...) # solve, function to compute the inverse
  x$setinv(inv) # sets the value of the inverse in the cache via the setinv function.
  
  return(inv) # print it out 
  }

