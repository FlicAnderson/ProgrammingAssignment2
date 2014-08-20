## Coursera :: R Programming :: Programming Assignment 2 :: cachematrix.R
## ======================================================================== 
## (20th August 2014)
## Author: Flic Anderson
##

## AIM: this script finds the inverse of a matrix & caches the result, returning 
## ...  the cached value if the inverted matrix does not need to be recalculated

# source functions: makeCacheMatrix(); cacheSolve()

### function: makeCacheMatrix()
# create 'matrix' object which can cache its inverse
makeCacheMatrix <- function(x=matrix()){    # input x is a matrix
  
    m <- NULL   # m will be the matrix & is reset every time makeCacheMatrix()
    # is called
  
    # get() returns value of the original input
    get <- function(){x}
  
    # setInvertedMatrix() solves the matrix when cacheSolve() is called & stores
    # the result as 'm' using the '<<-' operator
    setInvertedMatrix <- function(solve){m <<- solve}
  
    # getInvertedMatrix() returns the cached value of 'm' when cacheSolve() is 
    # called again with the same initial values
    getInvertedMatrix <- function(){m}
  
    # list returns newly created 'matrix' object & all functions which are part
    # of it:    
    list(
      get = get,    
      setInvertedMatrix=setInvertedMatrix, 
      getInvertedMatrix=getInvertedMatrix
    )
  
}


### function: cacheSolve()
# compute the inverse of the 'matrix' returned by makeCacheMatrix(); 
# if inverse has already been calculated & there != change, return cached object
cacheSolve <- function(x, ...) {  # input is object created by makeCacheMatrix()
  
    # access object x and get the inverted matrix value
    m <- x$getInvertedMatrix()  #
  
    # if the matrix is already cached, and 'm' is NOT NULL:
    if(!is.null(m)){ 
    
        # send this message to the console
        message("getting cached data")  
    
        # return pre-calculated inverted matrix & end the cacheSolve() function
        return(m)
    }  
  
    # if x$getInvertedMatrix() was NULL & 'm' is not cached, get the original 
    # values of x so we can solve it now:
    datA <- x$get() 
  
    # m is inverted using the original matrix values
    m <- solve(datA, ...)
  
    # m is set as the inverted matrix values
    x$setInvertedMatrix(m)
  
    # 'm' is returned, & with it the inverted matrix of 'x'
    m
}

