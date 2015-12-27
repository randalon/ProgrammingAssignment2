###########################################################################
## Second programming assignment.
## December 2015.
###########################################################################
## This first function, 'makeCacheMatrix' creates a special 'vector', 
## which is really a list containing a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
###########################################################################
## Usage:
## 1. create a matrix
##    x <- matrix(rnorm(9),3,3)
## 2. create the vector calling this function
##    z <- makeCacheMatrix(x)  
###########################################################################
makeCacheMatrix <- function(x = matrix()) {
    xI <- NULL

    # 1. set the value of the matrix
    set <- function(y) {
         x <<- y
         xI <<- NULL
    }
    
    # 2. get the value of the matrix
    get <- function() x
    
    # 3. set the value of the inverse of the matrix
    setinverse <- function(solve)  xI <<- solve

    # 4.  get the value of the inverse of the matrix
    getinverse <- function() xI

    # create the 'vector' (list of functions)
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}  ## end of the 'makeCacheMatrix' function

###########################################################################
## This second function 'cacheSolve' calculates the inverse of a matrix 
## of the special "vector" created with the first function. However, it first
## checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse of the matrix in the cache via the 
## 'setmean' function, so that when we need it again, it can be looked up  
## in the cache rather than recomputed.
###########################################################################
## Usage:
## 1. create a matrix
##    x <- matrix(rnorm(9),3,3)
## 2. create the vector calling this function
##    z <- makeCacheMatrix(x)
## 3. create the inverse of the matrix
##    xi <- cacheSolve(z)
## Testing the results
##    round( x %*% xi , 0)
###########################################################################
cacheSolve <- function(x, ...) {
    # get the inverse of the matrix of the 'vector'
    xI <- x$getinverse()
    
    # checks if the inverse of the matrix exists
    if (!is.null(xI)) {
      message("getting cached data")
      return(xI)
    }
    
    # if not exists, gets the matrix
    data <- x$get()
    
    # calculate the inverse of the matrix
    xI <- solve(data)
    
    # sets the inverse of the matrix
    x$setinverse(xI)
    
    # Return a matrix that is the inverse of 'x'
    xI
    
} ## ends of the function 'cacheSolve' 

####### cachematrix.R
