## This script contains functions that support a special type of matrix, called
## CacheMatrix.  This special type represent a matrix and the cached storage of
## its inverse value.  The following functiosn are supported:

## makeCacheMatrix(x) - function to create the special matrix, where x is the 
## newly created matrix
## cacheSolve(x) - function to compute the inverse of matrix specified in x. 'x' 
## should be of type 'list' compatible wuth the matrix type 'CacheMatrix'.


## This function creates a special type of matrix (called 'CacheMatrix) that 
## supports particular 'setter' and 'getter' methods on the value of the created
## matrix itself and also its inverse.  

## The special methods are:
## set <- function to set the matrix
## get <- function to get the value of matrix
## setInv <- function to set the value of the matrix inverse
## getInv <- function to get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    # i is the matrix inverse
    i <- NULL
    
    # 'set' method for matrix x
    set <- function(y) {
        x <<- y
        i <<- NULL                
    }
    
    # 'get' method for matrix x
    get <- function() x
    
    # 'setInv' method for matrix inverse i
    setInv <- function(inv) i <<- inv
    
    # 'getInv' method for matrix inverse i
    getInv <- function() i
    
    # List of methods supported by this special matrix
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Return a matrix that is the inverse of 'x'.  If the inverse has been 
## previously computed and cached, then the stored value is reurned; otherwise
## the inverse is computed in real time and cached for future retrieval

## The arg to cacheSolve must be of type list

cacheSolve <- function(x, ...) {
    
    # Check that x is a list class
    stopifnot(class(x) == "list")
    
    # Check if inverse has already been computed
    i <- x$getInv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Inverse not cached, so compute now
    message("computing inverse now")
    i <- solve(x$get())
    x$setInv(i)
    i    
}
