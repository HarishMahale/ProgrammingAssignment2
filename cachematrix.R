##Code created by Harish Mahale for Coursera Assignment-2

##Matrix inversion is usually a costly computation.
##There may be some benefit to caching the inverse of a matrix rather than computing it repeatedly

## Below functions aim to cache the inverse of a matrix...
## So that it need not be calculated everytime if available in cache already

## makeCacheMatrix : This function creates a special "matrix" and is a list containing a fuction to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Creates a special matrix object that can cache its inverse
    
    invx <- NULL                #sets inverse of x to NULL
    
    set <- function(y) {
        x <<- y                 # creates matrix x
        invx <<- NULL           #sets inverse of x to NULL in this environment
    }
    
    get <- function() x                         #returns the matrix x
    
    setinvx <- function(solve) invx <<- solve   #sets inverse matrix
    
    getinvx <- function() invx                  #gets inverse matrix
    
    list(set = set, get = get, setinvx = setinvx, getinvx = getinvx)
}


## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    invx <- x$getinvx()
    
    if(!is.null(invx)) {
        message("Getting cached data")
        return(invx)
    }
    
    data <- x$get()
    
    invx <- solve(data, ...)
    
    x$setinvx(invx)
    
    invx
}
