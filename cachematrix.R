## This script computes the inverse of a matrix, and caches it's value for future use to enhance the performance of 
## operations that involve calculating the inverse of a matrix. Considering that, the inverse of a matrix can be a time consuming opertation.
## A pre-requisite to run this script: the matrix is inversible with no errors.
## The Script has 2 functions: makeCacheMatrix and cacheSolve that are described below.

## The makeCacheMatrix function, takes a matrix as an input and constructs a list with 4 functions: 
## 1- set: to initialize certain variables according to a given matrix
## 2- get: to return the value of a given matrix
## 3- setinverse: to cache the inverse matrix provided as a parameter
## 4- getinverse: to retrieve the cached value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        
        set <- function (y){
                x <<- y
                invM <<- NULL
        }
        
        get <- function () x
        
        setinverse <- function(inverse) invM <<- inverse
        
        getinverse <- function () invM
        
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## The cacheSolve function, takes a matrix as an input and checks if there is already an inverse for this matrix available
## in the cache. If not, it calculates the inverse of the given matrix and caches its value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invM <- x$getinverse()
        
        if (!is.null(invM)){
                message ("getting cached matrix")
                return(invM)
        }
        
        data <- x$get()
        
        invM <- solve(data,...)
        
        x$setinverse(invM)
        
        invM
}
