## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special matrix, which contains a list of functions to
## 1- set the value of the matrix, 2- get the value of the matrix, 3- set the value of its inverse matrix
## 4- get the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
                x <<- y
                m <<- NULL
        }
     get <- function() x
     setinverse <- function(mean) m <<- mean
     getinverse <- function() m
     list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse)


}


## Write a short comment describing this function
## This method calculates the inverse of the special matrix created with the function makeCacheMatrix. It checks first 
## to see if the inverse of an identical matrix has already been calculated. If it is the case, it will use the cache, otherwise, it will 
## calculate the inverse and via the setinverse function

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
