
## This function creates a special matrix, which contains a list of 4 functions. The different roles of the functions are:
## 1- set the value of the matrix, 2- get the value of the matrix, 3- set the value of the inverse matrix
## 4- get the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
                x <<- y
                m <<- NULL
        }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse)


}

## This method calculates the inverse of the special matrix created with the function makeCacheMatrix. It checks first 
## to see if the inverse has already been calculated. If it is the case, it will use the cache, otherwise, it will 
## calculate the inverse and via the setinverse function and cache it.

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
