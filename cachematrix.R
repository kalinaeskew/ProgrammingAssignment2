## Creates matrix that can be cached as its inverse

makeCacheMatrix <- function(x = matrix()) { 
        
## Set the value of the matrix
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
## Get the value of the matrix
        
        get <- function() x
        
## Set the value of the inverse
        
        setinverse <- function(solve) inverse <<-solve
        
## Get the value of the inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes inverse of matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
         data <- x$get()
         inverse <- solve(data, ...)
         x$setinverse(inverse)
         inverse
}