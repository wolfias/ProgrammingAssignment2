## cache the inverse of a matrix

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(m){
                x <<- y
                inverseMatrix <<- NULL
                
        }
        get <- function() x
        setinverse <-function(im) inverseMatrix <<- im
        getinverse <-function() inverseMatrix
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)){
                message("geting cached data")
                return (inverseMatrix)
        }
        
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
