## cacheMatrix takes a matrix and stores it
## as well as calculates its inverse using the solve
## function, it stores the values of the matrix
## and the inverse in cache for easy retrieval
## using the get and getsolve functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Once makeCacheMatrix is called, cacheSolve can take
## advantage of the get and getsolve functions
## to retrieve the inverse of the matrix from 
## cache if possible, otherwise it will calculate it

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}