#Programming Assignment 2: Lexical Scoping
#In this assignmentI'm going to write a pair of functions;namely, 
#makeCacheMatrix and cachesolve, that can cache the inverse of a matrix.


#The first function, makeCacheMatrix creates a special "matrix" 
makeCacheMatrix  <- function(c) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The second function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse 
#in the cache via the setinverse function
cachesolve <- function(x=matrix(), ...) {
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
