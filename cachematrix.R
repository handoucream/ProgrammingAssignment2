## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse. 
## makeCacheMatrix will generate the inverse of x and save the inverse in a matrix. 

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


## cacheSolve is to get the cached inverse of matrix x from the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
                    m <- x$getsolve()
                    if(!is.null(m)) {
                                        message("getting cached inverse data")
                                        return(m)
                    }
                    data <- x$get()
                    m <- solve(data, ...)
                    x$setsolve(m)
                    m        
}
