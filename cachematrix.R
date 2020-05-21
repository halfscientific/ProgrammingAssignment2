## Lexical scoping 
## This function gets the value of vector and sets the value of matrix

makeCacheMatrix <- function(x = matrix()) {
        
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        

}


## This function finds invrse in catche if not found calculates one
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
