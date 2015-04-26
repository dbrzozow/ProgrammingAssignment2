## This function represents a matrix and stores its inverse
## It is really an object as the environment is stored due to returend list which simply
## represents the matrix. The matrix will be stored as x and its iverse as mi

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) mi <<- inverse
    getinv <- function() mi
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve takes as an argument the object representing the matrix
## it will either calculate the inverse or use the stored one if such stored one exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mi <- x$getinv()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data)
    x$setinv(mi)
    mi
}
