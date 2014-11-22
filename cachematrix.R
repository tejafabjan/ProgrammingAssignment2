## Below are two functions that are used to create a special matrix
## object that stores a numeric vector and cache's its inverse

## This function creates a special "matrix" object that 
## can cache its inverse
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

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
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

mat <- matrix(1:2, nrow = 2, ncol = 2) # test 1
mat <- matrix(c(1,3,9,7,5,3,9,5,4), nrow = 3, ncol = 3)  # test 2
solve(mat)  # check if it is invertable
matObj <- makeCacheMatrix(mat)
# class(matObj) # check if it is a list

cacheSolve(matObj)
cacheSolve(matObj)

