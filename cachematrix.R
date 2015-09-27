# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set matrix
# 2. get matrix
# 3. set inverse
# 4. get inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve assumes that the matrix is always invertible. It first checks if the inverse is cached, if yes 
# it returns the cached value, else it calcultes, sets and returns an inverse.
cacheSolve <- function(x, ...) {
    if(!is.null(x$getinverse())) {
        message("getting cached data.")
        return(x$getinverse())
    }
    x$setinverse(solve(x$get()))
    x$getinverse()
}

## Sample run:
#> x = rbind(c(1, 2), c(3, 1))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    1
#> cacheSolve(m)
#[,1] [,2]
#[1,] -0.2  0.4
#[2,]  0.6 -0.2
#> cacheSolve(m)
#getting cached data.
#[,1] [,2]
#[1,] -0.2  0.4
#[2,]  0.6 -0.2
