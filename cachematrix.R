# Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. These 
## functions are used to create a special object that stores a matrix and caches
## the inverse.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Compute the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed, 
## then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}


I <- rbind(c(10,1,4),c(20,1,10),c(-4,2,1))
m<-makeCacheMatrix(I)
identical(m$get(),I)
m$getInv()
cacheSolve(m)
cacheSolve(m)
cacheSolve(m)

I <- rbind(c(15,6,9),c(20,1,10),c(-4,2,1))
m<-makeCacheMatrix(I)
m$getInv()
cacheSolve(m)
