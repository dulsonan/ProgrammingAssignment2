## If X is a square invertible matrix, the base R function solve(X) returns
## its inverse. However, computing the inverse can be expensive, which is bad
## if it needs to be computed many times.
##  
## This pair of functions create a special "matrix", and a function for
## calculating the inverse of one of those "matrice"s. The first time
## cacheSolve() is called it will calculate the inverse of the "matrix"
## and cache that value; subsequent invocations return the cached value.
##
## Example usage:
##
##    > mat <- matrix(rnorm(4), 2, 2)
##    > c <- makeCacheMatrix(mat)
##    > cacheSolve(c)
##    [,1]       [,2]
##    [1,] 0.64690423 -0.2591773
##    [2,] 0.02270586  0.8979702
##    > 
## 



## Return a 'cacheMatrix' - a wrapper around a matrix suitable for use
## with the cacheSolve() method. The returned value is actually a list 
## of functions which get & set the matrix and its inverse. Reuse is 
## safe, via the set() function:
##
##    > mat <- matrix(1:4, 2, 2)
##    > c <- makeCacheMatrix(mat)
##    > c$getInverse()
##    NULL
##    > s <- cacheSolve(c)
##    > c$getInverse()
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    > mat2 <- matrix(5:8, 2, 2)
##    > c$set(mat)
##    > c$getInverse()
##    NULL
makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y) {
        ## Reset both matrix x and our cached inverse
        ## Unlike <-, <<- searches through parent environments 
        ## for an existing definition of the variable being
        ## defined.
        x <<- y
        cachedInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInv <<- inverse
    getInverse <- function() cachedInv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Takes a special "matrix" returned by makeCacheMatrix(), and calculates and
## returns its inverse. Subsequent invocations of cacheSolve() will return the 
## previously calculated value of the inverse.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (is.null(inverse)) {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
    }
    inverse
}
