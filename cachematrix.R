## My functions calculates the inverse of matrix and caches it
## to optimize futher computations.

## The first function creates a special "matrix" object, that can cash its inverse.
## Actually, it is a list, containing a function to:
## 1. Set the value of the matrix;
## 2. Get the value of the matrix;
## 3. Set the value of the inverse matrix;
## 4. Get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created in the above fnction. But first it checks to see if the inverse
## of the matrix has already been calculated. If so, it gets the inverse from
## the cash. Otherwise, it calculates the inverse and set it in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}