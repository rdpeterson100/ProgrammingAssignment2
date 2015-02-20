## makeCacheMatrix and cacheSolve: functions to solve the inverse of a matrix, 
##      store the solution in cache, then on subsequent runs to check the cache
##      and if the solution is already there, to take the solution from cache.

## makeCacheMatrix: defines 4 functions and generates a list of the 4 functions.
##      1) set: a function that allows you to create a matrix with "x$set(y).
##              Sets the internal variable x to the matrix y which is read in
##              from the parent environment.
##      2) get: function that returns the matrix (x) that you have set.
##      3) setinverse: function that sets "solve"s the inverse of the matrix, and
##              sets writes the inverse into the variable m.
##      4) getinverse: function that returns m, the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
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


## cachesolve - solves the inverse of the matrix after first checking whether
##      it already exists in cache.

## Gets m from x (which is returned from makeCacheMatrix).
## Checks whether m is NULL. If it isn't, the solution already exists in the 
##      cache, and the function returns the value of m and then stops.
## If m is NULL, function uses the get function of makeCacheMatrix to get the
##      matrix from makeCacheMatrix, solves the inverse, saves the solution to
##      the variable x, and returns the solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
