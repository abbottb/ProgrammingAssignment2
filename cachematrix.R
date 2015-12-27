## makeCacheMatrix is a set of functions that stores and returns values
## cacheSolve returns the inverse matrix

## makeCacheMatrix stores a list of 4 functions
## get returns the matrix x
## set stores the value of the matrix x
## get inverse returns the value of the inverse matrix m
## set inverse stores the value of the inverse matrix m

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the existing inverse matrix if one exists
## cacheSolve calculates the inverse matrix and returns it if one
## didn't exist previously

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
