## makeCacheMatrix is a set of functions that stores and returns values
## cacheSolve returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## makeCacheMatrix stores a list of 4 functions
        ## 1. get returns the matrix x
        ## 2. set stores the value of the matrix x
        ## 3. get inverse returns the value of the inverse matrix m
        ## 4. set inverse stores the value of the inverse matrix m
        
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

cacheSolve <- function(x, ...) {
        
        ## cacheSolve returns the existing inverse matrix if one exists
        ## cacheSolve calculates the inverse matrix and returns it if one
        ## didn't exist previously
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
