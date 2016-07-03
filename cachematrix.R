## Put comments here that give an overall description of what your
 functions do



## Function makeCacheMatrix 
## Takes a matrix 'x' input and returns a special matrix containing subsetted functions: 
## 'get' returns the data in matrix 'x'
## 'setsolve' stores the result of a 'solve' function on matrix 'x' returned from the calling
## environment 
## 'getsolve' returns the inverse of matrix 'x' stored by the 'setsolve' function


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function cacheSolve 
## Takes a special matrix 'x' input and checks for a stored inverse by calling the subsetted
## function 'getsolve()' of special matrix 'x' 
## If a stored inverse exists the function outputs a message that it is using stored
## data and returns the cached value, ending operation of the function.
## If a stored value does not exist matrix data is pulled from the special matrix using the
## subsetted function 'get()'.  The inverse of the matrix data is generated with the 'solve' 
## function and the result is returned to the special matrix via the 'setsolve' subsetted
## function of the special matrix.
## cacheSolve then outputs the inverted matrix.


cacheSolve <- function(x, ...) { 
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
} 
