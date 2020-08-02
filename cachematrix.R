## This assignment invoves solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve".


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        j <- x$getInverse()
        if(!is.null(j)){
                message("getting cached data")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}