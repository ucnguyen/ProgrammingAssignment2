## Programming Assignment 2: Lexical Scoping
## By U. Nguyen

## There are 2 functions in this source file that work togehter
## to allow the user to cache the inverse of a matrix, so that it 
## can be recalled without having to be recalculated.


#############################################################
# makeCacheMatrix - creates a "matrix" object of 4 functions:
# - set: receives input matrix to store it
# - get: returns the input matrix
# - setInverse: receives the inverse matrix to store it
# - getInverse: returns the inverse matrix
#############################################################                           
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(newMatrix) {
                x <<- newMatrix
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inv <<- inverseMatrix
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


###############################################################
# cacheSolve - accepts input object created by makeCacheMatrix,
#              calculates inverse matrix if not already cached,
#              returns the inverse matrix
###############################################################
cacheSolve <- function(x, ...) {
        i <- x$getInverse()         	
        if(!is.null(i)) {		
                message("getting cached data")	
                return(i)
        }
        data <- x$get()	
        i <- solve(data, ...)	
        x$setInverse(i)		
        i
}
