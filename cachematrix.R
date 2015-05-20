## These functions calculate the inverse of a matrix 'x' and stores it in memory.
## If matrix 'x' has already been inverted, the result is retrieved from memory instead of calculating the inverse again.

## This function creates a list of functions for storing and retrieving the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                    ##Set the matrix that should be inverted
                x <<- y
                i <<- NULL
        }
        
        get <- function() {                     ##Retrieve matrix that should be inverted
                x
        }
        
        setinverted <- function(inverted){      ##Store the inverted matrix (inside the function makeCacheMatrix)
                i <<- inverted
        }
        
        getinverted <- function(){              ##Retrieve the inverted matrix
                i    
        }
        
        list(set = set, get = get, setinverted = setinverted, 
             getinverted = getinverted)         ##Return a list of functions that can be used to store and retreive the matrix
}


## This function checks to see if the matrix X has already been inverted - if not
## it calculates it and caches the result

cacheSolve <- function(x, ...) {
        i <- x$getinverted()
        if(!is.null(i)) {                       ##Check if matrix 'x' has been inverted (if inversion is not null)
                message("getting cached data")
                return(i)                       ##If already inverted (not null), return the inverted matrix
        }
        
        data <- x$get()                         ##If not inverted already, get the matrix 'x'
        i <- solve(data, ...)                   ##Invert the matrix 'x'
        x$setinverted(i)
        i
}
