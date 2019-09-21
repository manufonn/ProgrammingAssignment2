
##########

makeCacheMatrix <- function(x = matrix()) { ## this function creates special matrix to cache inverse
        v <- NULL
        set <- function(y) {  
                x <<- y   ## sets the value of the matrix
                v <<- NULL
        }
        get <- function() x   ## gets the value of the matrix
        setinverse <- function(inverse) v <<- inverse  ## sets the value of the inverse
        getinverse <- function() v  ## gets the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



cacheSolve <- function(x, ...) { ## this function calculates inverse of matrix if cache is empty
        v <- x$getinverse()        ## retrieves inverse from cache without calculation
        if(!is.null(v)) {        ## if the inverse is NOT null
                message("getting the cached data")
                return(v)		## states the inverse
                
        }
        Matx <- x$get()		## computes the inverse from original function x
        v <- solve(Matx, ...)	## returns the inverse matrix
        x$setinverse(v)
        v
}

