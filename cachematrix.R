makeCacheMatrix <- function(x = matrix()) { ## this function creates special matrix to cache the inverse
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) v <<- inverse
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        v <- x$getinverse()        ## retrieves inverse from cache without calculation
        if(!is.null(v)) {        ## if the inverse is NOT null
                message("getting the cached data in")
                return(v)		## states the inverse
                
        }
        Matx <- x$get()		## computes the inverse from original function x
        v <- solve(Matx, ...)	## return the inverse matrix
}