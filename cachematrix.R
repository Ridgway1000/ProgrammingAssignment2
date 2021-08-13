## Some function computations can take a long time to process, especially repeatedly like in a loop. If the contents are not channging we can cache the output
## and then looked up from this cache rather than calculating every time. The below functions are an example of this.



##The below is a function which creates a special "matrix" object that can cache its inverse for the input.


makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setInverse <- function() inversed <<- solve(x) #calculate an inverse of the matrix
        getInverse <- function() inversed
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The below Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated, then the cachesolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getInverse()
        if (!is.null(invs)) {
                message("retrieving cached data")
                return(invs)
        }
        mat <- x$get()
        invs <- solve(mat, ...)
        x$setInverse(invs)
        invs
}
