## Create Matrix to be cacheable after first inverse

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) ix <<- solve
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## return the inverse of matrix 
## If x never been inverse the function will inverse it,
## or else return the first inverse made..

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverse()
        if(!is.null(ix)) {
                return(ix)  #getting cached data
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}
