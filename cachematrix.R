## The functions makeCacheMatrix and cacheSolve take as input an invertible
## square matrix and return its inverse.  They also cache the inverse answer
## and if called upon again will return the cached answer if the calling arg
## is the same as previous calls.
##
## 

## makeCacheMatrix sets up the initial structure of the cache.  For instance:
##
## square_matrix <- matrix(as.numeric(c(1,2,1,1,3,4,4,8,9),3))
## cached_inverse <- makeCacheMatrix(square_matrix)
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## caceSolve checks the structure of makeCacheMatrix output to see if it has
## a cached answer, if it does it returns the cached content, if it does not 
## cacheSolve calculates the inverse of the matrix and caches the new answer.
##
## cacheSolve(cached_inverse)
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message('getting cached data')
                return(m)
        }
        inverse <- x$get()
        m <- solve(inverse)
        x$setinverse(m)
        m
}
