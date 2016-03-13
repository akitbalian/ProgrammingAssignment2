## This is a set of two functions intended to work togheter to reduce
## computing times when calculating the inverse of a matrix

#Usage: x <- makeCacheMatrix(matrix())
#       cachemean(x)
#       x can be any variable name
#       matrix() needs to be a square inversible matrix
#       x = matrix(c(1,3,1,3,2,3,1,3,3),nrow=3,ncol=3) was used to tesing

#create makeCacheMatrix function based on the makeVector example
#only real distinction is that we take a matrix as input instead of a numeric
#... and that we calculate the inverse instead of a mean
makeCacheMatrix <- function( x = matrix() ) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#create cachemean based on example given
#use solve() to get inverse of the matrix
cachemean <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
