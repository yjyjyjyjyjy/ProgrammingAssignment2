## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function( m = matrix() ) {

    m_i <- NULL

    set <- function( matrix ) {
            m <<- matrix
            m_i <<- NULL
    }

    get <- function() {
    	m
    }

    setInverse <- function(inverse) {
        m_i <<- inverse
    }

    getInverse <- function() {
        m_i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
            return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}
