## Put comments here that give an overall description of what your
## functions do

## This creates creates the special "matrix" capable of caching its inverse

makeCacheMatrix <- function( x = matrix() )
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setInverse <- function(inverse)
    {
        inv <<- inverse
    }
    
    getInverse <- function()
    {
        inv
    }
    
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
         
}


## This checks the special "matrix" for a cached inverse and returns it if already cached. If not already cached, 
## it calculates the inverse, caches it, then returns it.
cacheSolve <- function(x, ...)
{
    inv <- x$getInverse()
    if(!is.null(inv))
    {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
