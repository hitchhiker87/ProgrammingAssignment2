## makeCacheMatrix creates a special "matrix" object, which is then saved to            
## to the cache. 
##
## cacheSolve returns the inverse of the matrix previously saved to the cache.


## The first function, makeCacheMatrix creates a special 'matrix' object, 
## which is really a list containing a function to:
## * 1. set the value of the matrix
## * 2. get the value of the matrix
## * 3. set the value of the inverse
## * 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special 'matrix' created
## by makeCacheMatrix. It first checks to see if the inverse
## has already been caclulated. If so, it will get the inverse of the matrix  
## from the cache and skips the computation. Otherwise, it calculates 
## the matrix inverse and sets the value of the inverse in the cache via 
## the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}