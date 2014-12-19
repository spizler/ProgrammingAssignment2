## makeCacheMatrix creates a Cache Matrix object and allows other functions to 
##  get and set the matrix and it's inverse
## cacheSolve takes in a Cache Matrix object, and if the inverse matrix is null,
##  the inverse matrix is solved and set

## Creates a special "Cache Matrix" object that can cache its inverse. The two 
## variables stored in this object will be "x" and "invx"
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse matrix
    invx <- NULL
    
    ## set a new matrix, inverse not yet calculated
    set <- function(y) {
        x <<- y
        inx <<- NULL
    }
    
    ## get matrix
    get <- function() x
    
    ## set inverse matrix
    setinv <- function(inversematrix) invx <<- inversematrix
    
    ## get invsese matrix
    getinv <- function() invx
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## retrieve existing inversematrix (may be NULL)
    invx <- x$getinv()
    
    ## if invx is not NULL, return invmatrix
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    
    ## if invx does not exist, calculate invx and set invx
    data <- x$get() #get matrix, x
    invx <- solve(data, ...)
    x$setinv(invx)
    invx
}

