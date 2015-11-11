## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix instantiates an object and exposes the functions
## through the returned list
## if a matrix is passed to start then it assigned to x

makeCacheMatrix <- function(x = matrix()) {
    ## initialise NULL so that any call will get an expected result
    m <- NULL
    ## set function can be called later and seperately. it expects an matrix
    set <- function(y) {
        ## assign it to a parent/global/?? variable x
        x <<- y
        ## NULL the parent/global/?? m as the set will invalidate any previous value
        m <<- NULL
    }
    ## get returns x
    get <- function() x
    # take the passed in matrix, assign to parent/global/?? m 
    setmatrix <- function(passedMatrix) m <<- passedMatrix
    # return the matrix
    getmatrix <- function() m
    # expose the functions via the list
    list( set = set, get = get, 
          setmatrix = setmatrix, 
          getmatrix = getmatrix)
}


## cacheSolve receives a makeCacheMatrix object and a matrix if provided
## it checks the object to see if it has a matrix, if it does it returns
## that value instead of computing it.
## this one also checks if a new matrix has been provided and assigns it for 
## computation and assignment
cacheSolve <- function(x, nuMatrix = NULL) {
    #check if we've been provided a nuMatrix
    # if so, update the object
    if(!is.null(nuMatrix)) {
        message("new value matrix... updating object")
        x$set(nuMatrix)            
    }

    #check if we have an x in cache
    m <- x$getmatrix()
    if (!is.null(m)) {  #we got a value so give it
        message("here's one I prepared earlier")
        return(m)
    } else {  # no value yet so
        m <- solve(x$get())  # let's compute 
        x$setmatrix(m)   # and then set it
        return(m)  # and return it
    }
}
