## @author lietwin (ludi.akue@gmail.com)

## makeCacheMatrix takes an invertibke matrix and returns a special matrix
## which is a list containing 4 functions to
## resp. set/get the value of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # variable inv va contenir la matrice inverse
    inv <- NULL
    # function set permet de changer la matrice initiale
    set <- function(k){
        x <<- k
        inv <<- NULL
    }
    # function get to get the initial matrix x
    get <-function() x
    # function getinv to get the inverted matrix inv
    getinv <- function() inv
    # function setinv to set the inverted matrix inv 
    setinv <- function(inverse) inv <<- inverse
    
    # a list of set, get, getinv and setinv functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes the resulting special matrix
## checked if its inversed exists already, if so its gets the inverse and skips the computation
## if not, it calculates the inverse

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cache inverse")
        inv
    }
    mcopy <- x$get()
    inv <- solve(mcopy, ...)
    x$setinv(inv)
    inv
}
