# ============================================================================
# The following code defines a special matrix type which can retain its
# inverse when it is calculated. It also defines a special function for
# calculating the inverse of the special matrix so that the inverse of
# matrix is calculated only if it has not been calculated before; otherwise,
# the preveiously calculated nverse of matrix is returned. 

# The result is reducing the computation time when inverse of a matrix has to
# be calculated repeatedly, such as in a loop.
# ============================================================================


# This function creates an special matrix which can retain its inverse when
# it is calculated.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y  #Stored the new matrix
        inv <<- NULL  #Clearing the preveiously cached inverse of the matrix
    }
    get <- function(){
        x  #Returning current stored matrix
    }
    set.inv <- function(new.inv){
        inv <<- new.inv  #Caching new inverse of the matrix
    }
    get.inv <- function(){
        inv  #Returining cached inverse of the matrix
    }
    list(set = set, get = get,
         set.inv = set.inv,
         get.inv = get.inv)
}


# This function calculates the inverse of a special matrix as defined by
# "makeCacheMatrix" function if the inverse has not been calculated yet.
# Otherwise, it returns the inverse calculated before.
cacheSolve <- function(x, ...) {
    
    inv <- x$get.inv()
    if(!is.null(inv)) {
        message("Getting cached inverse of the matrix ...");
        return(inv) #Returning the preveiously cached inverse of the matrix
    }
    
    #Calculating inverse of the matrix, only if it has not been calculated yet
    message("Caclulating inverse of the matrix for the first time ...");
    new.matrix <- x$get()
    inv <- solve(new.matrix, ...)
    x$set.inv(inv)
    inv
}