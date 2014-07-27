## makeCahceMatrix creates a special matrix to store an inverse.
## cacheSolve calculates the inverse of a matrix
## The combined functions work in the same way as the makeVector
## and cachemeans functio examples provided

##############################
## makeCacheMatrix Function ##
##############################

## Function searches through parent environment to see if the inverse of a 
## matrix has already been caluclated. If it has, it will use that inverse
## instead of calculating a new one.

makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(z) {
                x <<- z
                inverse_x <<- NULL
        }
        get <- function () x
        set_inverse <- function(inverse) inverse_x <<- inverse
        get_inverse <- function() inverse_x
        list( set = set, get = get, set_inverse = set_inverse,
              get_inverse = get_inverse)
}

#########################
## cacheSolve Function ##
#########################

## Function checks to see if an inverse matrix has previously been
## calculated and cached. If none is found, it calculates the inverse 
## and caches it

cacheSolve <- function(x, ...) {
        inverse_x <- x$get_inverse ()
        if (!is.null(inverse_x)) {
                message("Retreiving Cached Inverse Matrix")
                return(inverse_x)
        } else {
                inverse_x <- solve(x$get())
                x$set_inverse(inverse_x)
                message("Calculating Inverse Matrix")
                return(inverse_x)
        }
}
