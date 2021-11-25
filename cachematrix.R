# Two following functions compute the inverse of the matrix and cache the 
# computed value.

# The function creates a list that stores the functions to set and get matrix 
# and to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() return(x)
    set_inverse <- function(inv_matrix) inverse_matrix <<- inv_matrix
    get_inverse <- function() return(inverse_matrix)
    return(list(set = set, 
                get = get, 
                set_inverse = set_inverse, 
                get_inverse = get_inverse))
}


# The function returns the inverse matrix based on the list created
# by the makeCacheMatrix function. 
# The cached inverse matrix will be returned if the inverse has been 
# already computed. 
# Otherwise, the inverse matrix will be computed and the value for 
# it will be set in the cache via set_inverse.

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$get_inverse()
    if (!is.null(inverse_matrix)) {
        message("Getting cached inversed matrix.")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    return(inverse_matrix)
}
