makeCacheMatrix <- function (x=matrix()){
        inv <- matrix()
        set_matrix <- function (y=matrix()) {
                x <<- y
                inv <<- NULL
        }
        
        get_matrix <- function() x
        
        set_inv <-function(matrix_inv) inv <<- matrix_inv
        
        get_inv <- function() inv
        
        list (set_matrix=set_matrix,get_matrix=get_matrix,set_inv=set_inv,get_inv=get_inv)
}

CacheSolve <- function (x, ...) {
        inv <- x$get_inv()
        if(!all(is.na(inv))) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get_matrix()
        inv <- solve(data)
        x$set_inv(inv)
        inv
}