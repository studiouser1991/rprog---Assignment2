makeCacheMatrix <- function(x = numeric()) {
        ##the function must hold the cached value or NULL if nothing is stored
        ##that's why original value is set to NULL
      
        m <- NULL 
        
        ##In order to set the value to the function
        ##Hence, the matrix is assigned to a new value, m is NULL again
        set_matrix <- function(y) {
            x <<- y
            m <<- NULL  
        }
        
        ##To get the value of the stored matrix
        get_matrix <- function() {
            x
        }
        ##To cache the value
        cache_inv <- function(solve){ 
            m <<- solve
        }
        
        ##To get the cached value
        get_inverse <- function() {m
        }
        ##To return the list of all above functions
        list(set_matrix = set_matrix, get_matrix = get_matrix, cache_inv=cache_inv, 
             get_inverse=get_inverse)        
    }
    
##The following function computes the inverse of the special matrix.
##It checks the availability of the cached value firstly. If it is available,
##then it gets this value and doesn't proceed with computation, otherwise, it coputes
##inverse and caches the value
    
cacheSolve <- function(x, ...) {
    ##The function checks whether the value was cached already
        
    m <- x$get_inv()
    ##The message is returned with chached data
    
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
  ##If the value is not cached, then we load the matrix into the data      
        data <- x$get_m()
    
  ##And find the inverse matrix
  
        m <- solve(data, ...)
  ##Store it in the cache
        
        x$cache_inv(m)
  ##In the end, we return the value of inverse matrix
        m
    }
    
