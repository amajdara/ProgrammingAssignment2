## Combination of these two functions aim to accelerate calculation of the inverse of a
## matrix via "Caching" mechanism, i.e. if the inverse of the matrix has been calculated 
## before (and the matrix has not changed), it's much faster to just fetch the stored results, 
## rather than re-calculating it. The speed up will be significant for large size matrices. 


## Function "makeCacheMatrix" gets a matrix as input, and is able to set the elements of 
## the matrix, return the value of the matrix, set the elements of the inverse matrix, and
## return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y     
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
}


## Input to "cacheSolve" is a vector created by "makeCacheMatrix". It returns the inverse  
## of the input. If the inverse has been calculated before, this function fetches it from 
## Cache. If not, it calculates the inverse matrix using function "solve".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        start_time <- Sys.time()  ## Start measuring the processing time 
        
        inv <- x$getinv()
        if(!is.null(inv)) {       # i.e., the inverse is already calculated
                message("getting cached data")
                end_time <- Sys.time()  
                duration <- end_time - start_time  # Calculate the processing time
                print(duration)
                return(inv)
        }
        data <- x$get()           # Get the input and calculate the inverse matrix
        inv <- solve(data, ...) 
        x$setinv(inv)
        
        end_time <- Sys.time()      
        duration <- end_time - start_time          # Calculate the processing time
        print(duration)
        
        inv        
}

