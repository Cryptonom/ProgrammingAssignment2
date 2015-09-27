## Pair of functions that  
##   i) can create a matrix that can cache its inverse 
##  ii) can compute the inverse


## This function creates a special "matrix" object that can cache its inverse.
## There are getter and setter functions for the inverse and the matrix itself.
## It is assumed that the matrix is always invertible.
makeCacheMatrix <- function(mat = matrix()) 
{
    inv <- NULL                         ## initialize inverse
    
    setmatrix <- function(m)            ## function to set matrix to m 
    {                                   ## and initialise inverse
        mat <<- m
        inv <<- NULL
    }
    getmatrix <- function() mat         ## funtion to get matrix
    setinvers <- function(i) inv <<- i  ## funtion to set inverse
    getinvers <- function() inv         ## funtion to get inverse
    
    list(setmatrix = setmatrix,         ## return getter and setter as list
         getmatrix = getmatrix,
         setinvers = setinvers,
         getinvers = getinvers) 
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## It is assumed that the matrix mat is always invertible.
cacheSolve <- function(mat, ...) 
{
    inv <- mat$getinvers()	
    if (!is.null(inv)) 				## is there already a inverse matrix cached?
    {
        message("getting cached data")
        return(inv)
    }
    
    data <- mat$getmatrix()
    inv <- solve(data, ...)		    ## compute inverse matrix
    mat$setinvers(inv)				## set cached inverse matrix within mat
    
    inv					        	## return inverse of mat
}
