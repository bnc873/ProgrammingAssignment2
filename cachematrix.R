## makeCacheMatrix.R and cacheSolve.R create a special object of a matrix and caches its inverse 

## 'makeCacheMatrix.R' computes the inverse of an input matrix 'x' and caches
## the input matrix and the inverse matrix

## 'x' is a matrix
## 'inv' is the inverse of the matrix 'x'

makeCacheMatrix <- function(x = matrix()){

        inv <- NULL      ## initialize value for inv (an inverse matrix of x)
        
        ##input_matrix function assigns the input matrix to a special object 'setmatrix'
        set <- function(y) {   
               x <<- y          ## assign input matrix to x in parent environment
               inv <<- NULL     ## clears any cached value of inv (inverse matrix object) in parent environment
        }
        
        get <- function() x     ## return x from parent env     
        
        setinverse <- function(solve) inv <<- solve     ## set solve to inverse matrix
        getinverse <- function() inv                    ## returns inverse matrix 
        
        ## special vector of functions in makeCacheMatrix 
        list(set = set, 
             get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve.R can retrieve a cached inverse matrix of the input matrix 'x'. 
## Where the inverse is not cached, it computes the inverse of 'x'.  

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()     ## return cached inverse of input matrix if it exists 
        
        if(!is.null(inv)) {
                message("Getting cached data . . .")    ## Display message in console
                return(inv)                             ## print cached inverse matrix
        }
        
        ## compute inverse of x if it was not cached
        
        xdata <- x$get()                ## get input matrix
        inv <- solve(xdata, ...)        ## compute inverse 
        x$setinverse(inv)               ## assign inverse to inv in parent env
        inv                             ## print inverse matrix
}
