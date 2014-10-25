## makeCachematrix and cacheSolve are a pair of functions  
## that can cache the inverse of a matrix


## makeCacheMatrix creates a special matrix object that can cache the
## matrix's inverse
makeCacheMatrix <- function(x = matrix()) {

        ## the value of the inverse matrix needs to be reset when setting
        ## new values to the matrix
        inversevar <- NULL
        
        ## to set and modify existing matrix and make inverse matrix null
        set <- function(y) 
        {
                x <<- y
                inversevar <<- NULL
        }
        
        ## function to output the matrix
        get <- function() 
        {        
                x
        }
        
        ## set the inverse matrix
        setinverse <- function(inv) 
        {
                inversevar <<- inv
        }
        
        ## returns the inverse matrix 
        getinverse <- function()
        {
                inversevar
        }
        
        ## create a list of the functions in makeCacheMatrix in order to
        ## access them in another environment
        list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve creates the inverse of the matrix assuming that the matrix
## is always invertible

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inversevar <- x$getinverse()
        
        ## if the inverse matrix is not null then return the cached data
        if(!is.null(inversevar)) 
        {
                message("getting cached data")
                
                ## return the value of the inverse matrix and go out of
                ##functions
                return(inversevar)
               
        }
        
        ## execute the following code if the inverse variable is null
        ## we could have use if else as well if return (inversvar) was not used
        ## in if statement above
        
        ## get the matrix x in order to inverse it using solve function
        datamatrix <- x$get()
        
        ## solve function creates the inverse of the matrix
        inversevar <- solve(datamatrix, ...)
        
        ## set the value of the inverse matrix
        x$setinverse(inversevar)
        
        ## return the inverse from cacheSolve function
        inversevar
                
        
        
}
