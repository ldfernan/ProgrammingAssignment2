## This R script contains two functions that can help cache the output of Matrix
## Inverse operation. If the code involves taking repeated inverses of a matrix whose content 
## does not change often, then there may be benefit in caching the inverse matrix.

## How to implement caching using the functions below:
## Example:
## Step 1: your_matrix <- matrix(c(2,2,3,2),2,2)
## Step 2: matrix_cache <- makeCacheMatrix(your_matrix)
## Step 3: cacheSolve(matrix_cache)  #This will create inverse and cache the output
## Step 4: cacheSolve(matrix_cache)  #Matrix has not changed, hence this will read the cache! instead of recalculating the Inverse!!

## makeCacheMatrix takes a matrix as the input and outputs a list with get matri, set matrix, get 
## inverse, set inverse funtions. These are internally called by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y  # <<- operator saves the value in a different environment
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a invertibe matrix as input and returns the inverse of the matri.
## However, the function checks and returns a cached value of inverse if available. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() # search cached inverse matrix
    if(!is.null(inv)) { 
        message("getting cached Inverse Matrix") #Cached Inverse matrix found. Function returns the cached matrix and exits.
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...) #Solve function with just one matrix argument performs Inverse operation.
    x$setinverse(inv)
    inv
}
