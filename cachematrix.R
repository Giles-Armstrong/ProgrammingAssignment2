## These functions are part of the Week 3 assignment for the JHU Coursera R Programming course 

## The first function creates a matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {     ## initializes a function that has a default
                                                ## object class of matrix. 
  Inv <- NULL     ## initialize Inv as a NULL will eventually hold inverse of x
  set <- function(y) {     ## define set function to assign new value of x in parent environtment
    x <<- y     ## <<- superassignment operator will search for y but if 
                ## not found will create one in global environment
    Inv <<- NULL    ## If there is a new matrix set function will reset Inv to NULL
  }
  get <- function() x     ## a function to return x (initial matrix)
  setInverse <- function(Inverse) Inv <<- Inverse     ## assigns value of Inverse in the parent environtment
  getInverse <- function() Inv     ## a function to return Inv (inverse of x)
  list(set = set, get = get,       ## a list so the cacheSolve function can use the $ operator
       setInverse = setInverse,    ## to retrieve functions
       getInverse = getInverse)

}


## The Second function will solve the inverse of the matrix that is returned above
## First it will check if there is already an inverse and if so will return that

cacheSolve <- function(x, ...) {     ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()     ## gets inverse from makeCacheMatrix 
  if(!is.null(Inv)) {     ## checks if Inv is already cached
    message("getting cached data")     ## if it is then the function retrieves m
    return(Inv)
  }
  data <- x$get()    ## gets matrix
  Inv <- solve(data, ...)    ## solve is a function in R which gets the inverse of a matrix
  x$setInverse(Inv)    ## sets the Inverse variable in the global environment
  Inv    ## returns the inverse
}