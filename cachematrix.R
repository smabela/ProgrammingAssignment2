## A function is a piece of code written to carry out a specified task;
## it may accept arguments or parameters (or not) 
## and it may return one or more values (or not)


## makeCacheMatrix function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}


## cacheSolve function computes the inverse of a matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        
		 ## Return a matrix that is the inverse of 'x'
		 
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
		
		
		
		
}
