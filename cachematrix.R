## In this assignment, I am focusing on calculating and caching the inverse of a matrix

## Step 1: Set the first function input "x" as a matrix
## Step 2: Determined my working value "t" as a null
## Step 3: To test I used this sample: (rnorm(32),4,4) 

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(c){
    x <<- c
    t <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) t <<- inverse
  getinverse <- function()t
  list(set = set,
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Considering the above, I used cacheSolve get the cache data or return the original function

cacheSolve <- function(x, ...) {
  t <- x$getinverse()
  if(!is.null(t)) {
       message("getting cached data")
       return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setinverse(t)
  t
}
