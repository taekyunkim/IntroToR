## Intro to R - Programming Assignment 2
## Tae Kyun Kim - 20140921

## The function has 2 parts:
## 1. makeCacheMatrix -> caches a matrix m which is the inverse of the inserted matrix x
## 2. cacheSolve -> if x has not changed, then return m, else calculate new makeCacheMatrix with matrix x' and return inverse matrix m'



## 1. makeCacheMatrix -> caches a matrix m which is the inverse of the inserted matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse )
  
}


## 2. cacheSolve -> if x has not changed, then return m, else calculate new makeCacheMatrix with matrix x' and return inverse matrix m'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}