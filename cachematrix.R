## R Programming: Programming Assignment 2

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. It can be very memory-efficient, specially for large matrices. 
## The following pair of functions are used to cache the inverse of a matrix.
## We assume that the matrix provided is always invertible

## As the example provided: makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function (x = matrix()) {
      
      inv <- NULL             ## We initialize the inverse to NULL
      
      set <- function(y) {    ## Set the value of the matrix
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x     ## Get the value of the matrix
      
      setinverse <- function(inverse) inv <<- inverse       ## Set the value of the inverse of the matrix
      
      getinverse <- function() inv                          ## Get the value of the inverse of the matrix
      
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    ## Return this list
}


## The following function returns the inverse of the matrix. It first checks to see if the inverse has already been computed. 
## If so, it gets the result and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value 
## of the inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()                     ## Checking for previous calculations of the inverse
      
      if (!is.null(inv)) {                      ## If the inverse already exists, we return it and we are done 
            message ("getting cached data")
            return (inv)
      }
      
      data <- x$get()               ## We get the function
      
      inv <- solve(data, ...)       ## We calculate the inverse
      
      x$setinverse (inv)            ## We set the value of the inverse in inv
      
      inv                           ## We return inv
}


## Usage example: 

## > x <- rbind (c (1, 2, 3), c(3, -2, 1), c(-3, 2, 1))
## > m <- makeCacheMatrix(x)
## > s <- cacheSolve(m)

## When we run it the first time, the function will calculate the inverse: 

##  s
## [,1]   [,2] [,3]
## [1,] 0.250 -0.250 -0.5
## [2,] 0.375 -0.625 -0.5
## [3,] 0.000  0.500  0.5

## When we do it the next time, it will get the previous calculated value. 
