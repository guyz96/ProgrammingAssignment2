## I have made a function that could retrieve the chached inverse of a matrix as
## a big matrix could use a huge computation power when you try to inverse.
## The function below works as it is assumed that the matrix supplied is always 
## invertible.

## There are 3 steps in this function
## The first thing that occurs in the function is the initialization of two objects, x and i.
## The second thing is that we define the "behaviors" or functions for objects of type makeCacheMatrix()
## The last part creates a new object by returning a list(). The last section 
## of code assigns each of these functions as an element within a list(), and returns it to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y){
            x <<- y
            i <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) i <<- inverse
          getinverse <- function() i
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## the function attempts to retrieve a inverse from the object passed in as the argument. 
## First, it calls the getinverse() function on the input object.
## Then it checks to see whether the result is NULL.
## If the result of !is.null(i) is FALSE, cacheSolve() gets the vector from the input object, 
## calculates a solve(), uses the setinverse() function on the input object 
## to set the mean in the input object, and then returns the value of the inverse to the parent environment by printing the inverse object.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'\
      i <- x$getinverse()
      if (!is.null(i)) {
        message("wait a second,getting chached matrix for you!")
        return(i)
      }
      z <- x$get()
      i <- solve(z, ...)
      x$setinverse(i)
      i
}


## how to use
## aMatrix <- makeCacheMatrix(1:4, 2)
## aMatrix$get()                            # retrieve the MATRIX
## aMatrix$getinverse()                     # solve the inverse of the matrix
## cacheSolve(aMatrix)                      # getting the chached matrix