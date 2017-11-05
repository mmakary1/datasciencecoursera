## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: 
## Set and get matrix 
## and set/get inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function:
##obtain the inverse of the X matrix computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# Try out the functions:

# 1.create mock matrix , 2 rows by 2 columns
Matrix <- matrix(c(10,30,20,40),2,2)

# 2. compute matrix and assign variable to computed output
cm <- makeCacheMatrix(Matrix)

cacheSolve(cm) ##computed matrix inverse
##output:
      [,1]  [,2]
[1,] -0.20  0.10
[2,]  0.15 -0.05
cacheSolve(cm) ##next run obtains cached matrix inverse
##output:
getting cached data
      [,1]  [,2]
[1,] -0.20  0.10
[2,]  0.15 -0.05 

