#This file contains two functions which together calculate the inverse of
#of a matrix and cache the result so it does not need to be recalculated
#In addition, two matrices are created and their inverses calculated to test the functions


#The makeCacheMatrix function creates a list containing four functions that:
# 1. set the matrix to invert
# 2. get the matrix to invert
# 3. set the inverse matrix
# 4. get the inverse matrix
# The function accepts one parameter, the matrix to invert

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


#The cacheSolve function calculates the inverse matrix using the functions 
#in the list created by makeCacheMatrix.
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse sets it in the cache via the setinverse function
#The function accepts one parameter, the list returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m  
}


#Matrices to test functions
m1 <- matrix(c(1:3,6:4,13,14,16),3,3)
m2 <- matrix(c(2:4,6:4,13,14,16),3,3)

#Create list for m1
l1 <- makeCacheMatrix(m1)
#Calculate inverse for m1
c1 <- cacheSolve(l1)
#Confirm inverse was calculated (product should be identity matrix)
c1 %*% m1

#Second test
l2 <- makeCacheMatrix(m2)
c2 <- cacheSolve(l2)
c2 %*% m2





