## These are functions that can set and get the values of 
## a matrix and use those values to calculate the inverse, storing
## the inverse in a cache (should it be needed later).

## makeCacheMatrix sets and gets the values of a matrix
## and sets and gets the inverse values of that matrix. It
## initially sets the value of the cache to NULL because
## nothing has been cached yet.

makeCacheMatrix <- function(x = matrix()) {
        matrixcache <- NULL
        set <- function(new) {
                x <<- new
                matrixcache <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(solve){
                matrixcache <<- solve
        }
        getinverse <- function(){ 
                matrixcache
        }
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve first checks to see if the cache has a value that
## isn't NULL. If the cache has a non-null value, it displays
## "getting cached data" and returns that value. If the cache
## is NULL, it means there is a newly entered matrix and it
## must calculate the inverse of the matrix and then store
## it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixcache <- x$getinverse()
        if(!is.null(matrixcache)) {
                message("getting cached data")
                return(matrixcache)
        }
        data <- x$get()
        matrixcache <- solve(data)
        x$setinverse(matrixcache)
        matrixcache
        
}

## Test:
## > x = matrix(sample(1:10, 9), 3, 3)
## > y = makeCacheMatrix(x)

## Matrix is displayed: 
## > y$get()
##      [,1] [,2]  [,3]
## [1,]    1    6    8
## [2,]    2    9    5
## [3,]    4    7   10

## Initial inverse calculation does not retrieve from cache:
## > cacheSolve(y)
##            [,1]        [,2]        [,3]
## [1,] -0.4545455  0.03305785  0.34710744
## [2,]  0.0000000  0.18181818 -0.09090909
## [3,]  0.1818182 -0.14049587  0.02479339

## Second request for inverse calculation  of the same 
## matrix retrieves the value from cache:
## cacheSolve(y)
## getting cached data
##            [,1]        [,2]        [,3]
## [1,] -0.4545455  0.03305785  0.34710744
## [2,]  0.0000000  0.18181818 -0.09090909
## [3,]  0.1818182 -0.14049587  0.02479339


