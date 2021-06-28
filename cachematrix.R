## solves the inverse of matrix. Stores the solution using cacheSolve function. 
## 
#  set() the value of matrix 
# get ()  gets the value of matrix
# setinv() solves and sets value of inverse
# getinv() gets value of inverse


## defines a matrix and defines the list of accompanying functions described above.
## These functions set,get, getinv, setinv are part of makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinv <- function(solve) i <<- solve
                getinv <- function() i
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        }



## This functions first looks for a "stored" value , which if not availbel, the inverse is calculated
cacheSolve <- function(x, ...) {
                i <- x$getinv()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(i)
                i
        }

