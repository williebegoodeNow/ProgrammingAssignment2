## This program illustrates using a cache to store data in one
## environment and using it in a current environment employing the solve()
## function. Using solve(m) and cacheSolve(mc) return the identical
## results; the key difference being that cacheSolve() uses the 
## cache version of m. [m = matrix; mc = cache version of m]

## The function takes a matrix as an argument and places it in x
## 1. Caches y in x using <<- operator
## 2. declares invx and caches NULL in invx; caches setinvx in inverse
## 3. Get returns x (matrix) using lambda (anonymous) function
## 4. Setinvx sets (stores) inverse as invx; cached in inverse

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinvx <- function(inverse) invx <<- inverse
    getinvx <- function() invx
    list(set = set, get = get,
         setinvx = setinvx,
         getinvx = getinvx)
}

## This function takes an instance of makeCacheMatrix as an argument -- x
## Essentially, it takes a cached version of the original matrix in a
## current environment and returns the inverse values of the cached matrix
## from a different environment

## 1. x$getinvx() passes stored invx to invx
## 2. x$get retrieves the cached matrix and stores it in data
## 3. The inverse of the matrix is passed to invx
## 4. Invx is stored as the current value of the matrix
## 5. Invx displays the inverse values of the cached matrix

cacheSolve <- function(x, ...) {
    invx <- x$getinvx()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve (data, ...)
    x$setinvx(invx)
    invx
}
