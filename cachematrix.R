# These 2 functions are meant to cache the inverse of a matrix


# 'makeCacheMatrix' function
# ==========================
# Stores a matrix in a variable called 'CachedInvMat'
# And makes available a list of functions to get or set
# the original matrix 'x' and its inverse 'CachedInvMat'

makeCacheMatrix <- function(x = numeric()) {
        CachedInvMat <- NULL
        set <- function(y) {
                x <<- y
                CachedInvMat <<- NULL
        }
        get <- function() x
        setInv <- function(i) CachedInvMat <<- i
        getInv <- function() CachedInvMat
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


# 'cacheSolve' function
# ======================
# Gets the cached inversed matrix
# If non existing, gets the original matrix, inverses it
# and orders to store the value through 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached inversed matrix")
                return(m)
        }
        matrix2inverse <- x$get()
        m <- solve(matrix2inverse)
        x$setInv(m)
        m
}