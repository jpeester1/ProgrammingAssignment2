# makeCacheMatrix takes a new matrix as an argument. It returns a list containing 4 functions to operate on this matrix
#cacheSolve takes a matrix created using makeCacheMatrix as input, checks to see if the inverse of this matrix 
#is cached. If so,the cached version is returned otherwise the function calculates the inverse of the function 
#and stores it in the cache environments using the <<- operator



#makeCacheMatrix takes a new matrix as an argument. It returns a list containing 4 functions to operate on this matrix. 
#get() - the get function returns the matrix passed in
#set() - sets the new matrix in the cache environment and clears(sets to null) the cached, inverse version of the 
#        matrix held in 'm'
#setinverse() - takes a matrix and assigns it to our cached matrix held in 'm'. This matrix should be the inverse of our
#         original matrix
#getinverse() - returns the cached version of the matrix specified using setinverse()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        #set the new matrix. When we set the new matrix, set the old, cached matrix of the parent to null
        #so that it will be inversed
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #pass in an inverse of a matrix, assign it to our cahced matrix variabe
        setinverse <- function(inverse) m <<- inverse
        #get the inverse 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve takes a matrix created using makeCacheMatrix as input, checks to see if the inverse of this matrix 
#is cached. If so,the cached version is returned otherwise the function calculates the inverse of the function 
#and stores it in the cache environments using the <<- operator

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
