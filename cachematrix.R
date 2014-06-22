# Create Cacheable Matrix Object

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
m$set(matrix( c(0, 2, 2, 0), 2, 2))
  x <<- matrix
  m <<- NULL
}
m$get <- function() x
setmean <- function(mean) m <<- mean
getmean <- function() m
list(m$set = m$set, m$get = m$get,
     setmean = setmean,
     getmean = getmean)
}

# Test the inverse cacher

cacheSolve <- function(x, ...) {
        m <- x$getmean
        if(!is.null(m)) {
          message("getting cached data")
          return (m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
