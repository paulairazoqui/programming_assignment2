# define la función makeVector
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# define la función cachemean
cachemean <- function(x, ...) {
  m <- x$getmean()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# crea un vector especial
v <- makeVector(1:10)

# calcula la media del vector (primera vez)
cachemean(v)
#> [1] 5.5

# calcula la media del vector de nuevo (segunda vez)
cachemean(v)
#> getting cached data
#> [1] 5.5