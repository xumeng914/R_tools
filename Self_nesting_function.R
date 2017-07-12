
y <- 0
f <- function(x) {
  if (x > 0) {
    y <- x + f(x - 1)
  }
  return(y)
}

f(10)