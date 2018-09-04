exp <- function(lambda){
  u <- runif(1)
  y <- -log(1-u)/lambda
  return(y)
}

