# Item a ------------------------------------------------------------------
I <- function(n,lambda){
  s <- sum(1:n*expo_n(n,1))
  if (s>=21.6){
    return(1)
  }
  else{
    return(0)
  }
}




