# Item b ------------------------------------------------------------------
ant <- function(n){
  z1 <- z2 <- m <- prob <- NULL
  for (j in 1:n){
    for (i in 1:100){
      u <- runif(5)
      z1[i] <- sum(1:5*(-log(u)))
      z2[i] <- sum(1:5*(-log(1-u)))
      
      if(z1[i]>21.6){z1[i]=1}
      else{z1[i]=0}
      if(z2[i]>21.6){z2[i]=1}
      else{z2[i]=0}
      m[i] <- mean(c(z1[i],z2[i]))
    }
    prob[j] <- mean(m)
  }
  return(prob)
}
mean(ant(100))