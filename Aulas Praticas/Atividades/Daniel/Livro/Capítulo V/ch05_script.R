# Questão 01 ####
q1 <- function() return(log(runif(1)*(exp(1)-1) + 1))
q1()

# Questão 02 ####
q2 <- function(){
  if(runif(1)*6 <= 3){
    return(2 * (1 + sqrt(runif(1))))
  } else {
    return(6-2*sqrt(3-3*runif(1)))
  }
}
q2()

# Questão 03 ####
q3 <- function() return((- 1 + sqrt(1 + 8 * runif(1)))/2)
q3()

# Questão 04 ####

q4 <- function(alpha, beta) return((log(1/runif(1))/alpha)**1/beta)
q4(1,1)

# Questão 05 ####
q5 <- function(){
  if(runif(1) < .5){
    return(log(2*runif(1))/2)
  } else {
    return(-log(2*(1-runif(1)))/2)
  }
}
q5()
# Questão 06 ####
q6 <- function() return(-log(1-runif(1)*(1-exp(-.05))))
q6()

{
  a <- 0
  for (i in 1:100000){
    a[i] <- q6()
  }
  cat("A média calculada foi =", mean(a))
  rm(list = c("a", "i"))
}

# Questão 10 ####
q10 <- function(){
  n <- 0 ; claim <- NULL
  for(i in 1:1000){
    if(runif(1) < .05){
      n <- n + 1
    }
  }
  for(i in 1:n){
    claim[i] <- -log(runif(1))/(1/800)
  }
  return(sum(claim))
}
a <- 0
for(i in 1:100){
  a[i] <- q10()
}
mean(a)
{
  n <- 0
  for(i in 1:100){
    prob <- q10()
   if(prob > 50000){
     n <- n + 1
    }
  }
  cat("Prob =", n/100)
  rm(list = c("n","prob","i"))
}

# Questão 12 #####
# a)
q12 <- function(lambda,n){
  x <- NULL
  for(i in 1:n){
    x[i] <- -log(1-runif(1))/lambda
  }
  return(max(x))
}
q12(1, 100)

# b) Dessa vez usaríamos o mínimo.

# Questão 14 ####

# a)
# b)

# Questão 16 ####
# Usarei método da rejeição.

# Questão 18 ####
q18 <- function(x) return(sqrt(log(1/runif(1))))
q18()

# Questão 19 ####
# a)
q19a <- function() return((-1 + sqrt(1 + 8*runif(1)))/2)
q19a()

# b)
q19b <- function(){
  if(runif(1) < .5) return(runif(1))
  return((sqrt(runif(1))))
}
q19b()

# c)

# Questão 22 ####

# Questão 23 ####

# Questão 24 ####