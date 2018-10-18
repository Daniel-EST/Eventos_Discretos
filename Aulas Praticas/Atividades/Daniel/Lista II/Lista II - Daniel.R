# Atividade II # 

# Funções #### 
expr <- function(lambda){
  u <- runif(1)
  return(-1*log(u)/lambda)
}

# Questão I #### 

# Simulando processo de poisson não homogêneo. 
poisproc <- function(lambda, s){
  t <- s
  lambda_t <- (3+4/t+1)
  t <- t + expr(lambda)
  while(runif(1) > lambda_t*1/7){
    t <- t + expr(lambda)
  }
  return(t)
}

############# DEPOIS DAQUI A MERDA ACONTECE ###############################

# Simulando processo de fila. 
series_queue <- function(lambda, lambda_1, lambda_2, Time){
  t <- 0; Na <- 0; Nd <- 0; n1 <- 0; n2 <- 0; A1 <- 0; A2 <- 0; D <- 0
  ta <- poisproc(lambda, 0) ; t1 <- Inf ; t2 <- Inf
  while(t <= Time){
    # CASO 1
    if(ta == min(ta, t1, t2)){
      t <- ta
      Na <- Na + 1
      n1 <- n1 + 1
      ta <- poisproc(lambda, t) 
      if(n1 == 1) t1 <- t + expr(lambda_1)
      A1[Na] <- t
    }
    # CASO 2
    if(t1 < ta & t1 <= t2){
      t <-  t1
      n1 <- n1 - 1 ; n2 <- n2 + 1
      if(n1 == 0){
        t1 <- Inf
      } else {
        t1 <- t + expr(lambda_1)
      }
      if(n2 == 1) t2 <- t + expr(lambda_2)
      A2[Na - n1] <- t
    }
    if(t2 < ta & t2 < t1){
      t <- t2
      Nd <- Nd + 1
      n2 <- n2 - 1
      if(n2 == 0) t2 <- Inf
      if(n2 > 0) t2 <- t + expr(lambda_2)
      D[Nd] <- t
    }
  }
  while(Nd!=Na){
    Nd <- Nd+1
    D[Nd] <- t+expr(3)
  }
  A1 <- A1[-Na] ; D <- D[-Nd]
  return(list(Chegada = A1, `Saída` = D, mean.perm = mean(D - A1)))
}

tm <- 0
for(i in 1:1000){
  tm[i] <- series_queue(3,1,3,100)$mean.perm
}
mean(tm)