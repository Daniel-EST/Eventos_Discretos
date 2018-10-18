# Atividade II # 

# Funções #### 
expr <- function(lambda){
  u <- runif(1)
  return(-1*log(u)/lambda)
}

exprn <- function(n,lambda){
  exp <- NULL
  for(i in 1:n){
    exp[i] <- expr(lambda)
  }
  return(exp)
}

poisson <- function(lambda){
  x <- 0
  u <- runif(1)
  i <- 0; p <- exp(-1*lambda); f <-p
  while(u>=f){
    i <- i+1;p <- p*lambda/i;f <- f+p
  }
  return(i)
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
    Nd <- Nd + 1
    D[Nd] <- t + expr(lambda_2)
  }
  A1 <- A1[-Na] ; D <- D[-Nd]
  return(list(Chegada = A1, `Saída` = D, mean.perm = mean(D - A1)))
}

tm <- 0
for(i in 1:1000){
  tm[i] <- series_queue(7,1,3,100)$mean.perm
}
mean(tm)

# Questão 2 ####

lucro_dia <- function(lambda){
  n <- poisson(lambda)
  custo_dia <- exprn(n, 1/1000)
  return(11000 - sum(custo_dia))
}

lucro_ano <- function(lambda){
  custo_ano <- NULL
  for(i in 1:365){
    custo_ano[i] <- lucro_dia(lambda)
  }
  lucro_ano <- 25000 + sum(custo_ano)
  return(lucro_ano)
}

l_m <- 0
for(i in 1:100){
  l_m[i] <- lucro_ano(10)
}
mean(l_m)