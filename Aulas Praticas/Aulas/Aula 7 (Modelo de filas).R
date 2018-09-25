#Criando um programa para o modelo de filas apresentado em sala

#Definindo algumas variveis;

#t = tempo; Nc = Número de chegadas ate o tempo t; Ns = Número de saidas ate o tempo t
#n = Número de clinetes no sistema no tempo t (SS)
#tc = tempo da chegada do proximo cliente apos t
#ts = tempo de termino do serviço do cliente sendo atendido no tempo t


t <- 0; Nc <- 0; Ns <- 0; n <- 0; C <- NULL; S <- NULL
lambda2 <- 1; lambda <- 2; Tempo <- 5

T1 <- rexp(1,lambda); tc <- T1; ts <- Inf

while (TRUE){
  if (tc<=ts & tc<=Tempo){
    t <- tc; Nc <- Nc+1; n <- n+1
    y <- rexp(lambda); tc <- tc+t
    if (n==1){
      z <- rexp(lambda2); ts <- t+z
    }
    C <- c(C,t)
    }
  if (ts<tc & ts<=Tempo){
    t <- ts; n <- n-1; Ns <- Ns+1;
    if (n==0){
      ts <- Inf
      }
    else {
      z <- rexp(1,lambda2); ts <- t+z
      }
    S <- c(S,t)
    }
  if (min(tc,ts)>Tempo & n>0){
    t <- ts; n <- n-1; Ns <- Ns+1
    if (n==0){
      z <- rexp(1,lambda2); ts <- t+z
      }
    S <- c(S,t)
  }
  if (min(tc,ts)>Tempo & n==0){
    Tp <- max(0,t-Tempo)
    break
  }
}




