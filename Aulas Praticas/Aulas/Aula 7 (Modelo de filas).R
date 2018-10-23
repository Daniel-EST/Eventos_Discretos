#Criando um programa para o modelo de filas apresentado em sala

#Definindo algumas variveis;

#t = tempo; Nc = Número de chegadas ate o tempo t; Ns = Número de saidas ate o tempo t
#n = Número de clinetes no sistema no tempo t (SS)
#tc = tempo da chegada do proximo cliente apos t
#ts = tempo de termino do servi?o do cliente sendo atendido no tempo t

lambda2 <- 1; lambda <- 2; Tempo <- 10

fila2 <- function(Tempo,lambda,lambda2){
  t <- 0; Nc <- 0; Ns <- 0; n <- 0; C <- NULL; S <- NULL
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
      return(list(Chegada=round(C,2),Saida=round(S,2)))
    }
  }
}

fila2(Tempo,lambda,lambda2)


# Fazendo grafico ---------------------------------------------------------

dados <- fila2(Tempo,lambda,lambda2)
dados
library(plotly)
y1 <- dados$Chegada; x <- 1:length(dados$Chegada); y2 <- dados$Saida
eixo_x <- list(
  title = "i-esimo cliente",
  titlefont = t
)
eixo_y <- list(
  title = "Tempo",
  titlefont = t)

p1 <- plot_ly(y=y1, x=x , type="scatter", mode="makers+lines",name="Tempo Chegada",line = list(color = 'rgb(255, 0, 0)', width = 3)) %>% 
  layout(xaxis = eixo_x, yaxis = eixo_y); p1
p1 <- add_trace(p1, y=~y2, x=~x , type="scatter", mode="makers+lines",name="Tempo Saida",line = list(color = 'rgb(0, 255, 0)', width = 3)); p1
