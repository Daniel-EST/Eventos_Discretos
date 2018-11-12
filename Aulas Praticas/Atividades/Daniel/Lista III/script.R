# Funções auxiliares ####
require(plotly)
# Gera uma exponencial
expr <- function(lambda){
  u <- runif(1)
  return(-1*log(u)/lambda)
}

# Geram n exponcencias
exprn <- function(n,lambda){
  exp <- NULL
  for(i in 1:n){
    exp[i] <- expr(lambda)
  }
  return(exp)
}

# Atividade ####
# Questão 1
# a)
q1_a <- function(n){
  prob <- NULL
  for(i in 1:n) prob[i] <- ifelse(sum(1:5*exprn(5, 1)) >= 21.6, 1, 0)
  return(mean(prob))
}
q1_a(100)

# b)
q1_b <- function(n){
  media <- NULL ; prob <- NULL
  for(j in 1:n){
    for(i in 1:n){
      u <- runif(5)
      r1 <- ifelse(sum(1:5*(-log(u))) >= 21.6 , 1, 0)
      r2 <- ifelse(sum(1:5*(-log(1-u))) >= 21.6, 1, 0)
      media[i] <- mean(c(r1, r2))
   }
   prob[j] <- mean(media) 
  }
  return(mean(prob))
}
q1_b(100)

# c)
q1_c <- function(n){
  prob <- NULL ; y <- NULL ; z <- NULL
  for(j in 1:n){
    for(i in 1:n){
      y[i] <- ifelse(sum(1:5*(-log(runif(5)))) >= 21.6 , 1, 0)
      z[i] <- sum(1:5*(-log(runif(5))))
    }
    covariance <- cov(y, z)
    prob[j] <- mean(y)-(covariance/55)*(mean(z)-15)
  }
  return(mean(prob))
}
q1_c(100)

# d)
# Acredito que o boxplot é um bom gráfico para observar a dispersão das probabi-
# lidades e assim averiguar qual método diminuiu mais a variância.

antagonica <- NULL
for(i in 1:100){
  antagonica[i] <- q1_b(100)
}

controle <- NULL
for(i in 1:100){
  controle[i] <- q1_c(100)
}

plot_ly(y = antagonica, 
        type = "box", 
        boxpoints = "all",
        jitter = 0.3, 
        pointpos = -1.8, name = 'Antagônica') %>%
  add_trace(y = controle, 
            type = "box", 
            boxpoints = "all", 
            jitter = 0.3,
            pointpos = -1.8,
            name = 'Controle')

# Observamos que o método das variáveis antagônicas diminuiu mais a variância do que
# o método de controle