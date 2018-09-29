#Vamos usar o metodo da Composição

#Primeiro note que;
#P(X=i)=.5*P1(X1=i)+.5*P2(X2=i), onde X1~Geo(.5) e X2~Geo(1/3)

#Gerando Gerometricas
geometrica <- function(p){
  u1 <- runif(1)
  x <- 1+as.integer(log(1-u1)/log(1-p))
  return(x)
}


funcao=function(){
  u <- runif(1)
  t <- as.integer(2*u)+1
  #Se t=1 ==> X1 e se t=2 ==> X2
  if (t==1){
    x <- geometrica(.5)
    return(x)
  }
  if (t==2){
    x <- geometrica(1/3)
    return(x)
  }
}

#Testando para ver se esta certo

x <- 0
for (i in 1:100000){
  x[i] <- funcao()
}
mean(x)
