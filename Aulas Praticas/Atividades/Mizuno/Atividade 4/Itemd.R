# Item d ------------------------------------------------------------------
e1 <- 0
for (i in 1:1000){
  e1[i] <- f(100)
}

e <- ant(1000)

hist(e1, col="red", ylab=" ", 
     xlab=expression(theta), main="Analisando a variancia dos dois metodos" )
hist(e, col="blue", add=T)
legend("topright", legend=c("Controle","Antagonicas"), col=c("red", 
                                                             "blue"), pt.cex=2, pch=15 )
             
                                                      