# Item d ------------------------------------------------------------------
e1 <- 0
for (i in 1:1000){
  e1[i] <- cont(100)
}

e <- ant(1000)

#Controle esta de vermelho e antagocnicas esta de azul
hist(e1, col="red",add=TRUE)
hist(e, col="blue", ylab=" ",xlab=expression(theta), main="Analisando a variancia dos dois metodos")
legend("topright", legend=c("Controle","Antagonicas"), col=c("red","blue"), pt.cex=2, pch=15 )
