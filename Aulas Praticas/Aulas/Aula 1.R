#Estimando pi

pi=function(n){
  g=c(1:n)
  u1=runif(n,0,1)
  u2=runif(n,0,1)
  g[u1**2+u2**2<=1]=1
  g[u1**2+u2**2>1]=0
  4*mean(g)
}
pi(100000)

#Exercicio 12

f=function(n){
  N=c(1:n)
  for (i in 1:n){
    s=0
    j=0
    while (s<1){
      u1=runif(1,0,1)
      s=s+u1
      j=j+1
    }
    N[i]=j
  }
  print(mean(N))
}

f(1000)

#Exercicio 13

g=function(n){
  N=c(1:n)
  for (i in 1:n){
    s=1
    j=0
    while (s>=exp(-3)){
      u1=runif(1,0,1)
      s=s*u1
      j=j+1
    }
    N[i]=j-1
  }
  print(mean(N))
}
 
g(1000)
