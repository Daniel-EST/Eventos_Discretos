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