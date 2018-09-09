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